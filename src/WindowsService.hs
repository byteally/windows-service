{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module WindowsService where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
--import qualified Language.C.Inline.Cpp as CPP
import           Data.Monoid ((<>))
import Data.String
import Data.Word
import Data.Coerce
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Internal
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import System.Environment
import Control.Concurrent.Async


C.context winSvcCtx

  
C.include "<stdio.h>"
C.include "<windows.h>"
C.include "<tchar.h>"
C.include "<strsafe.h>"

svcName :: ByteString
svcName = fromString "HSSVVNAME"

data SvcState = SvcState
  { svcStatusHandle :: ForeignPtr SvcStatusHandle
  , svcStatus :: ForeignPtr SvcStatus
  , svcStopEventHandle :: ForeignPtr Handle
  } deriving (Show, Eq)
  
svcState :: IORef SvcState
svcState = unsafePerformIO $ do
  newIORef =<< (SvcState
                 <$> (newForeignPtr_ nullPtr)
                 <*> (newForeignPtr_ nullPtr)
                 <*> (newForeignPtr_ nullPtr)
               )
{-# NOINLINE svcState #-}
  

svcInstall :: IO ()
svcInstall = do
  svcBin <- fromString <$> getExecutablePath
  [C.block| void {
  SC_HANDLE schSCManager;
  SC_HANDLE schService;
  TCHAR szPath[MAX_PATH];

  schSCManager = OpenSCManager( 
                      NULL,                    // local computer
                      NULL,                    // ServicesActive database 
                     SC_MANAGER_ALL_ACCESS);   // full access rights 
 
  if (NULL == schSCManager) 
  {
    printf("OpenSCManager failed (%d)\n", GetLastError());
    return;
  }

  schService = CreateService( 
        schSCManager,              // SCM database        
        $bs-ptr:svcName,           // name of service 
        $bs-ptr:svcName,           // service name to display 
        SERVICE_ALL_ACCESS,        // desired access 
        SERVICE_WIN32_OWN_PROCESS, // service type 
        SERVICE_DEMAND_START,      // start type 
        SERVICE_ERROR_NORMAL,      // error control type 
        $bs-ptr:svcBin,            // path to service's binary 
        NULL,                      // no load ordering group 
        NULL,                      // no tag identifier 
        NULL,                      // no dependencies 
        NULL,                      // LocalSystem account 
        NULL);                     // no password 
 
    if (schService == NULL) 
    {
        printf("CreateService failed (%d)\n", GetLastError()); 
        CloseServiceHandle(schSCManager);
        return;
    }
    else
    {
      printf("Service installed successfully\n");
    }

    CloseServiceHandle(schService); 
    CloseServiceHandle(schSCManager);
}|]

svcUnInstall :: IO ()
svcUnInstall = [C.block|void {
    SC_HANDLE schSCManager;
    SC_HANDLE schService;
    SERVICE_STATUS ssStatus; 

    // Get a handle to the SCM database.
 
    schSCManager = OpenSCManager( 
        NULL,                    // local computer
        NULL,                    // ServicesActive database 
        SC_MANAGER_ALL_ACCESS);  // full access rights 
 
    if (NULL == schSCManager) 
    {
        printf("OpenSCManager failed (%d)\n", GetLastError());
        return;
    }

    // Get a handle to the service.
   
    schService = OpenService( 
        schSCManager,       // SCM database 
        $bs-ptr:svcName,    // name of service 
        DELETE);            // need delete access 
 
    if (schService == NULL)
    { 
        printf("OpenService failed (%d)\n", GetLastError()); 
        CloseServiceHandle(schSCManager);
        return;
    }

    // Delete the service.
 
    if (! DeleteService(schService) ) 
    {
        printf("DeleteService failed (%d)\n", GetLastError()); 
    }
    else printf("Service deleted successfully\n"); 
 
    CloseServiceHandle(schService); 
    CloseServiceHandle(schSCManager);                   
}|]


stopSvc :: IO ()
stopSvc = do
  stopEventHPtr <- svcStopEventHandle <$> readIORef svcState
  [C.block| void {
    HANDLE ghSvcStopEvent = $fptr-ptr:(HANDLE stopEventHPtr);
    SetEvent(ghSvcStopEvent);
}|]

createSvcEvent :: IO Bool
createSvcEvent = do
  -- stopEventHPtr <- svcStopEventHandle <$> readIORef svcState
  stopEventHPtr <- newForeignPtr_ =<< [C.block| HANDLE {

    // Create an event. The control handler function, SvcCtrlHandler,
    // signals this event when it receives the stop control code.

    HANDLE ghSvcStopEvent = CreateEvent(
                         NULL,    // default security attributes
                         TRUE,    // manual reset event
                         FALSE,   // not signaled
                         NULL);   // no name
    return ghSvcStopEvent;
  }|]
  atomicModifyIORef' svcState (\st -> (st { svcStopEventHandle = stopEventHPtr
                                          }, ()))
  withForeignPtr stopEventHPtr $ \stopEventHPtr' -> case stopEventHPtr' == nullPtr of
    True -> reportSvcStatus ReportSvcStopped >> pure False
    False -> pure True

svcInit :: IO () -> IO ()
svcInit svc = do
  svcReportEvent (fromString "Inside SVC Init")
  isEvtCreated <- createSvcEvent
  when (not isEvtCreated) (reportSvcStatus ReportSvcStopped) 
  when isEvtCreated $ do
    stopEventHPtr <- svcStopEventHandle <$> readIORef svcState
    reportSvcStatus ReportSvcRunning
    svcAsync <- async svc
    [C.block| void {
      HANDLE ghSvcStopEvent = $fptr-ptr:(HANDLE stopEventHPtr);
      while(1)
      {
        // Check whether to stop the service.

        WaitForSingleObject(ghSvcStopEvent, INFINITE);
        break;
      }
    }|]
    svcReportEvent (fromString "Cancelling HS SVC")
    cancel svcAsync 
    reportSvcStatus ReportSvcStopped

svcStart :: IO () -> IO ()
svcStart svc = do
  svcMainPtr <- $(C.mkFunPtr [t|DWORD -> Ptr (LPTSTR) -> IO ()|]) $ \argc argv -> do
    svcReportEvent (fromString "Inside SVC Main")
    svcMain svc
  status <- [C.block| int {
    SERVICE_TABLE_ENTRY DispatchTable[] = 
    { 
        { $bs-ptr:svcName, (LPSERVICE_MAIN_FUNCTION) $(void (*svcMainPtr)(DWORD, LPTSTR *)) }, 
        { NULL, NULL } 
    }; 
 
    // This call returns when the service has stopped. 
    // The process should simply terminate when the call returns.

    if (!StartServiceCtrlDispatcher( DispatchTable )) 
    { 
      return 0;
    }
    return 1;
  }|]
  when (status == 0) $ do
    svcReportEvent (fromString "Unable to start service. StartServiceCtrlDispatcher failed")

getSvcCurrentState :: IO DWORD
getSvcCurrentState = do
  svcStatusPtr <- svcStatus <$> readIORef svcState
  [C.block| DWORD {
    SERVICE_STATUS* gSvcStatus = $fptr-ptr:(SERVICE_STATUS* svcStatusPtr);
    return gSvcStatus->dwCurrentState;
}|]
  
svcMain :: IO () -> IO ()
svcMain svc = do
  svcReportEvent (fromString "Really Inside SVC Main")
  svcCtrlHandlerPtr  <- $(C.mkFunPtr [t|DWORD -> IO ()|]) svcCtrlHandler
  svcStatusPtr <- newForeignPtr_ =<< [C.exp| SERVICE_STATUS* {(SERVICE_STATUS*) malloc (sizeof (SERVICE_STATUS))}|]
  gSvcStatusHandle <- newForeignPtr_ =<< [C.block| SERVICE_STATUS_HANDLE {
    SERVICE_STATUS* gSvcStatus = $fptr-ptr:(SERVICE_STATUS* svcStatusPtr);
    SERVICE_STATUS_HANDLE gSvcStatusHandle = RegisterServiceCtrlHandler( 
        $bs-ptr:svcName,
        $(void (*svcCtrlHandlerPtr) (DWORD)));

    if( !gSvcStatusHandle )
    { 
        return 0; 
    } 

    // These SERVICE_STATUS members remain as set here

    gSvcStatus->dwServiceType = SERVICE_WIN32_OWN_PROCESS; 
    gSvcStatus->dwServiceSpecificExitCode = 0;
    return gSvcStatusHandle;
  }|]
  atomicModifyIORef' svcState (\st -> (st { svcStatus = svcStatusPtr
                                          , svcStatusHandle = gSvcStatusHandle
                                          }, ()))
  svcReportEvent (fromString ("After Register Service" ++ (show svcStatusPtr)))
  withForeignPtr gSvcStatusHandle $ \gSvcStatusHandle' -> case gSvcStatusHandle' == nullPtr of
    True -> svcReportEvent (fromString "RegisterServiceCtrlHandler")
    False -> do
      reportSvcStatus ReportSvcStartPending
      svcInit svc


svcCtrlHandler :: DWORD -> IO ()
svcCtrlHandler SERVICE_CONTROL_STOP = do
  svcReportEvent (fromString "Inside SVC Ctrl Handler")
  reportSvcStatus ReportSvcStopPending
  stopSvc
  currState <- getSvcCurrentState
  reportSvcStatus (ServiceReport (coerce currState) NO_ERROR 0)
svcCtrlHandler dw = do
  svcReportEvent (fromString ("svcReportEvent: Ignoring " ++ (show dw)))

reportSvcStatus :: ServiceReport -> IO ()
reportSvcStatus report = do
  svcStatusPtr <- svcStatus <$> readIORef svcState
  svcStatusHandle <- (svcStatusHandle) <$> readIORef svcState
  [C.block|void {
    DWORD dwCurrentState = $(DWORD currState);
    DWORD dwWin32ExitCode = $(DWORD exitCode);
    DWORD dwWaitHint = $(DWORD waitHint);
    SERVICE_STATUS* gSvcStatus = $fptr-ptr:(SERVICE_STATUS* svcStatusPtr);
    SERVICE_STATUS_HANDLE gSvcStatusHandle = $fptr-ptr:(SERVICE_STATUS_HANDLE svcStatusHandle);
    static DWORD dwCheckPoint = 1;

    // Fill in the SERVICE_STATUS structure.

    gSvcStatus->dwCurrentState =  dwCurrentState;
    gSvcStatus->dwWin32ExitCode = dwWin32ExitCode;
    gSvcStatus->dwWaitHint = dwWaitHint;

    if (dwCurrentState == SERVICE_START_PENDING)
        gSvcStatus->dwControlsAccepted = 0;
    else gSvcStatus->dwControlsAccepted = SERVICE_ACCEPT_STOP;

    if ( (dwCurrentState == SERVICE_RUNNING) ||
           (dwCurrentState == SERVICE_STOPPED) )
        gSvcStatus->dwCheckPoint = 0;
    else gSvcStatus->dwCheckPoint = dwCheckPoint++;

    // Report the status of the service to the SCM.
    SetServiceStatus( gSvcStatusHandle, gSvcStatus );

}|]
  where
   currState, exitCode, waitHint :: DWORD
   currState = coerce (reportCurrentState report) 
   exitCode = coerce (reportExitCode report)
   waitHint = coerce (reportWaitHint report)


svcReportEvent :: ByteString -> IO ()
svcReportEvent evt = [C.block| void {
    HANDLE hEventSource;
    LPCTSTR lpszStrings[2];
    TCHAR Buffer[80];

    hEventSource = RegisterEventSource(NULL, $bs-ptr:svcName);

    if( NULL != hEventSource )
    {
        StringCchPrintf(Buffer, 80, TEXT("%s failed with %d"), $bs-ptr:evt, GetLastError());

        lpszStrings[0] = $bs-ptr:svcName;
        lpszStrings[1] = Buffer;

        ReportEvent(hEventSource,        // event log handle
                    EVENTLOG_ERROR_TYPE, // event type
                    0,                   // event category
                    $(DWORD _SVC_ERROR), // event identifier
                    NULL,                // no security identifier
                    2,                   // size of lpszStrings array
                    0,                   // no binary data
                    lpszStrings,         // array of strings
                    NULL);               // no binary data

        DeregisterEventSource(hEventSource);
    }
}|]

