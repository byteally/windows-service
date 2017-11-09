
#include <stdio.h>

#include <windows.h>

#include <tchar.h>

#include <strsafe.h>

void inline_c_WindowsService_0_9963f6550a83c934357110ab455a38ee9f47d90a(char * svcName_inline_c_0, char * svcName_inline_c_1, char * svcBin_inline_c_2) {

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
        svcName_inline_c_0,           // name of service 
        svcName_inline_c_1,           // service name to display 
        SERVICE_ALL_ACCESS,        // desired access 
        SERVICE_WIN32_OWN_PROCESS, // service type 
        SERVICE_DEMAND_START,      // start type 
        SERVICE_ERROR_NORMAL,      // error control type 
        svcBin_inline_c_2,            // path to service's binary 
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

}


void inline_c_WindowsService_1_10c1ef5464598779d53707dbf9df22de11437a8e(char * svcName_inline_c_0) {

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
        svcName_inline_c_0,    // name of service 
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

}


void inline_c_WindowsService_2_90f332b35ac9fef2ba1c32ab60af9a8b70072917(HANDLE stopEventHPtr_inline_c_0) {

    HANDLE ghSvcStopEvent = stopEventHPtr_inline_c_0;
    SetEvent(ghSvcStopEvent);

}


HANDLE inline_c_WindowsService_3_d8ec7a36b89d357597f022ab203d2b700449ac80() {


    // Create an event. The control handler function, SvcCtrlHandler,
    // signals this event when it receives the stop control code.

    HANDLE ghSvcStopEvent = CreateEvent(
                         NULL,    // default security attributes
                         TRUE,    // manual reset event
                         FALSE,   // not signaled
                         NULL);   // no name
    return ghSvcStopEvent;
  
}


void inline_c_WindowsService_4_ca6c09869378ccc7acd1017a405bcb5fb6dc1fd3(HANDLE stopEventHPtr_inline_c_0) {

      HANDLE ghSvcStopEvent = stopEventHPtr_inline_c_0;
      while(1)
      {
        // Check whether to stop the service.

        WaitForSingleObject(ghSvcStopEvent, INFINITE);
        break;
      }
    
}


int inline_c_WindowsService_5_4c94d7a1a1e7c4cb093935ea74a7ba4d214f6a4d(char * svcName_inline_c_0, void (* svcMainPtr_inline_c_1)(DWORD , LPTSTR *)) {

    SERVICE_TABLE_ENTRY DispatchTable[] = 
    { 
        { svcName_inline_c_0, (LPSERVICE_MAIN_FUNCTION) svcMainPtr_inline_c_1 }, 
        { NULL, NULL } 
    }; 
 
    // This call returns when the service has stopped. 
    // The process should simply terminate when the call returns.

    if (!StartServiceCtrlDispatcher( DispatchTable )) 
    { 
      return 0;
    }
    return 1;
  
}


DWORD inline_c_WindowsService_6_889a05317bab2c5f0da66481a182c4bd9f4ce3e7(SERVICE_STATUS * svcStatusPtr_inline_c_0) {

    SERVICE_STATUS* gSvcStatus = svcStatusPtr_inline_c_0;
    return gSvcStatus->dwCurrentState;

}


SERVICE_STATUS * inline_c_WindowsService_7_db856bb7f971ead30ab33bc98ceab5ccf41f87b0() {
return ((SERVICE_STATUS*) malloc (sizeof (SERVICE_STATUS)));
}


SERVICE_STATUS_HANDLE inline_c_WindowsService_8_257e01abdb158dff9fd3a2df1ee69260993ede8f(SERVICE_STATUS * svcStatusPtr_inline_c_0, char * svcName_inline_c_1, void (* svcCtrlHandlerPtr_inline_c_2)(DWORD )) {

    SERVICE_STATUS* gSvcStatus = svcStatusPtr_inline_c_0;
    SERVICE_STATUS_HANDLE gSvcStatusHandle = RegisterServiceCtrlHandler( 
        svcName_inline_c_1,
        svcCtrlHandlerPtr_inline_c_2);

    if( !gSvcStatusHandle )
    { 
        return 0; 
    } 

    // These SERVICE_STATUS members remain as set here

    gSvcStatus->dwServiceType = SERVICE_WIN32_OWN_PROCESS; 
    gSvcStatus->dwServiceSpecificExitCode = 0;
    return gSvcStatusHandle;
  
}


void inline_c_WindowsService_9_e63adbee53e1bfca2800f2bbeee07f5e398dc70a(DWORD currState_inline_c_0, DWORD exitCode_inline_c_1, DWORD waitHint_inline_c_2, SERVICE_STATUS * svcStatusPtr_inline_c_3, SERVICE_STATUS_HANDLE svcStatusHandle_inline_c_4) {

    DWORD dwCurrentState = currState_inline_c_0;
    DWORD dwWin32ExitCode = exitCode_inline_c_1;
    DWORD dwWaitHint = waitHint_inline_c_2;
    SERVICE_STATUS* gSvcStatus = svcStatusPtr_inline_c_3;
    SERVICE_STATUS_HANDLE gSvcStatusHandle = svcStatusHandle_inline_c_4;
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


}


void inline_c_WindowsService_10_2a3356b836f4ae9e6bf760098f377d0fd29b400f(char * svcName_inline_c_0, char * evt_inline_c_1, char * svcName_inline_c_2, DWORD _SVC_ERROR_inline_c_3) {

    HANDLE hEventSource;
    LPCTSTR lpszStrings[2];
    TCHAR Buffer[80];

    hEventSource = RegisterEventSource(NULL, svcName_inline_c_0);

    if( NULL != hEventSource )
    {
        StringCchPrintf(Buffer, 80, TEXT("%s failed with %d"), evt_inline_c_1, GetLastError());

        lpszStrings[0] = svcName_inline_c_2;
        lpszStrings[1] = Buffer;

        ReportEvent(hEventSource,        // event log handle
                    EVENTLOG_ERROR_TYPE, // event type
                    0,                   // event category
                    _SVC_ERROR_inline_c_3, // event identifier
                    NULL,                // no security identifier
                    2,                   // size of lpszStrings array
                    0,                   // no binary data
                    lpszStrings,         // array of strings
                    NULL);               // no binary data

        DeregisterEventSource(hEventSource);
    }

}

