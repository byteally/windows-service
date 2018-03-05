{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Internal where


import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import           Language.C.Inline.Context (ctxTypesTable)

import           Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as TH
import Foreign.Ptr
import Data.Word
import Data.Int

type DWORD32 = Word32
type LPVOID = Ptr ()
type LPDWORD = Ptr DWORD
type LPSTR = Ptr C.CChar
type LPTSTR = Ptr TCHAR
type DWORD = Word32
type LONG = Int32
type FLOAT = Float
type INT = Int32
type UINT = Word32
type USHORT = Word16
type TCHAR = C.CWchar


_SVC_ERROR :: DWORD
_SVC_ERROR = 0xC0020001

newtype SvcCurrentState = SvcCurrentState { getSVCState :: DWORD }
  deriving (Num, Eq, Show)

pattern SERVICE_CONTINUE_PENDING :: SvcCurrentState
pattern SERVICE_CONTINUE_PENDING = SvcCurrentState 0x00000005

pattern SERVICE_PAUSE_PENDING :: SvcCurrentState
pattern SERVICE_PAUSE_PENDING = SvcCurrentState 0x00000006

pattern SERVICE_PAUSED :: SvcCurrentState
pattern SERVICE_PAUSED = SvcCurrentState 0x00000007

pattern SERVICE_RUNNING :: SvcCurrentState
pattern SERVICE_RUNNING = SvcCurrentState 0x00000004

pattern SERVICE_START_PENDING :: SvcCurrentState
pattern SERVICE_START_PENDING = SvcCurrentState 0x00000002

pattern SERVICE_STOP_PENDING :: SvcCurrentState
pattern SERVICE_STOP_PENDING = SvcCurrentState 0x00000003

pattern SERVICE_STOPPED :: SvcCurrentState
pattern SERVICE_STOPPED = SvcCurrentState 0x00000001

{-# COMPLETE
  SERVICE_CONTINUE_PENDING, SERVICE_PAUSE_PENDING, SERVICE_PAUSED, SERVICE_RUNNING, SERVICE_START_PENDING, SERVICE_STOP_PENDING, SERVICE_STOPPED #-}

pattern NO_ERROR = SvcExitCode 0

newtype SvcExitCode = SvcExitCode DWORD
                    deriving (Num, Eq, Show)

newtype SvcWaitHint = SvcWaitHint DWORD
                    deriving (Num, Eq, Show)

data ServiceReport = ServiceReport
  { reportCurrentState :: SvcCurrentState
  , reportExitCode :: SvcExitCode
  , reportWaitHint :: SvcWaitHint
  }

pattern ReportSvcStopped = ServiceReport SERVICE_STOPPED NO_ERROR 0
pattern ReportSvcRunning = ServiceReport SERVICE_RUNNING NO_ERROR 0
pattern ReportSvcStartPending = ServiceReport SERVICE_START_PENDING NO_ERROR 3000
pattern ReportSvcStopPending = ServiceReport SERVICE_STOP_PENDING NO_ERROR 4000

pattern SERVICE_CONTROL_STOP :: DWORD
pattern SERVICE_CONTROL_STOP = 0x00000001

pattern SERVICE_CONTROL_PAUSE :: DWORD
pattern SERVICE_CONTROL_PAUSE = 0x00000002

pattern SERVICE_CONTROL_CONTINUE :: DWORD
pattern SERVICE_CONTROL_CONTINUE = 0x00000003

pattern SERVICE_CONTROL_INTERROGATE :: DWORD
pattern SERVICE_CONTROL_INTERROGATE = 0x00000004

data SvcCtrlMsg = SvcCtrlMsg


mkStdFunPtr :: TypeQ -> ExpQ
mkStdFunPtr hsTy = do
  ffiImportName <- uniqueFfiImportName
  dec <- forImpD StdCall Safe "wrapper" ffiImportName [t| $(hsTy) -> IO (FunPtr $(hsTy)) |]
  TH.addTopDecls [dec]
  varE ffiImportName
  where
    uniqueFfiImportName :: TH.Q TH.Name
    uniqueFfiImportName = TH.newName . show =<< TH.newName "inline_c_ffi"

data SvcStatusHandle
data SvcStatus
data Handle

winSvcCtx :: C.Context
winSvcCtx = C.baseCtx <> C.bsCtx <> C.funCtx <> C.fptrCtx <> winSvcCtx'
  where
    winSvcCtx' = mempty
      { ctxTypesTable = winSvcTypesTable
      }

winSvcTypesTable :: Map.Map C.TypeSpecifier TypeQ
winSvcTypesTable = Map.fromList
  [ (C.TypeName "SERVICE_STATUS", [t| SvcStatus |])
  , (C.TypeName "SERVICE_STATUS_HANDLE", [t| Ptr SvcStatusHandle |])
  , (C.TypeName "HANDLE", [t| Ptr Handle |])
  , (C.TypeName "DWORD", [t| DWORD |])
  , (C.TypeName "LPTSTR", [t| LPTSTR |])
  , (C.TypeName "LPVOID", [t| LPVOID |])
  ]
      
