module Main where

import WindowsService
import System.Environment
import Control.Monad
import Control.Concurrent
import Data.String

-- 1072 The specified service has been marked for deletion.
-- 1060 Service does not exist
-- 
main :: IO ()
main = do
  args <- getArgs
  let tstSvcName = (fromString "testHaskSvc")
      installArgs = defSvcInstallArg tstSvcName 
  case ("-i" `elem` args, "-u" `elem` args) of
    (True, _) -> svcInstall installArgs
    (False, True) -> svcUnInstall tstSvcName
    (False, False) -> svcStart tstSvcName $ forever $ do
      threadDelay 10000000
      svcReportEvent tstSvcName (fromString "Worker")
