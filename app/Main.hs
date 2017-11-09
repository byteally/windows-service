module Main where

import WindowsService
import System.Environment
import Control.Monad

-- 1072 The specified service has been marked for deletion.
-- 1060 Service does not exist
-- 
main :: IO ()
main = do
  args <- getArgs
  case ("-i" `elem` args, "-u" `elem` args) of
    (True, _) -> svcInstall
    (False, True) -> svcUnInstall
    (False, False) -> svcStart (forever $ pure ())