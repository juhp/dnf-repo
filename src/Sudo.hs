module Sudo (
  doSudo,
  prompt_,
  yesno
  ) where

import Control.Monad
import Data.List.Extra (lower)
import SimpleCmd (cmdN, sudo_)

-- FIXME make this silent (simple-cmd-0.2.7) unless debug
doSudo :: Bool -> Bool -> String -> [String] -> IO ()
doSudo dryrun debug c args = do
  if dryrun
    then cmdN c args
    else do
    when debug $ cmdN c args
    sudo_ c args

prompt :: String -> IO String
prompt desc = do
  putStr $ desc ++ ": "
  getLine

prompt_ :: String -> IO ()
prompt_ desc = do
  void $ prompt desc

yesno :: String -> IO Bool
yesno desc = do
  inp <- prompt $ desc ++ "? [y/n]"
  case lower inp of
    "y" -> return True
    "yes" -> return True
    "n" -> return False
    "no" -> return False
    _ ->  yesno desc
