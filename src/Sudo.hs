module Sudo (doSudo) where

import Control.Monad (when)
import SimpleCmd (cmdN, sudo_)

-- FIXME make this silent (simple-cmd-0.2.7) unless debug
doSudo :: Bool -> Bool -> String -> [String] -> IO ()
doSudo dryrun debug c args = do
  if dryrun
    then cmdN c args
    else do
    when debug $ cmdN c args
    sudo_ c args
  putStrLn ""
