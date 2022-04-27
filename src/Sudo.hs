module Sudo (doSudo) where

import SimpleCmd (cmdN, sudo_)

doSudo :: Bool -> String -> [String] -> IO ()
doSudo dryrun = if dryrun then cmdN else sudo_
