module Sudo (doSudo) where

import SimpleCmd (cmdN, sudo_)

-- FIXME make this silent (simple-cmd-0.2.7) unless debug
doSudo :: Bool -> String -> [String] -> IO ()
doSudo dryrun c args = do
  (if dryrun then cmdN else sudo_) c args
  putStrLn ""
