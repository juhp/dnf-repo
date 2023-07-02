module Sudo (
  doSudo
  ) where

import Control.Monad
import SimpleCmd (cmdN, sudo_)

-- FIXME make this silent (simple-cmd-0.2.7) unless debug
doSudo :: Bool -> Bool -> String -> [String] -> IO ()
doSudo dryrun debug c args = do
  when (dryrun || debug) $ do
    cmdN c args
  unless dryrun $
    sudo_ c args
