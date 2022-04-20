-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs

import qualified Paths_dnf_tool

-- FIXME add copr disabled
main :: IO ()
main = do
  simpleCmdArgs' (Just Paths_dnf_tool.version)
    "DNF wrapper tool"
    "see https://github.com/juhp/dnf-tool#readme" $
    runMain
    <$> optional repoOpt
    <*> some (strArg "ARGS")
  where
    repoOpt = strOptionWith 'c' "copr" "REPO" "Specify Copr repo"

-- FIXME support other coprs
runMain :: Maybe String -> [String] -> IO ()
runMain mcopr args = do
  let copr = case mcopr of
               Nothing -> []
               Just repo -> ["--enablerepo", "copr:copr.fedorainfracloud.org:" ++ replace "/" ":" repo]
  sudo_ "dnf" $ copr ++ args
