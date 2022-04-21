{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)

import Paths_dnf_tool (getDataFileName, version)
import YumRepoFile

-- FIXME --testing
-- FIXME --disable-{testing,modular,others}
main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF repo wrapper tool"
    "see https://github.com/juhp/dnf-tool#readme" $
    runMain
    <$> switchWith 'c' "add-copr" "Create repo file for copr repo"
    <*> strArg "REPO"
    <*> some (strArg "ARGS")

coprRepoTemplate :: FilePath
coprRepoTemplate =
  "_copr:copr.fedorainfracloud.org:OWNER:REPO.repo"

-- FIXME support other coprs
-- FIXME delete created repo file if copr doesn't exist
runMain :: Bool -> String -> [String] -> IO ()
runMain createcopr repo args = do
  withCurrentDirectory "/etc/yum.repos.d" $ do
    repofiles <-
      if createcopr
      then addRepo
      else filter (replace "/" ":" repo `isInfixOf`) <$>
           filesWithExtension "." "repo"
    case repofiles of
      [] -> error' $ "repo file not found for " ++ repo
      [repofile] -> runRepo repofile
      repos -> error' $ show (length repos) ++ " repo files found: " ++ unwords repos
      where
        runRepo :: FilePath -> IO ()
        runRepo repofile = do
          name <- readRepoName repofile
          let repoargs = ["--enablerepo", name]
          sudo_ "dnf" $ repoargs ++ args

        addRepo :: IO [FilePath]
        addRepo = do
          case stripInfix "/" repo of
            Nothing -> error' $ "invalid copr: " ++ repo
            Just (copr_owner,copr_repo) -> do
              template <- getDataFileName coprRepoTemplate
              repodef <- cmd "sed" ["-e", "s/@COPR_OWNER@/" ++ copr_owner ++ "/g", "-e", "s/@COPR_REPO@/" ++ copr_repo ++ "/g", template]
              let repofile = replace "OWNER" copr_owner $
                             replace "REPO" copr_repo coprRepoTemplate
              exists <- doesFileExist repofile
              if exists
                then error' $ "repo already defined: " ++ repofile
                else putStrLn $ "Setting up copr repo " ++ repo
              withTempDir $ \ tmpdir -> do
                let tmpfile = tmpdir </> repofile
                writeFile tmpfile repodef
                sudo_ "cp" [tmpfile, repofile]
                return [repofile]

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif
