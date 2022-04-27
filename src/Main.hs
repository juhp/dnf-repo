{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad
import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)
import System.Time.Extra (sleep)

import Paths_dnf_repo (getDataFileName, version)
import YumRepoFile

data Mode = Copr | Enable | Disable | List
  deriving Eq

-- FIXME --save-enabled
main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF wrapper repo tool"
    "see https://github.com/juhp/dnf-repo#readme" $
    runMain
    <$> switchWith 'n' "dryrun" "Dry run"
    <*> modeOpt
    <*> optional testingOpt
    <*> strArg "REPO"
    <*> many (strArg "ARGS")
  where
    modeOpt =
      flagWith' Copr 'c' "add-copr" "Create repo file for copr repo" <|>
      flagWith' List 'l' "list" "List repos" <|>
      flagWith Enable Disable 'd' "disable" "Disable repos"

    testingOpt =
      flagWith' EnableTesting 't' "enable-testing" "Include testing repos" <|>
      flagWith' DisableTesting 'T' "disable-testing" "Exclude testing repos"

coprRepoTemplate :: FilePath
coprRepoTemplate =
  "_copr:copr.fedorainfracloud.org:OWNER:REPO.repo"

-- FIXME support other coprs
-- FIXME delete created repo file if copr doesn't exist
runMain :: Bool -> Mode -> Maybe Testing -> String -> [String]
        -> IO ()
runMain dryrun mode mtesting repo args = do
  withCurrentDirectory "/etc/yum.repos.d" $ do
    repofiles <- if mode == Copr
                 then addRepo
                 else sort . filter (replace "/" ":" repo `isInfixOf`) <$>
                      filesWithExtension "." "repo"
    if null repofiles
      then error' $ "no repo file found for " ++ repo
      else do
        names <- readRepoNames (mode == Disable) mtesting repofiles
        if mode == List
          then mapM_ putStrLn names
          else do
          when (null args) $
            error' "please give one or more dnf arguments"
          sleep 1
          putStrLn ""
          let repoargs =
                concatMap (\r -> [if mode == Disable then "--disablerepo" else "--enablerepo", r])
                names
          (if dryrun then cmdN else sudo_) "dnf" $ repoargs ++ args
    where
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
                unless dryrun $ writeFile tmpfile repodef
                unless dryrun $ sudo_ "cp" [tmpfile, repofile]
                return [repofile]

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif
