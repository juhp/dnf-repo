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
import ExpireRepos (expireRepos)
import YumRepoFile

data Mode = Copr | Enable | Disable | List | Expire
  deriving Eq

main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF wrapper repo tool"
    "see https://github.com/juhp/dnf-repo#readme" $
    runMain
    <$> switchWith 'n' "dryrun" "Dry run"
    <*> switchWith 's' "save" "Save enabled state"
    <*> modeOpt
    <*> optional testingOpt
    <*> optional modularOpt
    <*> strArg "REPO"
    <*> many (strArg "ARGS")
  where
    modeOpt =
      flagWith' Copr 'c' "add-copr" "Create repo file for copr repo" <|>
      flagWith' List 'l' "list" "List repos" <|>
      flagWith' Expire 'x' "expire" "Expire repo cache" <|>
      flagWith Enable Disable 'd' "disable" "Disable repos"

    testingOpt =
      flagWith' EnableTesting 't' "enable-testing" "Include testing repos" <|>
      flagWith' DisableTesting 'T' "disable-testing" "Exclude testing repos"

    modularOpt =
      flagWith' EnableModular 'm' "enable-modular" "Include modular repos" <|>
      flagWith' DisableModular 'M' "disable-modular" "Exclude modular repos"

coprRepoTemplate :: FilePath
coprRepoTemplate =
  "_copr:copr.fedorainfracloud.org:OWNER:REPO.repo"

-- FIXME both enabling and disabled at the same time
-- FIXME confirm if many repos
-- FIXME --disable-non-core (modular,testing,cisco, etc)
-- FIXME support non-fedora coprs
-- FIXME delete created copr repo file if repo doesn't exist
runMain :: Bool -> Bool -> Mode -> Maybe Testing -> Maybe Modular
        -> String -> [String] -> IO ()
runMain dryrun save mode mtesting mmodular repo args = do
  withCurrentDirectory "/etc/yum.repos.d" $ do
    repofiles <- if mode == Copr
                 then addRepo
                 else sort . filter (replace "/" ":" repo `isInfixOf`) <$>
                      filesWithExtension "." "repo"
    if null repofiles
      then error' $ "no repo file found for " ++ repo
      else do
        names <- readRepoNames (mode == Disable) mtesting mmodular repofiles
        if mode == List
          then mapM_ putStrLn names
          else do
          when (null args) $
            error' "please give one or more dnf arguments"
          sleep 1
          putStrLn ""
          when (mode == Expire) $
            unless dryrun $ expireRepos names
          let repoargs =
                concatMap (\r -> [if mode == Disable then "--disablerepo" else "--enablerepo", r]) names
            in doSudo "dnf" $ repoargs ++ args
        when save $ do
          putStr "Press Enter to save repo enabled state:"
          void getLine
          doSudo "dnf" $
            ["config-manager",
             if mode == Disable then "--set-disabled" else "--set-enabled"] ++
            names

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

        doSudo :: String -> [String] -> IO ()
        doSudo = if dryrun then cmdN else sudo_

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif
