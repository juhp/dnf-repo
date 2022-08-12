{-# LANGUAGE CPP, LambdaCase #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Bifunctor (bimap)
import Data.List.Extra
import Data.Maybe (mapMaybe)
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Extra (withTempDir)
import System.Time.Extra (sleep)

import Paths_dnf_repo (getDataFileName, version)
import ExpireRepos (expireRepos)
import Sudo
import YumRepoFile

main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF wrapper repo tool"
    "see https://github.com/juhp/dnf-repo#readme" $
    runMain
    <$> switchWith 'n' "dryrun" "Dry run"
    <*> switchWith 'D' "debug" "Debug output"
    <*> switchLongWith "exact" "Match repo names exactly"
    <*> switchWith 's' "save" "Save the repo enable/disable state"
    <*> many modeOpt
    <*> many (strArg "DNFARGS")
  where
    modeOpt =
      AddCopr . replace "/" ":" <$> strOptionWith 'c' "add-copr" "COPR" "Create repo file for copr repo" <|>
      AddKoji <$> strOptionWith 'k' "add-koji" "REPO" "Create repo file for koji repo" <|>
      DisableRepo <$> strOptionWith 'd' "disable" "REPOPAT" "Disable repos" <|>
      EnableRepo <$> strOptionWith 'e' "enable" "REPOPAT" "Enable repos" <|>
      ExpireRepo <$> strOptionWith 'x' "expire" "REPOPAT" "Expire repo cache" <|>
      DeleteRepo <$> strOptionWith 'E' "delete-repofile" "REPOPAT" "Remove unwanted .repo file" <|>
      flagWith' (Specific EnableTesting) 't' "enable-testing" "Enable testing repos" <|>
      flagWith' (Specific DisableTesting) 'T' "disable-testing" "Disable testing repos" <|>
      flagWith' (Specific EnableModular) 'm' "enable-modular" "Enable modular repos" <|>
      flagWith' (Specific DisableModular) 'M' "disable-modular" "Disable modular repos" <|>
      flagLongWith' (Specific EnableDebuginfo) "enable-debuginfo" "Enable debuginfo repos" <|>
      flagLongWith' (Specific DisableDebuginfo) "disable-debuginfo" "Disable debuginfo repos" <|>
      flagLongWith' (Specific EnableSource) "enable-source" "Enable source repos" <|>
      flagLongWith' (Specific DisableSource) "disable-source" "Disable source repos"

coprRepoTemplate :: FilePath
coprRepoTemplate = "copr.fedorainfracloud.orgCOLONOWNERCOLONREPO.repo"

kojiRepoTemplate :: FilePath
kojiRepoTemplate = "koji-REPO.repo"

-- FIXME both enabling and disabled at the same time
-- FIXME --enable-all-coprs (for updating etc)
-- FIXME confirm repos if many
-- FIXME --disable-non-cores (modular,testing,cisco, etc)
runMain :: Bool -> Bool -> Bool -> Bool -> [Mode] -> [String] -> IO ()
runMain dryrun debug exact save modes args = do
  hSetBuffering stdout NoBuffering
  withCurrentDirectory "/etc/yum.repos.d" $ do
    forM_ modes $
      \case
        AddCopr copr -> addCoprRepo copr
        AddKoji repo -> addKojiRepo repo
        _ -> return ()
    repofiles <- filesWithExtension "." "repo"
--    when debug $ print repofiles
    nameStates <- sort <$> concatMapM readRepos repofiles
    let repoActs = selectRepo exact nameStates modes
    unless (null repoActs) $ do
      mapM_ print repoActs
      putStrLn ""
    forM_ modes $
      \case
        ExpireRepo _ -> do
          putStrLn ""
          expireRepos dryrun debug $ mapMaybe expiring repoActs
        DeleteRepo _ ->
          mapM_ deleteRepos $ mapMaybe deleting repoActs
        _ -> return ()
    when save $
      if null repoActs
        then putStrLn "no changes to save\n"
        else do
        prompt_ "Press Enter to save repo enabled state"
        doSudo dryrun debug "dnf" $
          "config-manager" :
          concatMap saveRepo repoActs
    if null args
      then do
      listRepos $ map (updateState repoActs) nameStates
      else do
      sleep 1
      putStrLn ""
      let repoargs = concatMap changeRepo repoActs
        in doSudo dryrun debug "dnf" $ repoargs ++ args
    where
      -- FIXME pull non-fedora copr repo file
      -- FIXME delete created copr repo file if repo doesn't exist
      addCoprRepo :: String -> IO ()
      addCoprRepo repo = do
        case stripInfix ":" repo of
          Nothing -> error' $ "invalid copr: " ++ repo
          Just (copr_owner,copr_repo) -> do
            template <- getDataFileName coprRepoTemplate
            repodef <- cmd "sed" ["-e", "s/@COPR_OWNER@/" ++ copr_owner ++ "/g", "-e", "s/@COPR_REPO@/" ++ copr_repo ++ "/g", template]
            let repofile = ("_copr:" ++) $
                           replace "COLON" ":" $
                           replace "OWNER" copr_owner $
                           replace "REPO" copr_repo coprRepoTemplate
            exists <- doesFileExist repofile
            if exists
              then error' $ "repo already defined: " ++ repofile
              else putStrLn $ "Setting up copr repo " ++ repo
            withTempDir $ \ tmpdir -> do
              let tmpfile = tmpdir </> repofile
              unless dryrun $ writeFile tmpfile repodef
              doSudo dryrun debug "cp" [tmpfile, repofile]

      addKojiRepo :: String -> IO ()
      addKojiRepo repo = do
        template <- getDataFileName kojiRepoTemplate
        repodef <- cmd "sed" ["-e", "s/@REPO@/" ++ repo ++ "/g", template]
        let repofile = replace "REPO" repo kojiRepoTemplate
        exists <- doesFileExist repofile
        if exists
          then error' $ "repo already defined: " ++ repofile
          else putStrLn $ "Setting up koji repo " ++ repo
        withTempDir $ \ tmpdir -> do
          let tmpfile = tmpdir </> repofile
          unless dryrun $ writeFile tmpfile repodef
          doSudo dryrun debug "cp" [tmpfile, repofile]

      listRepos :: [RepoState] -> IO ()
      listRepos repoStates = do
        let (on,off) =
              -- can't this be simplified?
              bimap (map fst) (map fst) $ partition (fst . snd) repoStates
        putStrLn "Enabled:"
        mapM_ putStrLn on
        putStrLn ""
        putStrLn "Disabled:"
        mapM_ putStrLn off

      deleteRepos :: FilePath -> IO ()
      deleteRepos repofile = do
        mowned <- cmdMaybe "rpm" ["-qf", "/etc/yum.repos.d" </> repofile]
        case mowned of
          Just owner -> error' $ repofile +-+ "owned by" +-+ owner
          Nothing -> do
            ok <- yesno $ "Remove " ++ takeFileName repofile
            when ok $ doSudo dryrun debug "rm" [repofile]

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif

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
