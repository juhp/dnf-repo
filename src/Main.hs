{-# LANGUAGE CPP, LambdaCase #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Bifunctor (bimap)
import Data.List.Extra
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Extra (withTempDir)
import System.Time.Extra (sleep)

import Paths_dnf_repo (getDataFileName, version)
import ExpireRepos
import Sudo
import YumRepoFile

main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF wrapper repo tool"
    "see https://github.com/juhp/dnf-repo#readme" $
    runMain
    <$> switchWith 'n' "dryrun" "Dry run"
    <*> switchWith 'q' "quiet" "Suppress necessary output"
    <*> switchWith 'D' "debug" "Debug output"
    <*> switchWith 'l' "list" "List all repos"
    <*> switchWith 's' "save" "Save the repo enable/disable state"
    <*> optional (flagWith' True 'w' "weak-deps" "Use weak dependencies" <|>
                  flagWith' False 'W' "no-weak-deps" "Disable weak dependencies")
    <*> switchLongWith "exact" "Match repo names exactly"
    <*> many modeOpt
    <*> many (strArg "DNFARGS")
  where
    repoOptionWith =
      (fmap . fmap . fmap . fmap) cleanupReponame . strOptionWith
      where
        cleanupReponame =
          dropWhileEnd (== ':') . replace "/" ":" . dropWhile (== '/')

    modeOpt =
      DisableRepo <$> repoOptionWith 'd' "disable" "REPOPAT" "Disable repos" <|>
      EnableRepo <$> repoOptionWith 'e' "enable" "REPOPAT" "Enable repos" <|>
      ExpireRepo <$> repoOptionWith 'x' "expire" "REPOPAT" "Expire repo cache" <|>
      flagWith' ClearExpires 'X' "clear-expires" "Undo cache expirations" <|>
      DeleteRepo <$> repoOptionWith 'E' "delete-repofile" "REPOPAT" "Remove unwanted .repo file" <|>
      flagWith' (Specific EnableTesting) 't' "enable-testing" "Enable testing repos" <|>
      flagWith' (Specific DisableTesting) 'T' "disable-testing" "Disable testing repos" <|>
      flagWith' (Specific EnableModular) 'm' "enable-modular" "Enable modular repos" <|>
      flagWith' (Specific DisableModular) 'M' "disable-modular" "Disable modular repos" <|>
      flagLongWith' (Specific EnableDebuginfo) "enable-debuginfo" "Enable debuginfo repos" <|>
      flagLongWith' (Specific DisableDebuginfo) "disable-debuginfo" "Disable debuginfo repos" <|>
      flagLongWith' (Specific EnableSource) "enable-source" "Enable source repos" <|>
      flagLongWith' (Specific DisableSource) "disable-source" "Disable source repos" <|>
      AddCopr <$> repoOptionWith 'c' "add-copr" "COPR" "Create repo file for copr repo" <|>
      AddKoji <$> repoOptionWith 'k' "add-koji" "REPO" "Create repo file for koji repo (f37-build, rawhide, epel9-build, etc)" <|>
      RepoURL <$> strOptionWith 'u' "repourl" "URL" "Use temporary repo from a baseurl"

coprRepoTemplate :: FilePath
coprRepoTemplate = "copr.fedorainfracloud.orgCOLONOWNERCOLONREPO.repo"

kojiRepoTemplate :: FilePath
kojiRepoTemplate = "koji-REPO.repo"

-- FIXME both enabling and disabled at the same time
-- FIXME --enable-all-coprs (for updating etc)
-- FIXME confirm repos if many
-- FIXME --disable-non-cores (modular,testing,cisco, etc)
runMain :: Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool -> Bool -> [Mode]
        -> [String] -> IO ()
runMain dryrun quiet debug listrepos save mweakdeps exact modes args = do
  hSetBuffering stdout NoBuffering
  withCurrentDirectory "/etc/yum.repos.d" $ do
    forM_ modes $
      \case
        AddCopr copr -> addCoprRepo dryrun debug copr
        AddKoji repo -> addKojiRepo dryrun debug repo
        _ -> return ()
    repofiles <- filesWithExtension "." "repo"
    -- when debug $ print repofiles
    nameStates <- sort <$> concatMapM readRepos repofiles
    let actions = selectRepo exact nameStates modes
        moreoutput = not (null args) || null actions || listrepos
    unless (null actions || quiet) $ do
      mapM_ (printAction save) actions
      when moreoutput $
        warning ""
    outputs <-
      forM actions $
      \case
        Expire repo -> do
          expireRepo dryrun debug repo
          return True
        UnExpire -> do
          clearExpired dryrun debug
          return True
        Delete repofile True -> do
          deleteRepo dryrun debug repofile
          return True
        _ -> return False
    when (or outputs && (save || moreoutput) && not quiet) $
      warning ""
    when save $ do
      if null actions
        then putStrLn "no changes to save"
        else do
        prompt_ "Press Enter to save repo enabled state"
        doSudo dryrun debug "dnf" $
          "config-manager" : concatMap saveRepo actions
      putStrLn ""
    if null args
      then
      when (null actions || listrepos) $
      listRepos $ map (updateState actions) nameStates
      else do
      sleep 1
      let repoargs = concatMap changeRepo actions
          weakdeps = maybe [] (\w -> ["--setopt=install_weak_deps=" ++ show w]) mweakdeps
          quietopt = if quiet then ("-q" :) else id
        in doSudo dryrun debug "dnf" $ quietopt repoargs ++ weakdeps ++ args

-- FIXME pull non-fedora copr repo file
-- FIXME delete created copr repo file if repo doesn't exist
addCoprRepo :: Bool -> Bool -> String -> IO ()
addCoprRepo dryrun debug repo = do
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
        putStrLn ""

-- FIXME check url exists!
addKojiRepo :: Bool -> Bool -> String -> IO ()
addKojiRepo dryrun debug repo = do
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
    putStrLn ""

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

deleteRepo :: Bool -> Bool -> FilePath -> IO ()
deleteRepo dryrun debug repofile = do
  mowned <- cmdMaybe "rpm" ["-qf", "/etc/yum.repos.d" </> repofile]
  case mowned of
    Just owner -> warning $ repofile +-+ "owned by" +-+ owner
    Nothing -> do
      ok <- yesno $ "Remove " ++ takeFileName repofile
      when ok $ do
        doSudo dryrun debug "rm" [repofile]

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif
