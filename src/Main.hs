{-# LANGUAGE CPP, LambdaCase #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List.Extra
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Network.Curl (curlGetString, CurlCode(CurlOK))
import Network.HTTP.Directory (httpExists', (+/+))
import SimpleCmd (cmd, cmdLines, cmdMaybe, error', grep, warning, (+-+),
#if MIN_VERSION_simple_cmd(0,2,4)
                  filesWithExtension
#endif
                 )
import SimpleCmdArgs
import SimplePrompt (yesNo)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable,
                         withCurrentDirectory,
#if !MIN_VERSION_simple_cmd(0,2,4)
                         listDirectory
#endif
                        )
import System.Environment (lookupEnv)
import System.FilePath
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Extra (withTempDir)
import System.Time.Extra (sleep)

import Paths_dnf_repo (getDataFileName, version)
import ExpireRepos
import State
import Sudo
import YumRepoFile

main :: IO ()
main = do
  checkSudo
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
          dropWhileEnd (== '/') . dropWhile (== '/') . replace ":" "/"

    modeOpt =
      DisableRepo <$> repoOptionWith 'd' "disable" "REPOPAT" "Disable repos" <|>
      EnableRepo <$> repoOptionWith 'e' "enable" "REPOPAT" "Enable repos" <|>
      OnlyRepo <$> repoOptionWith 'o' "only" "REPOPAT" "Only use matching repos" <|>
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
      AddCopr
      <$> repoOptionWith 'c' "add-copr" "COPR" "Create repo file for copr repo"
      <*> optional (strOptionLongWith "osname" "OSNAME" "Specify OS Name to override (eg epel)")
      <*> optional (strOptionLongWith "releasever" "RELEASEVER" "Specify OS Release Version to override (eg rawhide)") <|>
      AddKoji <$> repoOptionWith 'k' "add-koji" "REPO" "Create repo file for koji repo (f40-build, rawhide, epel9-build, etc)" <|>
      RepoURL <$> strOptionWith 'u' "repourl" "URL" "Use temporary repo from a baseurl"

fedoraCopr :: String
fedoraCopr = "copr.fedorainfracloud.org"

kojiRepoTemplate :: FilePath
kojiRepoTemplate = "koji-REPO.repo"

yumReposD :: String
yumReposD = "/etc/yum.repos.d"

-- FIXME both enabling and disabled at the same time
-- FIXME --enable-all-coprs (for updating etc)
-- FIXME confirm repos if many
-- FIXME --disable-non-cores (modular,testing,cisco, etc)
runMain :: Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool -> Bool -> [Mode]
        -> [String] -> IO ()
runMain dryrun quiet debug listrepos save mweakdeps exact modes args = do
  hSetBuffering stdout NoBuffering
  unlessM (doesDirectoryExist yumReposD) $
    error' $ yumReposD +-+ "not found!"
  withCurrentDirectory yumReposD $ do
    forM_ modes $
      \case
        AddCopr copr mosname mrelease -> addCoprRepo dryrun debug mosname mrelease copr
        AddKoji repo -> addKojiRepo dryrun debug repo
        _ -> return ()
    repofiles <- filesWithExtension "./" "repo"
    when debug $
      print modes
    nameStates <- sort <$> concatMapM readRepos repofiles
    let actions = selectRepo exact nameStates modes
        moreoutput = not (null args) || null actions || listrepos
    when debug $
      print actions
    unless (null actions || quiet) $ do
      mapM_ putStrLn $ reduceOutput $ mapMaybe (printAction save) actions
      when moreoutput $
        warning ""
    outputs <-
      forM actions $
      \case
        Expire repo _ -> do
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
    when save $
      if null actions
        then putStrLn "no changes to save"
        else do
        let changes = concatMap saveRepo actions
        unless (null changes) $ do
          ok <- yesNo $ "Save changed repo" +-+ "enabled state" ++ ['s' | length changes > 1]
          when ok $ do
            mdnf3 <- findExecutable "dnf-3"
            case mdnf3 of
              -- FIXME cannot combine --disable and --enable
              Just dnf3 -> doSudo dryrun debug dnf3 $ "config-manager" : changes
              -- FIXME need to have repo files in changes
              Nothing -> doSudo dryrun debug "sed" $ "config-manager" : changes
    if null args
      then
      when (null actions || listrepos) $ do
      when save $ putStrLn ""
      listRepos $ map (updateState actions) nameStates
      else do
      sleep 1
      when save $ putStrLn ""
      mdnf <- do
        mdnf5 <- findExecutable "dnf5"
        case mdnf5 of
          Just dnf5 -> return $ Just dnf5
          Nothing -> findExecutable "dnf"
      case mdnf of
        Just dnf -> do
          when debug $ putStrLn dnf
          let repoargs = mapMaybe changeRepo actions
              weakdeps = maybe [] (\w -> ["--setopt=install_weak_deps=" ++ show w]) mweakdeps
              quietopt = if quiet then ("-q" :) else id
              cachedir = ["--setopt=cachedir=/var/cache/dnf" </> relver | relver <- maybeToList (maybeReleaseVer args)]
            in doSudo dryrun debug dnf $ quietopt repoargs ++ cachedir ++ weakdeps ++ map mungeArg args
        -- FIXME rpm-ostree install supports --enablerepo
        Nothing -> error' "rpm-ostree not supported"
  where
    mungeArg :: String -> String
    mungeArg "distrosync" = "distro-sync"
    mungeArg arg = arg

addCoprRepo :: Bool -> Bool -> Maybe String -> Maybe String -> String -> IO ()
addCoprRepo dryrun debug mosname mrelease repo = do
  let (server,owner,project) =
        case  splitOn "/" repo of
          [] -> error' "empty repo string"
          [_] -> error' $ "unqualified repo project:" +-+ repo
          [o,p] -> (fedoraCopr, o , p)
          [c,o,p] ->
            if c == "redhat"
            then ("copr.devel.redhat.com", o, p)
            else
              if '.' `elem` c
              then (c, o, p)
              else error' $ "unknown copr server:" +-+ repo
          _ -> error' $ "unknown copr:" +-+ repo
      repofile =
        "_copr:" ++ server ++ ':' : mungeGroupFile owner ++ ':' : project <.> "repo"
  whenM (doesFileExist repofile) $
    error' $ "repo already defined:" +-+ repofile
  osName <- maybe getRpmOSName return mosname
  osVersion <- maybe getRpmOsRelease return mrelease
  let repofileUrl = "https://" ++ server +/+ "coprs" +/+ mungeGroupUrl owner +/+ project +/+ "repo" +/+ osName ++ '-' : osVersion +/+ mungeGroupFile owner ++ '-' : project <.> "repo"
  (curlres,curlcontent) <- curlGetString repofileUrl []
  unless (curlres == CurlOK) $
    error' $ "downloading failed of" +-+ repofileUrl
  putStrLn $ "Setting up copr repo" +-+ repo
  withTempDir $ \ tmpdir -> do
    let tmpfile = tmpdir </> repofile
    unless dryrun $ writeFile tmpfile $
      maybe id (replace "$releasever") mrelease $
      replace "enabled=1" "enabled=0" curlcontent
    doSudo dryrun debug "cp" [tmpfile, repofile]
    putStrLn ""
  where
    mungeGroupFile ('@':own) = "group_" ++ own
    mungeGroupFile own = own

    mungeGroupUrl ('@':own) = "g" +/+ own
    mungeGroupUrl own = own

addKojiRepo :: Bool -> Bool -> String -> IO ()
addKojiRepo dryrun debug repo = do
  sysarch <- cmd "rpm" ["--eval", "%{_arch}"]
  -- FIXME repo validation/sanity: not "-k list" or other dnf command
  let repourl = "https://kojipkgs.fedoraproject.org/repos" +/+ repo +/+ "latest" +/+ sysarch ++ "/"
  unlessM (httpExists' repourl) $ error' $ "no such koji repo:" +-+ repourl
  template <- getDataFileName kojiRepoTemplate
  repodef <- cmd "sed" ["-e", "s/@REPO@/" ++ repo ++ "/g", template]
  let repofile = replace "REPO" repo kojiRepoTemplate
  exists <- doesFileExist repofile
  if exists
    then error' $ "repo already defined:" +-+ repofile
    else putStrLn $ "Setting up koji repo" +-+ repo
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
  mowned <- cmdMaybe "rpm" ["-qf", yumReposD </> repofile]
  case mowned of
    Just owner -> warning $ repofile +-+ "owned by" +-+ owner
    Nothing -> do
      ok <- yesNo $ "Remove" +-+ takeFileName repofile
      when ok $ do
        doSudo dryrun debug "rm" [repofile]

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif

maybeReleaseVer :: [String] -> Maybe String
maybeReleaseVer args =
  let releaseverOpt = "--releasever"
  in
    case findIndices (releaseverOpt `isPrefixOf`) args of
      [] -> Nothing
      is -> let lst = last is
                opt = args !! lst
                relver =
                  case dropPrefix releaseverOpt opt of
                    "" ->
                      if lst == length args
                      then error' $ releaseverOpt +-+ "without version"
                      else args !! (lst + 1)
                    '=':rv -> rv
                    _ -> error' $ "could not parse" +-+ opt
                  -- still not sure if this fully makes sense
            in if all isDigit relver || relver `elem` ["rawhide","eln"]
               then Just relver
               else error' $ "unknown releasever:" +-+ relver

checkSudo :: IO ()
checkSudo = do
  -- previously checked for euid (no username for termux-fedora)
  issudo <- isJust <$> lookupEnv "SUDO_USER"
  when issudo $
    warning "*No need to run dnf-repo directly with sudo*"

getRpmOsRelease :: IO String
getRpmOsRelease = do
  -- not defined for fedora branches
  let systemReleaseVer = "system-release(releasever)"
  mReleaseVerPkg <- cmdMaybe "rpm" ["-q", "--whatprovides", systemReleaseVer]
  case mReleaseVerPkg of
    Just relverPkg -> do
      mreleasever <- find (systemReleaseVer `isPrefixOf`) <$> cmdLines "rpm" ["-q", "--provides", relverPkg]
      case mreleasever of
        Just releasever -> return $ last (words releasever)
        Nothing -> error' $ "failed to determine" +-+ systemReleaseVer
    Nothing -> do
      let systemRelease = "system-release"
      msysreleasepkg <- cmdMaybe "rpm" ["-q", "--whatprovides", systemRelease]
      case msysreleasepkg of
        Just sysrelpkg -> do
          let prefix = systemRelease ++ "("
          msysrelease <- find (prefix `isPrefixOf`) <$> cmdLines "rpm" ["-q", "--provides", sysrelpkg]
          case msysrelease of
            Just sysrelease ->
              -- "system-release(40)"
              return $ init $ dropPrefix prefix sysrelease
            Nothing -> error' $ "failed to determine" +-+ systemRelease
        Nothing -> error' "failed to determine OS version"

getRpmOSName :: IO String
getRpmOSName = do
  let osrelease = "/etc/os-release"
      idkey = "ID="
  osids <- grep ('^' : idkey) osrelease
  case osids of
    [] -> error' $ "failed to find ID in" +-+ osrelease
    [osid] -> return $
              dropSuffix "\"" $
              dropPrefix "\"" $
              dropPrefix idkey osid
    oss -> error' $ "multiple IDs in" +-+ osrelease ++ ":" ++ unwords oss
