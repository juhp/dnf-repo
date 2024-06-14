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
import System.Directory (doesDirectoryExist, doesFileExist, findFile,
                         withCurrentDirectory,
#if !MIN_VERSION_simple_cmd(0,2,4)
                         listDirectory
#endif
                        )
import System.Environment (lookupEnv)
import System.FilePath
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Extra (withTempDir)

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
    <*> switchLongWith "dnf4" "Use dnf4 (if dnf5 available)"
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
          -- FIXME handle copr url too
          serverAliases . replace "@" "group_" . replace "/" ":" . dropWhileEnd (== '/') . dropWhile (== '/') . handleCoprUrl

        serverAliases ('r':'e':'d':'h':'a':'t':':':copr) =
          "copr.devel.redhat.com:" ++ copr
        serverAliases copr = copr

        handleCoprUrl url =
          if "https:" `isPrefixOf` url
          then replace "coprs/" "" $ dropPrefix "https:" url
          else url

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
      <$> repoOptionWith 'c' "add-copr" "[SERVER/]COPR/PROJECT|URL" "Install copr repo file (defaults to fedora server)"
      <*> optional (strOptionLongWith "osname" "OSNAME" "Specify OS Name to override (eg epel)")
      <*> optional (strOptionLongWith "releasever" "RELEASEVER" "Specify OS Release Version to override (eg rawhide)") <|>
      AddKoji
      <$> repoOptionWith 'k' "add-koji" "REPO" "Create repo file for a Fedora koji repo (f40-build, rawhide, epel9-build, etc)" <|>
      AddRepo
      <$> strOptionWith 'r' "add-repofile" "REPOFILEURL" "Install repo file"
      <*> optional (strOptionLongWith "releasever" "RELEASEVER" "Specify OS Release Version to override (eg rawhide)") <|>
      RepoURL
      <$> strOptionWith 'u' "repourl" "URL" "Use temporary repo from a baseurl"

fedoraCopr :: String
fedoraCopr = "copr.fedorainfracloud.org"

kojiRepoTemplate :: FilePath
kojiRepoTemplate = "koji-REPO.repo"

yumReposD :: String
yumReposD = "/etc/yum.repos.d"

checkSystemPathFile :: String -> IO (Maybe String)
checkSystemPathFile prog = do
  let path = splitOn ":" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin"
  fmap takeFileName <$> findFile path prog

-- FIXME both enabling and disabled at the same time
-- FIXME confirm repos if many
-- FIXME --disable-non-cores (modular,testing,cisco, etc)
runMain :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Bool -> Bool
        -> [Mode] -> [String] -> IO ()
runMain dryrun quiet debug listrepos save dnf4 mweakdeps exact modes args = do
  hSetBuffering stdout NoBuffering
  unlessM (doesDirectoryExist yumReposD) $
    error' $ yumReposD +-+ "not found!"
  withCurrentDirectory yumReposD $ do
    forM_ modes $
      \case
        AddCopr copr mosname mrelease ->
          addCoprRepo dryrun debug mosname mrelease copr
        AddKoji repo ->
          addKojiRepo dryrun debug repo
        AddRepo repo mrelease ->
          addRepoFile dryrun debug mrelease repo
        _ -> return ()
    repofiles <- filesWithExtension "./" "repo"
    when debug $ print modes
    nameStates <- sort <$> concatMapM readRepos repofiles
    when debug $ mapM_ print nameStates
    let actions = selectRepo exact nameStates modes
        moreoutput = not (null args) || null actions || listrepos
    when (debug && not (null modes)) $
      if null actions
      then putStrLn "no actions"
      else print actions
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
            mdnf3 <- checkSystemPathFile "dnf-3"
            case mdnf3 of
              Just dnf3 ->
                -- FIXME cannot combine --disable and --enable
                doSudo dryrun debug dnf3 $ "config-manager" : changes
              Nothing ->
                -- FIXME need to have repo files in changes
                -- doSudo dryrun debug "sed" $ "config-manager" : changes
                error' "Saving repo state not yet supported without dnf"
    if null args
      then
      when (null actions || listrepos) $ do
      when save $ putStrLn ""
      listRepos $ map (updateState actions) nameStates
      else do
      when save $ putStrLn ""
      mdnf <-
        if dnf4
        then checkSystemPathFile "dnf"
        else
          maybeM (checkSystemPathFile "dnf") (return . Just) $
          checkSystemPathFile "dnf5"
      case mdnf of
        Just dnf ->
          let repoargs = mapMaybe changeRepo actions
              weakdeps = maybe [] (\w -> ["--setopt=install_weak_deps=" ++ show w]) mweakdeps
              quietopt = if quiet then ("-q" :) else id
              cachedir = ["--setopt=cachedir=/var/cache/dnf" </> relver | relver <- maybeToList (maybeReleaseVer args)]
              extraargs =
                -- special case for "dnf-repo -c owner/project install"
                case modes of
                  [mode] ->
                    case mode of
                      AddCopr proj _ _ | args == ["install"] ->
                                           [takeWhileEnd (/= ':') proj]
                      _ -> []
                  _ -> []
          in doSudo dryrun debug dnf $
             quietopt repoargs ++ cachedir ++ weakdeps ++ map mungeArg args ++
             extraargs
        -- FIXME rpm-ostree install supports --enablerepo
        Nothing -> error' "missing dnf (rpm-ostree is not supported)"
  where
    mungeArg :: String -> String
    mungeArg "distrosync" = "distro-sync"
    mungeArg arg = arg

addCoprRepo :: Bool -> Bool -> Maybe String -> Maybe String -> String -> IO ()
addCoprRepo dryrun debug mosname mrelease repo = do
  let (server,owner,project) = serverOwnerProject repo
      repofile =
        "_copr:" ++ server ++ ':' : owner ++ ':' : project <.> "repo"
  exists <- doesFileExist repofile
  if exists
    then warning $ "copr repo already defined:" +-+ repofile
    else do
    osName <- maybe getRpmOSName return mosname
    osVersion <- maybe getRpmOsRelease return mrelease
    let repofileUrl = "https://" ++ server +/+ "coprs" +/+ mungeGroupUrl owner +/+ project +/+ "repo" +/+ osName ++ '-' : osVersion +/+ owner ++ '-' : project <.> "repo"
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
    mungeGroupUrl ('g':'r':'o':'u':'p':'_':own) = "g" +/+ own
    mungeGroupUrl own = own

    serverOwnerProject rpo =
        case  splitOn ":" rpo of
          [] -> error' "empty repo string"
          [_] -> error' $ "unqualified repo project:" +-+ rpo
          [o,p] -> (fedoraCopr, o , p)
          [c,o,p] ->
            if '.' `elem` c
            then (c, o, p)
            else error' $ "unknown copr server:" +-+ rpo
          ["copr",_,_,_] -> serverOwnerProject $ dropPrefix "copr:" rpo
          _ -> error' $ "unknown copr:" +-+ rpo

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
    then warning $ "koji repo already defined:" +-+ repofile
    else do
    putStrLn $ "Setting up koji repo" +-+ repo
    withTempDir $ \ tmpdir -> do
      let tmpfile = tmpdir </> repofile
      unless dryrun $ writeFile tmpfile repodef
      doSudo dryrun debug "cp" [tmpfile, repofile]
      putStrLn ""

-- FIXME maybe handle string (vscode) or local file?
addRepoFile :: Bool -> Bool -> Maybe String -> String -> IO ()
addRepoFile dryrun debug mrelease url = do
  unless (".repo" `isExtensionOf` url) $
    error' $ url +-+ "does not appear to be a .repo file"
  let repofile = takeFileName url
  exists <- doesFileExist repofile
  if exists
    then warning $ "repo file already exists:" +-+ repofile
    else do
    (curlres,curlcontent) <- curlGetString url []
    unless (curlres == CurlOK) $
      error' $ "downloading failed of" +-+ url
    putStrLn $ "Setting up" +-+ repofile
    withTempDir $ \ tmpdir -> do
      let tmpfile = tmpdir </> repofile
      unless dryrun $ writeFile tmpfile $
        maybe id (replace "$releasever") mrelease $
        maybe id (replace "$releasever") mrelease $
        replace "enabled=1" "enabled=0" curlcontent
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
