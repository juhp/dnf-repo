module CoprRepo (
  addCoprRepo
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Network.Curl (curlGetString, CurlCode(CurlOK))
import Network.HTTP.Directory ((+/+))
import SimpleCmd (cmdLines, cmdMaybe, error', grep, warning, (+-+))
import System.Directory (doesFileExist)
import System.FilePath
import System.IO.Extra (withTempDir)

import Sudo

fedoraCopr :: String
fedoraCopr = "copr.fedorainfracloud.org"

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
