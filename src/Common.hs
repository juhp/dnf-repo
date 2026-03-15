module Common (
  getSysArch,
  getRpmOsRelease,
  getRpmOSName
  )
where

import Data.List.Extra (dropPrefix, dropSuffix, find, isPrefixOf)
import SimpleCmd (cmd, cmdLines, cmdMaybe, error', grep, (+-+))

getSysArch :: IO String
getSysArch =
  cmd "rpm" ["--eval", "%{_arch}"]

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
