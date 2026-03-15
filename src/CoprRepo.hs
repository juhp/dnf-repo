module CoprRepo (
  addCoprRepo
  )
where

import Control.Monad.Extra
import Data.List.Extra
import Network.Curl (curlGetString, CurlCode(CurlOK))
import Network.HTTP.Directory ((+/+))
import SimpleCmd (error', warning, (+-+))
import System.Directory (doesFileExist)
import System.FilePath
import System.IO.Extra (withTempDir)

import Common
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
