module KojiRepo (addKojiRepo)
where

import Control.Monad.Extra
import Data.List.Extra (replace)
import Network.HTTP.Directory (httpExists', (+/+))
import SimpleCmd (cmd, error', warning, (+-+))
import System.Directory (doesFileExist)
import System.FilePath
import System.IO.Extra (withTempDir)

import Paths_dnf_repo (getDataFileName)
import Common
import Sudo

kojiRepoTemplate :: FilePath
kojiRepoTemplate = "koji-REPO.repo"

addKojiRepo :: Bool -> Bool -> String -> IO ()
addKojiRepo dryrun debug repo = do
  sysarch <- getSysArch
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
