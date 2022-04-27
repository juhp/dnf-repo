module ExpireRepos (expireRepos) where

import Data.List (nub)

import Sudo

expiredFile :: FilePath
expiredFile = "/var/cache/dnf/expired_repos.json"

expireRepos :: Bool -> [String] -> IO ()
expireRepos dryrun repos = do
  old <- read <$> readFile expiredFile :: IO [String]
  let expired = nub $ old ++ repos
  doSudo dryrun "sed" ["-i", "-e", "s/" ++ show old ++ "/" ++ show expired ++ "/",
               expiredFile]
