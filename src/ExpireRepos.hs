module ExpireRepos (expireRepos) where

import Data.List (nub)
import SimpleCmd (sudo_)

expiredFile :: FilePath
expiredFile = "/var/cache/dnf/expired_repos.json"

expireRepos :: [String] -> IO ()
expireRepos repos = do
  old <- read <$> readFile expiredFile :: IO [String]
  let expired = nub $ old ++ repos
  sudo_ "sed" ["-i", "s/" ++ show old ++ "/" ++ show expired ++ "/",
               expiredFile]
