module ExpireRepos (expireRepos) where

import Data.List (nub)
import SimpleCmd (error')

import Sudo

expiredFile :: FilePath
expiredFile = "/var/cache/dnf/expired_repos.json"

expireRepos :: Bool -> Bool -> [String] -> IO ()
expireRepos _ _ [] = error' "no repos to expire given"
expireRepos dryrun debug repos = do
  old <- read <$> readFile expiredFile :: IO [String]
  let expired = nub $ old ++ repos
  doSudo dryrun debug "sed" ["-i", "-e",
                       "s/" ++ renderShow old ++ "/" ++ renderShow expired ++ "/",
                       expiredFile]
  putStrLn $ "expired now: " ++ show expired
  where
    renderShow :: [String] -> String
    renderShow = render . show

    render :: String -> String
    render "" = ""
    render (c:cs) =
      (if c `elem` "[]" then ['\\',c] else [c]) ++ render cs
