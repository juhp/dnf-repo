module ExpireRepos (
  expireRepos,
  clearExpired
  ) where

import Control.Monad
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
  ok <- yesno "Expire caches of above repos"
  when ok $ do
    doSudo dryrun debug "sed" ["-i", "-e",
                               "s/" ++ renderShow old ++ "/" ++ renderShow expired ++ "/",
                               expiredFile]
    unless dryrun $ do
      putStrLn ""
      putStrLn $ "marked expired in " ++ expiredFile
  putStrLn ""

renderShow :: [String] -> String
renderShow = render . show
  where
    render :: String -> String
    render "" = ""
    render (c:cs) =
      (if c `elem` "[]" then ['\\',c] else [c]) ++ render cs

clearExpired :: Bool -> Bool -> IO ()
clearExpired dryrun debug = do
  old <- read <$> readFile expiredFile :: IO [String]
  if null old
    then return ()
    else do
    mapM_ putStrLn old
    putStrLn ""
    ok <- yesno "Unset cache expirations"
    when ok $ do
      doSudo dryrun debug "sed" ["-i", "-e",
                               "s/" ++ renderShow old ++ "/" ++ renderShow [] ++ "/",
                               expiredFile]
      putStrLn ""
      putStrLn $ "expirations cleared in " ++ expiredFile
    putStrLn ""
