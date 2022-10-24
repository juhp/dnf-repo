module ExpireRepos (
  expireRepo,
  clearExpired
  ) where

import Control.Monad
import Data.List (nub)

import Sudo

expiredFile :: FilePath
expiredFile = "/var/cache/dnf/expired_repos.json"

expireRepo :: Bool -> Bool -> String -> IO ()
expireRepo dryrun debug repo = do
  old <- read <$> readFile expiredFile :: IO [String]
  let expired = nub $ old ++ [repo]
  ok <- yesno $ "Mark '" ++ repo ++ "' cache expired"
  when ok $ do
    doSudo dryrun debug "sed" ["-i", "-e",
                               "s/" ++ renderShow old ++ "/" ++ renderShow expired ++ "/",
                               expiredFile]
    unless dryrun $ do
      putStrLn ""
      putStrLn $ repo ++ " marked expired in " ++ expiredFile

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
