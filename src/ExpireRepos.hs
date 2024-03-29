module ExpireRepos (
  expireRepo,
  clearExpired
  ) where

import Control.Monad
import Data.List (nub)
import SimpleCmd (error')
import SimplePrompt (yesNo)

import Sudo

expiredFile :: FilePath
expiredFile = "/var/cache/dnf/expired_repos.json"

expireRepo :: Bool -> Bool -> String -> IO ()
expireRepo dryrun debug repo = do
  old <- read <$> readFile expiredFile :: IO [String]
  let expired = nub $ old ++ [repo]
  ok <- yesNo $ "Mark '" ++ repo ++ "' cache expired"
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
    then error' "no expired repos"
    else do
    mapM_ putStrLn old
    putStrLn ""
    ok <- yesNo "Unset cache expirations"
    when ok $ do
      doSudo dryrun debug "sed" ["-i", "-e",
                               "s/" ++ renderShow old ++ "/" ++ renderShow [] ++ "/",
                               expiredFile]
      putStrLn ""
      putStrLn $ "expirations cleared in " ++ expiredFile
