{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (readRepoName) where

import Data.List.Extra (trim)
import Data.Maybe (mapMaybe)
import SimpleCmd (error', grep)

-- FIXME check enabled?
readRepoName :: FilePath -> IO String
readRepoName file = do
  parts <- grep "^\\[" file
  case mapMaybe (secName . trim) parts of
    [] -> error' $ "repo file with no sections: " ++ file
    (sec:_) -> putStrLn sec >> return sec
  where
    secName :: String -> Maybe String
    secName ('[' : rest) | ' ' `notElem` rest =
      if last rest == ']'
      then Just (init rest)
      else Nothing
    secName xs = error' $ "bad section " ++ show xs ++ " in " ++ file
