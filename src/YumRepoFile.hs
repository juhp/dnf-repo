{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (readRepoName) where

import Data.Ini
import qualified Data.Text as T
import SimpleCmd (error')

-- FIXME check enabled?
readRepoName :: FilePath -> IO String
readRepoName file = do
  ini <- readIniFile file
  case ini of
    Left err -> error' err
    Right i ->
      case sections i of
        [sec] -> return $ T.unpack sec
        _ -> error' $ "broken repo file: " ++ file
