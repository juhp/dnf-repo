{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (readRepoNames) where

import Data.List.Extra (isPrefixOf, trim)
import SimpleCmd (error')

readRepo :: FilePath -> IO (String,Bool)
readRepo file =
  parseIni file . lines <$> readFile file

parseIni :: FilePath -> [String] -> (String,Bool)
parseIni file [] = error' $ "empty ini file: " ++ file
parseIni file (l:ls) =
  case trim l of
    "" -> parseIni file ls
    sec ->
      let section = secName sec
          enabled =
            case dropWhile (not . ("enabled=" `isPrefixOf`)) ls of
              [] -> error' $ "no enabled field for " ++ section
              (e:_) ->
                case trim e of
                  "enabled=1" -> True
                  "enabled=0" -> False
                  _ -> error' $ "unknown enabled state " ++ e ++ " for " ++ section
      in (section,enabled)
  where
    secName :: String -> String
    secName sec =
      if ' ' `elem` sec
      then error' $ "section contains space: " ++ sec
      else case sec of
        ('[' : rest) ->
          if last rest == ']'
          then init rest
          else error' $ "bad section " ++ sec ++ " in " ++ file
        _ -> error' $ "section not found in " ++ file

readRepoNames :: Bool -> [FilePath] -> IO [FilePath]
readRepoNames disable files = do
  reposEnabled <- mapM readRepo files
  return $ map fst $ filter (selectRepo . snd) reposEnabled
  where
    selectRepo :: Bool -> Bool
    selectRepo =
      if disable then id else not
