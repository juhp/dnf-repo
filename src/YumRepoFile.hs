{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (
  readRepoNames,
  Modular(..),
  Testing(..)
  )
where

import Data.List.Extra (isPrefixOf, isInfixOf, trim)
import SimpleCmd (error')

readRepoNames :: Bool -> Maybe Testing -> Maybe Modular -> [FilePath]
              -> IO [FilePath]
readRepoNames disable mtesting mmodular files = do
  reposEnabled <- mapM readRepo files
  return $
    map fst $
    filter (selectTest . fst) $
    filter (selectModular . fst) $
    filter (selectEnable . snd) reposEnabled
  where
    readRepo :: FilePath -> IO (String,Bool)
    readRepo file =
      parseIni file . lines <$> readFile file

    selectEnable :: Bool -> Bool
    selectEnable = if disable then id else not

    selectModular :: String -> Bool
    selectModular name =
      if "modular" `isInfixOf` name
      then case mmodular of
        Nothing -> False
        Just enable -> selectEnable (enable == DisableModular)
      else True

    selectTest :: String -> Bool
    selectTest name =
      if "testing" `isInfixOf` name
      then case mtesting of
        Nothing -> False
        Just enable -> selectEnable (enable == DisableTesting)
      else True

data Modular = EnableModular | DisableModular
  deriving Eq

data Testing = EnableTesting | DisableTesting
  deriving Eq

parseIni :: FilePath -> [String] -> (String,Bool)
parseIni file [] = error' $ "empty ini file: " ++ file
parseIni file (l:ls) =
  case trim l of
    "" -> parseIni file ls
    ('#':_) -> parseIni file ls
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
