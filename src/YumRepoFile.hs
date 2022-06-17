{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (
  Mode(..),
  readRepo,
  selectRepo,
  Modular(..),
  Testing(..),
  changeRepo,
  saveRepo,
  expiring
  )
where

import Data.List.Extra (isPrefixOf, isInfixOf, isSuffixOf, replace, trim)
import SimpleCmd (error')

type RepoState = (String,Bool)

data Mode = AddCopr String | EnableRepo String | DisableRepo String
          | ExpireRepo String | Default
  deriving Eq

data ChangeEnable = Disable String | Enable String | Expire String
  deriving (Eq,Ord,Show)

changeRepo :: ChangeEnable -> [String]
changeRepo (Disable r) = ["--disablerepo", r]
changeRepo (Enable r) = ["--enablerepo", r]
changeRepo _ = []

saveRepo :: ChangeEnable -> [String]
saveRepo (Disable r) = ["--disable", r]
saveRepo (Enable r) = ["--enable", r]
saveRepo _ = []

expiring :: ChangeEnable -> Maybe String
expiring (Expire r) = Just r
expiring _ = Nothing

data Modular = EnableModular | DisableModular
  deriving Eq

data Testing = EnableTesting | DisableTesting
  deriving Eq

selectRepo :: Bool -> Mode -> Maybe Testing -> Maybe Modular
           -> RepoState -> Maybe ChangeEnable
selectRepo _debug mode mtesting mmodular (name,enabled) =
  case mode of
    AddCopr repo -> if replace "/" ":" repo `isSuffixOf` name && not enabled
                    then Just (Enable name)
                    else selectOther
    EnableRepo pat -> if pat `isInfixOf` name && not enabled
                  then Just (Enable name)
                  else selectOther
    DisableRepo pat -> if pat `isInfixOf` name && enabled
                   then Just (Disable name)
                   else selectOther
    ExpireRepo pat -> if pat `isInfixOf` name
                  then Just (Expire name)
                  else Nothing
    _ -> selectOther
  where
    selectOther :: Maybe ChangeEnable
    selectOther
      | "modular" `isSuffixOf` name =
        case mmodular of
          Nothing -> Nothing
          Just include ->
            case include of
              EnableModular | not enabled ->
                               if "testing" `isInfixOf` name
                               then if mtesting == Just EnableTesting
                                    then Just (Enable name)
                                    else Nothing
                               else Just (Enable name)
              DisableModular | enabled -> Just (Disable name)
              _ -> Nothing
      | "testing" `isInfixOf` name =
          case mtesting of
            Nothing -> Nothing
            Just include ->
              case include of
                EnableTesting | not enabled -> Just (Enable name)
                DisableTesting | enabled -> Just (Disable name)
                _ -> Nothing
      | otherwise = Nothing

readRepo :: FilePath -> IO RepoState
readRepo file =
  parseRepo file . lines <$> readFile file

-- was called parseIni
parseRepo :: FilePath -> [String] -> RepoState
parseRepo file [] = error' $ "empty ini file: " ++ file
parseRepo file (l:ls) =
  case trim l of
    "" -> parseRepo file ls
    ('#':_) -> parseRepo file ls
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
