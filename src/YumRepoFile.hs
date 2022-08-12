{-# LANGUAGE OverloadedStrings #-}

module YumRepoFile (
  Mode(..),
  readRepos,
  RepoState,
  selectRepo,
  Modular(..),
  Testing(..),
  changeRepo,
  saveRepo,
  updateState,
  expiring,
  deleting
  )
where

import Data.List.Extra (isPrefixOf, isInfixOf, isSuffixOf, trim)
import SimpleCmd (error')

type RepoState = (String,Bool,FilePath)

data Mode = AddCopr String | AddKoji String
          | EnableRepo String | DisableRepo String
          | ExpireRepo String | DeleteRepo String
  deriving Eq

data ChangeEnable = Disable String | Enable String | Expire String
                  | Delete FilePath
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

deleting :: ChangeEnable -> Maybe String
deleting (Delete r) = Just r
deleting _ = Nothing

updateState :: [ChangeEnable] -> RepoState -> RepoState
updateState [] rs = rs
updateState (ce:ces) re@(repo,enabled,file) =
  case ce of
    Disable r | r == repo && enabled -> (repo,False,file)
    Enable r | r == repo && not enabled -> (repo,True,file)
    _ -> updateState ces re

data Modular = EnableModular | DisableModular
  deriving Eq

data Testing = EnableTesting | DisableTesting
  deriving Eq

selectRepo :: Bool -> [Mode] -> Maybe Testing -> Maybe Modular
           -> RepoState -> [ChangeEnable]
selectRepo exact modes mtesting mmodular (name,enabled,file) =
  case modes of
    [] -> selectOther
    (mode:modes') ->
      case mode of
        AddCopr repo -> if repo `isSuffixOf` name && not enabled
                        then [Enable name]
                        else selectOther
        AddKoji repo -> if repo `isSuffixOf` name && not enabled
                        then [Enable name]
                        else selectOther
        EnableRepo pat -> if pat `matchesRepo` name && not enabled
                      then [Enable name]
                      else selectOther
        DisableRepo pat -> if pat `matchesRepo` name && enabled
                       then [Disable name]
                       else selectOther
        ExpireRepo pat -> [Expire name | pat `matchesRepo` name]
        DeleteRepo pat -> if pat `matchesRepo` name
                      then if enabled
                           then error' $ "disable repo before deleting: " ++ name
                           else [Delete file]
                      else []
      ++ selectRepo exact modes' mtesting mmodular (name,enabled,file)
  where
    matchesRepo = if exact then (==) else isInfixOf

    selectOther :: [ChangeEnable]
    selectOther
      | "modular" `isSuffixOf` name =
        case mmodular of
          Nothing -> []
          Just include ->
            case include of
              EnableModular | not enabled ->
                               if "testing" `isInfixOf` name
                               then
                                 [Enable name | mtesting == Just EnableTesting]
                               else [Enable name]
              DisableModular | enabled -> [Disable name]
              _ -> []
      | "testing" `isInfixOf` name =
          case mtesting of
            Nothing -> []
            Just include ->
              case include of
                EnableTesting | not enabled -> [Enable name]
                DisableTesting | enabled -> [Disable name]
                _ -> []
      | otherwise = []

readRepos :: FilePath -> IO [RepoState]
readRepos file =
  parseRepos file . lines <$> readFile file

-- was called parseIni
parseRepos :: FilePath -> [String] -> [RepoState]
parseRepos file ls =
  case nextSection ls of
    Nothing -> []
    Just (section,rest) ->
      let (enabled,more) =
            case dropWhile (not . ("enabled=" `isPrefixOf`)) rest of
              [] -> error' $ "no enabled field for " ++ section
              (e:more') ->
                case trim e of
                  "enabled=1" -> (True,more')
                  "enabled=0" -> (False,more')
                  _ -> error' $ "unknown enabled state " ++ e ++ " for " ++ section
      in (section,enabled,file) : parseRepos file more
  where
    nextSection :: [String] -> Maybe (String,[String])
    nextSection [] = Nothing
    nextSection (l:ls') =
      case l of
        ('[' : rest) ->
          if last rest == ']'
          then Just (init rest, ls')
          else error' $ "bad section " ++ l ++ " in " ++ file
        _ -> nextSection ls'
