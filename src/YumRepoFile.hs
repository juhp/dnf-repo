{-# LANGUAGE TupleSections #-}


module YumRepoFile (
  Mode(..),
  SpecificChange(..),
  readRepos,
  RepoState,
  selectRepo,
  changeRepo,
  saveRepo,
  updateState,
  expiring,
  deleting
  )
where

import Data.Either (rights)
import Data.List.Extra (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, sortOn,
                        stripInfix, trim)
import Data.Maybe (mapMaybe)
import SimpleCmd (error')
import System.FilePath.Glob (compile, match)

type RepoState = (String,(Bool,FilePath))

data Mode = AddCopr String | AddKoji String
          | EnableRepo String | DisableRepo String
          | ExpireRepo String | ClearExpires
          | DeleteRepo String
          | Specific SpecificChange
  deriving (Eq, Ord, Show)

modePattern :: Mode -> Maybe String
modePattern (AddCopr c) = Just c
modePattern (AddKoji k) = Just k
modePattern (EnableRepo r) = Just r
modePattern (DisableRepo r) = Just r
modePattern (ExpireRepo r) = Just r
modePattern ClearExpires = Nothing
modePattern (DeleteRepo r) = Just r
modePattern (Specific _) = Nothing

data SpecificChange = EnableModular | DisableModular
                    | EnableTesting | DisableTesting
                    | EnableDebuginfo | DisableDebuginfo
                    | EnableSource | DisableSource
  deriving (Eq, Ord, Show)

repoSubstr :: SpecificChange -> String
repoSubstr EnableModular = "-modular"
repoSubstr DisableModular = "-modular"
repoSubstr EnableTesting = "-testing"
repoSubstr DisableTesting = "-testing"
repoSubstr EnableDebuginfo = "-debuginfo"
repoSubstr DisableDebuginfo = "-debuginfo"
repoSubstr EnableSource = "-source"
repoSubstr DisableSource = "-source"

data ChangeEnable = Disable String
                  | Enable String
                  | Expire String
                  | UnExpire
                  | Delete FilePath
  deriving (Eq,Ord,Show)

repoName :: ChangeEnable -> Maybe String
repoName (Disable r) = Just r
repoName (Enable r) = Just r
repoName (Expire r) = Just r
repoName UnExpire = Nothing
repoName (Delete _) = Nothing

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
updateState (ce:ces) re@(repo,(enabled,file)) =
  case ce of
    Disable r | r == repo && enabled -> (repo,(False,file))
    Enable r | r == repo && not enabled -> (repo,(True,file))
    _ -> updateState ces re

selectRepo :: Bool -> [RepoState] -> [Mode] -> [Either String ChangeEnable]
selectRepo exact repostates modes =
  nub $ foldr selectRepo' [] (nub (sort modes))
  where
    selectRepo' :: Mode -> [Either String ChangeEnable] -> [Either String ChangeEnable]
    selectRepo' mode acc =
      let results = nub $ mapMaybe (selectRepoMode mode acc) repostates
      in
        case results of
          [] -> error' ("no match for repo pattern action: " ++ show mode)
          [_] -> acc ++ results
          _ ->
            acc ++
            case modePattern mode of
              Nothing -> results
              Just p ->
                if isGlob p
                then results
                else
                  let actNames =
                        mapMaybe (\a -> (a,) <$> repoName a) $ rights results
                  in
                    if null actNames
                    then results
                    else
                      let (base,basename) =
                            head $ sortOn (length . snd) actNames
                      in
                        if all (basename `isPrefixOf`) $ map snd actNames
                        then [Right base]
                        else results

    selectRepoMode :: Mode -> [Either String ChangeEnable] -> RepoState
                   -> Maybe (Either String ChangeEnable)
    selectRepoMode mode acc (name,(enabled,file)) =
      case mode of
        AddCopr repo ->
          maybeEither repo isSuffixOf (not enabled) (Enable name)
        AddKoji repo ->
          maybeEither repo isSuffixOf (not enabled) (Enable name)
        EnableRepo pat ->
          maybeEither pat matchesRepo (not enabled) (Enable name)
        DisableRepo pat ->
          maybeEither pat matchesRepo enabled (Disable name)
        ExpireRepo pat ->
          maybeEither pat matchesRepo True (Expire name)
        ClearExpires -> Just (Right UnExpire)
        DeleteRepo pat ->
          maybeEither pat matchesRepo
          (not enabled || error ("disable repo before deleting: " ++ name))
          (Delete file)
        Specific change ->
          let substr =  repoSubstr change in
            if change `elem`
               [EnableModular,EnableTesting,EnableDebuginfo,EnableSource]
            then
              maybeEither substr
              (\p n -> p `isInfixOf` n &&
                       repoStatus acc (removeInfix substr name) True)
              (not enabled) (Enable name)
            else
              maybeEither substr isInfixOf enabled (Disable name)
      where
        maybeEither :: String -> (String -> String -> Bool) -> Bool
                    -> ChangeEnable -> Maybe (Either String ChangeEnable)
        maybeEither pat matcher state change =
          if pat `matcher` name
          then
            if state
            then Just $ Right change
            else
              if isGlob pat
              then Nothing
              else Just $ Left $
                   name ++ " is already " ++
                   if enabled then "enabled" else "disabled"
          else Nothing

    repoStatus :: [Either String ChangeEnable] -> String -> Bool -> Bool
    repoStatus acc repo state =
      case lookup repo repostates of
        Just (enabled,_) ->
          enabled == state || Right (Enable repo) `elem` acc
        Nothing -> False

    isGlob :: String -> Bool
    isGlob pat = any (`elem` pat) "*?["

    matchesRepo :: String -> String -> Bool
    matchesRepo "" = error' "empty repo pattern"
    matchesRepo pat
      | isGlob pat = (match . compile) $
        if exact
        then pat
        else
          case ('^' == head pat,'$' == last pat) of
            (True,True) -> init (tail pat)
            (True,False) -> tail pat ++ ['*' | last pat /= '*']
            (False,True) -> ['*' | head pat /= '*'] ++ init pat
            _ -> ['*' | head pat /= '*'] ++ pat ++ ['*' | last pat /= '*']
      | exact = (pat ==)
      | otherwise =
          case ('^' == head pat,'$' == last pat) of
            (True,True) -> (init (tail pat) ==)
            (True,False) -> (tail pat `isPrefixOf`)
            (False,True) -> (init pat `isSuffixOf`)
            _ -> (pat `isInfixOf`)

readRepos :: FilePath -> IO [RepoState]
readRepos file =
  parseRepos file . lines <$> readFile file

-- parse ini
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
      in (section,(enabled,file)) : parseRepos file more
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

-- adapted from simple-cmd
removeInfix :: String -> String-> String
removeInfix inf orig =
  maybe orig (uncurry (++)) $ stripInfix inf orig
