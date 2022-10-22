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

import Data.List.Extra (isPrefixOf, isInfixOf, isSuffixOf, nub, sort,
                        stripInfix, trim)
import SimpleCmd (error')
import System.FilePath.Glob (compile, match)

type RepoState = (String,(Bool,FilePath))

data Mode = AddCopr String | AddKoji String
          | EnableRepo String | DisableRepo String
          | ExpireRepo String | ClearExpires
          | DeleteRepo String
          | Specific SpecificChange
  deriving (Eq, Ord, Show)

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

data ChangeEnable = Disable String | Enable String | Expire String | UnExpire
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
updateState (ce:ces) re@(repo,(enabled,file)) =
  case ce of
    Disable r | r == repo && enabled -> (repo,(False,file))
    Enable r | r == repo && not enabled -> (repo,(True,file))
    _ -> updateState ces re

selectRepo :: Bool -> [RepoState] -> [Mode] -> [ChangeEnable]
selectRepo exact repostates modes =
  nub $ foldr selectRepo' [] (sort modes)
  where
    selectRepo' :: Mode -> [ChangeEnable] -> [ChangeEnable]
    selectRepo' mode acc =
      let result = concatMap (selectRepoMode mode acc) repostates
      in
        if null result
        then error' ("no match for repo pattern action: " ++ show mode)
        else acc ++ result

    selectRepoMode :: Mode -> [ChangeEnable] -> RepoState -> [ChangeEnable]
    selectRepoMode mode acc (name,(enabled,file)) =
      case mode of
        AddCopr repo ->
          [Enable name | repo `isSuffixOf` name, not enabled]
        AddKoji repo ->
          [Enable name | repo `isSuffixOf` name, not enabled]
        EnableRepo pat ->
          [Enable name | pat `matchesRepo` name, not enabled]
        DisableRepo pat ->
          [Disable name | pat `matchesRepo` name, enabled]
        ExpireRepo pat ->
          [Expire name | pat `matchesRepo` name]
        ClearExpires -> [UnExpire]
        DeleteRepo pat ->
          if pat `matchesRepo` name
          then if enabled
               then error' $ "disable repo before deleting: " ++ name
               else [Delete file]
          else []
        Specific change ->
          let substr =  repoSubstr change in
            if change `elem`
               [EnableModular,EnableTesting,EnableDebuginfo,EnableSource]
            then [Enable name | substr `isInfixOf` name, not enabled,
                  repoStatus acc (removeInfix substr name) True]
            else [Disable name | substr `isInfixOf` name, enabled]

    repoStatus :: [ChangeEnable] -> String -> Bool -> Bool
    repoStatus acc repo state =
      case lookup repo repostates of
        Just (enabled,_) ->
          enabled == state || Enable repo `elem` acc
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
