module State (
  Mode(..),
  SpecificChange(..),
  ChangeEnable(Expire,UnExpire,Delete),
  printAction,
  reduceOutput,
  selectRepo,
  changeRepo,
  saveRepo,
  updateState,
  expiring,
  deleting
  )
where

--import Debug.Trace (traceShowId)
import Data.Either (partitionEithers)
import Data.List.Extra (dropPrefix, dropSuffix, foldl',
                        isPrefixOf, isInfixOf, isSuffixOf, nub,
                        replace, sortOn, stripInfix)
import Data.Maybe (mapMaybe)
import SimpleCmd (error', (+-+))
import System.FilePath.Glob (compile, match)

import YumRepoFile (RepoState)

data Mode = AddCopr String (Maybe String) (Maybe String) | AddKoji String
          | RepoURL String
          | EnableRepo String | DisableRepo String | OnlyRepo String
          | ExpireRepo String | ClearExpires
          | DeleteRepo String
          | Specific SpecificChange
  deriving (Eq, Ord, Show)

modePattern :: Mode -> Maybe String
modePattern (AddCopr c _ _) = Just c
modePattern (AddKoji k) = Just k
modePattern (RepoURL _) = Nothing
modePattern (EnableRepo r) = Just r
modePattern (DisableRepo r) = Just r
modePattern (OnlyRepo r) = Just r
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

-- Bool is current repo state
data ChangeEnable = Disable String Bool
                  | Enable String Bool
                  | Only String Bool
                  | Expire String Bool
                  | UnExpire
                  | Delete FilePath Bool
                  | BaseURL String
  deriving (Eq,Ord,Show)

printAction :: Bool -> ChangeEnable -> Maybe (Either String String)
printAction save (Disable r s) =
  if s
  then
    Just $ Right $
    if save
    then "disable" +-+ quote r
    else "with disabled" +-+ quote r
  else Just $ Left $ quote r +-+ "already disabled"
printAction save (Enable r s) =
  if s
  then
    Just . Right $
    if save
    then "enable" +-+ quote r
    else "with enabled" +-+ quote r
  else Just $ Left $ quote r +-+ "already enabled"
printAction _ (Only r _) =
  Just . Right $ "with only" +-+ quote r
printAction _ (Expire r s) =
  if s
  then
    Just . Right $
    "with enabled" +-+ quote r
  else Nothing
printAction _ UnExpire =
  Just $ Right "unexpire:"
printAction _ (Delete f s) =
  if s
  then Just $ Right $ "delete" +-+ quote f
  else Just $ Left $ quote f +-+ "deletion skipped"
printAction _ (BaseURL _) = Nothing

reduceOutput :: [Either String String] -> [String]
reduceOutput os =
  case partitionEithers os of
    (is,[]) -> is
    (_,cs) -> cs

quote :: String -> String
quote s = '\'' : s ++ "'"

maybeRepoName :: ChangeEnable -> Maybe (ChangeEnable, String)
maybeRepoName d@(Disable r _) = Just (d, r)
maybeRepoName e@(Enable r _) = Just (e, r)
maybeRepoName o@(Only r _) = Just (o, r)
maybeRepoName x@(Expire r _) = Just (x, r)
maybeRepoName UnExpire = Nothing
maybeRepoName (Delete _ _) = Nothing
maybeRepoName (BaseURL _) = Nothing

changeRepo :: ChangeEnable -> Maybe String
changeRepo (Disable r True) = Just $ "--disablerepo=" ++ r
changeRepo (Enable r True) = Just $ "--enablerepo=" ++ r
changeRepo (Only r True) = Just $ "--repo=" ++ r
changeRepo (Expire r True) = Just $ "--enablerepo=" ++ r
changeRepo (BaseURL url) = Just $ "--repofrompath=" ++ repoUrlName ++ "," ++ url
  where
    repoUrlName = replace "/" ":" $
                  dropSuffix "/" $
                  dropPrefix "http://" $
                  dropPrefix "https://" url
changeRepo _ = Nothing

saveRepo :: ChangeEnable -> [String]
saveRepo (Disable r True) = ["--disable", r]
saveRepo (Enable r True) = ["--enable", r]
saveRepo _ = []

expiring :: ChangeEnable -> Maybe String
expiring (Expire r _) = Just r
expiring _ = Nothing

deleting :: ChangeEnable -> Maybe String
deleting (Delete r True) = Just r
deleting _ = Nothing

updateState :: [ChangeEnable] -> RepoState -> RepoState
updateState [] rs = rs
updateState (ce:ces) re@(repo,(enabled,file)) =
  case ce of
    Disable r True | r == repo && enabled -> (repo,(False,file))
    Enable r True | r == repo && not enabled -> (repo,(True,file))
    Only r True | r == repo && not enabled -> (repo,(True,file))
    Only r True | r /= repo && enabled -> (repo,(False,file))
    _ -> updateState ces re

selectRepo :: Bool -> [RepoState] -> [Mode] -> [ChangeEnable]
selectRepo exact repostates modes =
  nub $ foldl' selectRepo' [] (nub modes)
  where
    selectRepo' :: [ChangeEnable] -> Mode -> [ChangeEnable]
    selectRepo' acc mode =
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
                  let actNames = mapMaybe maybeRepoName results
                  in
                    if null actNames
                    then results
                    else
                      let base = head $ sortOn (length . snd) actNames
                      in
                        if all ((snd base `isPrefixOf`) . snd) actNames
                        then [fst base]
                        else results

    selectRepoMode :: Mode -> [ChangeEnable] -> RepoState
                   -> Maybe ChangeEnable
    selectRepoMode mode acc (name,(enabled,file)) =
      case mode of
        AddCopr repo _ _ ->
          maybeChange repo isSuffixOf (not enabled) (Enable name)
        AddKoji repo ->
          maybeChange repo isSuffixOf (not enabled) (Enable name)
        RepoURL url -> Just $ BaseURL url
        EnableRepo pat ->
          maybeChange pat matchesRepo (not enabled) (Enable name)
        DisableRepo pat ->
          maybeChange pat matchesRepo enabled (Disable name)
        OnlyRepo pat ->
          maybeChange pat matchesRepo (not enabled) (Only name)
        ExpireRepo pat ->
          maybeChange pat matchesRepo True (Expire name)
        ClearExpires -> Just UnExpire
        DeleteRepo pat ->
          maybeChange pat matchesRepo
          (not enabled || error ("disable repo before deleting:" +-+ name))
          (Delete file)
        Specific change ->
          let substr =  repoSubstr change in
            if change `elem`
               [EnableModular,EnableTesting,EnableDebuginfo,EnableSource]
            then
              maybeChange substr
              (\p n -> p `isInfixOf` n &&
                       repoStatus acc (removeInfix substr name))
              (not enabled) (Enable name)
            else
              maybeChange substr isInfixOf enabled (Disable name)
      where
        maybeChange :: String -> (String -> String -> Bool) -> Bool
                    -> (Bool -> ChangeEnable) -> Maybe ChangeEnable
        maybeChange pat matcher state change =
          if pat `matcher` name
          then
            if state
            then Just $ change True
            else
              if isGlob pat
              then Nothing
              else Just $ change False
          else Nothing

    repoStatus :: [ChangeEnable] -> String -> Bool
    repoStatus acc repo =
      case lookup repo repostates of
        Just (enabled,_) ->
          enabled || Enable repo True `elem` acc
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

-- adapted from simple-cmd
removeInfix :: String -> String-> String
removeInfix inf orig =
  maybe orig (uncurry (++)) $ stripInfix inf orig
