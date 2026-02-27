{-# LANGUAGE CPP #-}

module State (
  Mode(..),
  SpecificChange(..),
  ChangeEnable(Enable,Expire,UnExpire,Delete,TimeStamp),
  printAction,
  reduceOutput,
  selectRepo,
  changeRepo,
  saveRepo,
  updateState,
  expiring,
  deleting,
  PkgMgr(..),
  pkgMgrCmd
  )
where

--import Debug.Trace (traceShowId)
import Data.Either (partitionEithers)
import Data.List.Extra (dropPrefix, dropSuffix,
#if !MIN_VERSION_base(4,20,0)
                        foldl',
#endif
                        isPrefixOf, isInfixOf, isSuffixOf, nub,
                        replace, sortOn, stripInfix)
import Data.Maybe (mapMaybe)
import Safe (headMay, lastMay, tailSafe)
import SimpleCmd (error', (+-+))
import System.FilePath (takeBaseName)
import System.FilePath.Glob (compile, match)

import YumRepoFile (RepoState)

data Mode = AddCopr String (Maybe String) (Maybe String) | AddKoji String
          | AddRepo String (Maybe String) | RepoURL String
          | EnableRepo String | DisableRepo String | OnlyRepo String
          | ExpireRepo String | ClearExpires
          | DeleteRepo String | TimeStampRepo String
          | Specific SpecificChange
  deriving (Eq, Ord, Show)

modePattern :: Mode -> Maybe String
modePattern (AddCopr c _ _) = Just c
modePattern (AddKoji k) = Just k
modePattern (AddRepo r _) = Just r
modePattern (RepoURL _) = Nothing
modePattern (EnableRepo r) = Just r
modePattern (DisableRepo r) = Just r
modePattern (OnlyRepo r) = Just r
modePattern (ExpireRepo r) = Just r
modePattern ClearExpires = Nothing
modePattern (DeleteRepo r) = Just r
modePattern (TimeStampRepo r) = Just r
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
                  | TimeStamp String (Maybe String) Bool
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
printAction _ (TimeStamp {}) = Nothing
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
maybeRepoName t@(TimeStamp r _ _) = Just (t, r)
maybeRepoName UnExpire = Nothing
maybeRepoName (Delete _ _) = Nothing
maybeRepoName (BaseURL _) = Nothing

changeRepo :: ChangeEnable -> Maybe String
changeRepo (Disable r True) = Just $ "--disablerepo=" ++ r
changeRepo (Enable r True) = Just $ "--enablerepo=" ++ r
changeRepo (Only r True) = Just $ "--repo=" ++ r
changeRepo (Expire r True) = Just $ "--enablerepo=" ++ r
changeRepo (TimeStamp r _ True) = Just $ "--enablerepo=" ++ r
changeRepo (BaseURL url) = Just $ "--repofrompath=" ++ repoUrlName ++ "," ++ url
  where
    repoUrlName = replace "/" ":" $
                  dropSuffix "/" $
                  dropPrefix "http://" $
                  dropPrefix "https://" url
changeRepo _ = Nothing

data PkgMgr = Dnf5 | Dnf4

pkgMgrCmd :: PkgMgr -> String
pkgMgrCmd Dnf5 = "dnf5"
pkgMgrCmd Dnf4 = "dnf-3"

saveRepo :: Maybe PkgMgr -> ChangeEnable -> [String]
saveRepo pkgmgr (Disable r True) =
  case pkgmgr of
    Just dnf ->
      case dnf of
        Dnf4 -> ["--disable", r]
        Dnf5 -> ["setopt", r ++ ".enabled=0"]
    Nothing -> ["/^\\[" ++ r  ++ "\\]/,/^\\[/ s/^enabled=1/enabled=0/"]
saveRepo pkgmgr (Enable r True) =
  case pkgmgr of
    Just dnf ->
      case dnf of
        Dnf4 -> ["--enable", r]
        Dnf5 -> ["setopt", r ++ ".enabled=1"]
    Nothing -> ["/^\\[" ++ r  ++ "\\]/,/^\\[/ s/^enabled=0/enabled=1/"]
saveRepo _ _ = []

expiring :: ChangeEnable -> Maybe String
expiring (Expire r _) = Just r
expiring _ = Nothing

deleting :: ChangeEnable -> Maybe String
deleting (Delete r True) = Just r
deleting _ = Nothing

updateState :: [ChangeEnable] -> RepoState -> RepoState
updateState [] rs = rs
updateState (ce:ces) re@(repo,(enabled,file,url)) =
  case ce of
    Disable r True | r == repo && enabled -> (repo,(False,file,url))
    Enable r True | r == repo && not enabled -> (repo,(True,file,url))
    Only r True | r == repo && not enabled -> (repo,(True,file,url))
    Only r True | r /= repo && enabled -> (repo,(False,file,url))
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
                    case sortOn (length . snd) actNames of
                      [] -> results
                      (an:_ans) ->
                        if all ((snd an `isPrefixOf`) . snd) actNames
                        then [fst an]
                        else results

    selectRepoMode :: Mode -> [ChangeEnable] -> RepoState
                   -> Maybe ChangeEnable
    selectRepoMode mode acc (name,(enabled,file,murl)) =
      case mode of
        AddCopr repo _ _ ->
          maybeChange repo isSuffixOf (not enabled) False (Enable name)
        AddKoji repo ->
          maybeChange repo isSuffixOf (not enabled) False (Enable name)
        AddRepo repo _ ->
          maybeChange (takeBaseName repo) isSuffixOf (not enabled) False (Enable name)
        RepoURL url -> Just $ BaseURL url
        EnableRepo pat ->
          maybeChange pat matchesRepo (not enabled) False (Enable name)
        DisableRepo pat ->
          maybeChange pat matchesRepo enabled False (Disable name)
        OnlyRepo pat ->
          maybeChange pat matchesRepo (not enabled) True (Only name)
        ExpireRepo pat ->
          maybeChange pat matchesRepo True False (Expire name)
        ClearExpires -> Just UnExpire
        DeleteRepo pat ->
          maybeChange pat matchesRepo
          (not enabled || error ("disable repo before deleting:" +-+ name))
          False
          (Delete file)
        TimeStampRepo pat ->
          maybeChange pat matchesRepo (not enabled) False (TimeStamp name murl)
        Specific change ->
          let substr =  repoSubstr change in
            if change `elem`
               [EnableModular,EnableTesting,EnableDebuginfo,EnableSource]
            then
              maybeChange substr
              (\p n -> p `isInfixOf` n &&
                       repoStatus acc (removeInfix substr name))
              (not enabled) False (Enable name)
            else
              maybeChange substr isInfixOf enabled False (Disable name)
      where
        maybeChange :: String -> (String -> String -> Bool) -> Bool -> Bool
                    -> (Bool -> ChangeEnable) -> Maybe ChangeEnable
        maybeChange pat matcher state always change =
          if pat `matcher` name
          then
            if always
            then Just $ change state
            else
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
        Just (enabled,_,_) ->
          enabled || Enable repo True `elem` acc
        Nothing -> False

    isGlob :: String -> Bool
    isGlob pat = any (`elem` pat) "*?["

    matchesRepo :: String -> String -> Bool
    matchesRepo "" = error' "empty repo pattern"
    matchesRepo pat
      | isGlob pat =
        (match . compile) $
        if exact
        then pat
        else
          case startEnd pat of
            (True,True) -> init (tailSafe pat)
            (True,False) -> tailSafe pat ++ ['*' | last pat /= '*']
            (False,True) -> ['*' | headMay pat /= Just '*'] ++ init pat
            _ -> ['*' | headMay pat /= Just '*'] ++ pat ++ ['*' | lastMay pat /= Just '*']
      | exact = (pat ==)
      | otherwise =
          case startEnd pat of
            (True,True) -> (init (tailSafe pat) ==)
            (True,False) -> (tailSafe pat `isPrefixOf`)
            (False,True) -> (init pat `isSuffixOf`)
            _ -> (pat `isInfixOf`)

    startEnd :: String -> (Bool,Bool)
    startEnd pat = (headMay pat == Just '^', lastMay pat == Just '$')

-- adapted from simple-cmd
removeInfix :: String -> String-> String
removeInfix inf orig =
  maybe orig (uncurry (++)) $ stripInfix inf orig
