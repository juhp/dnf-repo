module YumRepoFile (
  readRepos,
  RepoState
  )
where

import Data.List.Extra (breakOn, isInfixOf, isPrefixOf, splitOn, trim, trimEnd)
import SimpleCmd (error', (+-+))

type RepoState = (String, -- reponame
                  (Bool, -- enabled
                   FilePath, -- repofile
                   Maybe String)) -- baseurl

readRepos :: FilePath -> IO [RepoState]
readRepos file =
  parseRepos file . lines <$> readFile file

-- parse ini
parseRepos :: FilePath -> [String] -> [RepoState]
parseRepos file ls =
  case nextSection ls of
    Nothing -> []
    Just (section,rest) ->
      let (mbaseurl,rest1) =
            case dropWhile (not . ("baseurl" `isInfixOf`)) rest of
              [] -> (Nothing,rest)
              (e:more') ->
                case breakOn "=" e of
                  (_,'=':url) -> (Just (trim url), more')
                  _ -> error' $ "failed to parse" +-+ show e +-+ "for" +-+ section
          (enabled,more) =
            case dropWhile (not . ("enabled" `isPrefixOf`)) rest1 of
              [] -> error' $ "no enabled field for" +-+ section
              (e:more') ->
                case splitOn "=" e of
                  [_,v] ->
                    case trim v of
                      "1" -> (True,more')
                      "0" -> (False,more')
                      _ -> error' $ "strange enabled state" +-+ e +-+ "for" +-+ section
                  _ -> error' $ "unknown enabled state" +-+ e +-+ "for" +-+ section
      in (section,(enabled,file,mbaseurl)) : parseRepos file more
  where
    nextSection :: [String] -> Maybe (String,[String])
    nextSection [] = Nothing
    nextSection (l:ls') =
      case trimEnd l of
        ('[' : rest) ->
          if last rest == ']'
          then Just (init rest, ls')
          else error' $ "bad section" +-+ l +-+ "in" +-+ file
        _ -> nextSection ls'
