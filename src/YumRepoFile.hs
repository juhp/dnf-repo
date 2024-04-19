module YumRepoFile (
  readRepos,
  RepoState
  )
where

import Data.List.Extra (isPrefixOf, splitOn, trim)
import SimpleCmd (error', (+-+))

type RepoState = (String,(Bool,FilePath))

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
            case dropWhile (not . ("enabled" `isPrefixOf`)) rest of
              [] -> error' $ "no enabled field for" +-+ section
              (e:more') ->
                case splitOn "=" e of
                  [_,v] ->
                    case trim v of
                      "1" -> (True,more')
                      "0" -> (False,more')
                      _ -> error' $ "strange enabled state" +-+ e +-+ "for" +-+ section
                  _ -> error' $ "unknown enabled state" +-+ e +-+ "for" +-+ section
      in (section,(enabled,file)) : parseRepos file more
  where
    nextSection :: [String] -> Maybe (String,[String])
    nextSection [] = Nothing
    nextSection (l:ls') =
      case l of
        ('[' : rest) ->
          if last rest == ']'
          then Just (init rest, ls')
          else error' $ "bad section" +-+ l +-+ "in" +-+ file
        _ -> nextSection ls'
