{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra
import Data.Bifunctor (bimap)
import Data.List.Extra
import Data.Maybe (mapMaybe)
import SimpleCmd
import SimpleCmdArgs
import System.Directory
import System.FilePath
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.IO.Extra (withTempDir)
import System.Time.Extra (sleep)

import Paths_dnf_repo (getDataFileName, version)
import ExpireRepos (expireRepos)
import Sudo
import YumRepoFile

main :: IO ()
main = do
  simpleCmdArgs' (Just version)
    "DNF wrapper repo tool"
    "see https://github.com/juhp/dnf-repo#readme" $
    runMain
    <$> switchWith 'n' "dryrun" "Dry run"
    <*> switchWith 'D' "debug" "Debug output"
    <*> switchWith 's' "save" "Save the repo enable/disable state"
    <*> modeOpt
    <*> optional testingOpt
    <*> optional modularOpt
    <*> many (strArg "[REPOPAT] ARGS")
  where
    modeOpt =
      AddCopr <$> strOptionWith 'c' "add-copr" "COPR" "Create repo file for copr repo" <|>
      DisableRepo <$> strOptionWith 'd' "disable" "REPOPAT" "Disable repos" <|>
      EnableRepo <$> strOptionWith 'e' "enable" "REPOPAT" "Enable repos" <|>
      ExpireRepo <$> strOptionWith 'x' "expire" "REPOPAT" "Expire repo cache" <|>
      pure Default

    testingOpt =
      flagWith' EnableTesting 't' "enable-testing" "Enable testing repos" <|>
      flagWith' DisableTesting 'T' "disable-testing" "Disable testing repos"

    modularOpt =
      flagWith' EnableModular 'm' "enable-modular" "Enable modular repos" <|>
      flagWith' DisableModular 'M' "disable-modular" "Disable modular repos"

coprRepoTemplate :: FilePath
coprRepoTemplate = "_copr:copr.fedorainfracloud.org:OWNER:REPO.repo"

-- FIXME both enabling and disabled at the same time
-- FIXME --enable-all-coprs (for updating etc)
-- FIXME confirm repos
-- FIXME --disable-non-cores (modular,testing,cisco, etc)
runMain :: Bool -> Bool -> Bool -> Mode -> Maybe Testing -> Maybe Modular
        -> [String] -> IO ()
runMain dryrun debug save mode mtesting mmodular args = do
  hSetBuffering stdout NoBuffering
  withCurrentDirectory "/etc/yum.repos.d" $ do
    case mode of
      AddCopr copr -> addCoprRepo copr
      _ -> return ()
    repofiles <- filesWithExtension "." "repo"
--    when debug $ print repofiles
    -- if null args
    --   then do
    --   repos <- getRepos Nothing
    --   unless (null repos) $ do
    --     putStrLn "Available repos:"
    --     mapM_ putStrLn repos
    --   else do
    nameStates <- sort <$> mapM readRepo repofiles
--        readRepoNames debug (mode == Disable) mtesting mmodular repofiles
    let repoActs = mapMaybe (selectRepo debug mode mtesting mmodular) nameStates
    unless (null repoActs) $ do
      mapM_ print repoActs
      putStrLn ""
    case mode of
      ExpireRepo _ -> do
        putStrLn ""
        expireRepos dryrun $ mapMaybe expiring repoActs
      _ -> return ()
    when save $
      if null repoActs
        then putStrLn "no changes to save\n"
        else do
        putStr "Press Enter to save repo enabled state:"
        void getLine
        doSudo dryrun "dnf" $
          "config-manager" :
          concatMap saveRepo repoActs
        putStrLn ""
    if null args
      then do
      listRepos $ map (updateState repoActs) nameStates
      else do
      sleep 1
      putStrLn ""
      let repoargs = concatMap changeRepo repoActs
        in doSudo dryrun "dnf" $ repoargs ++ args
    where
      -- FIXME pull non-fedora copr repo file
      -- FIXME delete created copr repo file if repo doesn't exist
      addCoprRepo :: String -> IO ()
      addCoprRepo repo = do
        case stripInfix "/" repo of
          Nothing -> error' $ "invalid copr: " ++ repo
          Just (copr_owner,copr_repo) -> do
            template <- getDataFileName coprRepoTemplate
            repodef <- cmd "sed" ["-e", "s/@COPR_OWNER@/" ++ copr_owner ++ "/g", "-e", "s/@COPR_REPO@/" ++ copr_repo ++ "/g", template]
            let repofile = replace "OWNER" copr_owner $
                           replace "REPO" copr_repo coprRepoTemplate
            exists <- doesFileExist repofile
            if exists
              then error' $ "repo already defined: " ++ repofile
              else putStrLn $ "Setting up copr repo " ++ repo
            withTempDir $ \ tmpdir -> do
              let tmpfile = tmpdir </> repofile
              unless dryrun $ writeFile tmpfile repodef
              doSudo dryrun "cp" [tmpfile, repofile]

      listRepos :: [RepoState] -> IO ()
      listRepos repoStates = do
        let (on,off) =
              -- can't this be simplified?
              bimap (map fst) (map fst) $ partition snd repoStates
        putStrLn "Enabled:"
        mapM_ putStrLn on
        putStrLn ""
        putStrLn "Disabled:"
        mapM_ putStrLn off

        -- getRepos :: IO [String]
        -- getRepos =
          -- files <-

          -- when debug $ print files
          -- return . sort $
          --   case mrepo of
          --     Nothing ->
          --       filter (selectTest (mode == Disable) mtesting) $
          --       filter (selectModular (mode == Disable) mmodular) files
          --     Just repo ->
          --       filter (replace "/" ":" repo `isInfixOf`) files

#if !MIN_VERSION_simple_cmd(0,2,4)
filesWithExtension :: FilePath -> String -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir
#endif
