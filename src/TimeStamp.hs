module TimeStamp (
  timestampRepo
  )
where

import Control.Monad.Extra (whenJust)
import Data.List.Extra (replace)
import Network.HTTP.Directory (httpLastModified', (+/+))

import Common

-- FIXME doesn't work for release repos
-- FIXME matches commented out download.example
timestampRepo :: String -> String -> IO ()
timestampRepo release baseurl = do
  sysarch <- getSysArch
  let url = replace "$releasever" release $
            replace "$basearch" sysarch baseurl
  mtime <- httpLastModified' $ url +/+ "repodata/repomd.xml"
  whenJust mtime print
