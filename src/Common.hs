module Common (
  getSysArch
  )
where

import SimpleCmd (cmd)

getSysArch :: IO String
getSysArch =
  cmd "rpm" ["--eval", "%{_arch}"]
