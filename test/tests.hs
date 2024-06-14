import SimpleCmd
import System.IO

program :: [String] -> IO ()
program test = do
  cmdLog "dnf-repo" test
  putStrLn ""

tests :: [[String]]
tests =
  [["-e", "updates", "-t"] -- updates to avoid rawhide failing
  ,["-e", "copr"]
  ,["-e", "openh264"]
  ,["-e", "rawhide"]
  ,["-d", "update"]
  ,["-o", "fedora"]
  ,["--enable-source"]
  ,["-d", "*", "--enable-source"]
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ "\n" ++ show (length tests) ++ " command tests run"
