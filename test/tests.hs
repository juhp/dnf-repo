import SimpleCmd
import System.IO

program :: [String] -> IO ()
program test = do
  cmdLog "dnf-repo" test
  putStrLn ""

tests :: [[String]]
tests =
  [["-t"]
  ,["-m"]
  ,["-e", "copr"]
  ,["-e", "openh264"]
  ,["-e", "rawhide"]
  ,["-d", "update"]
  ,["--enable-source"]
  ,["-d", "*", "--enable-source"]
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ "\n" ++ show (length tests) ++ " command tests run"
