module Main where
import NginxParser
import System.Environment
import System.Exit
import Data.Either

main = do
  args <- getArgs
  if (length args) < 1
    then do putStrLn "Usage: access.log"
            exitWith (ExitFailure 2)
    else do

  let filename = args !! 0

  -- filename -> [NginxLog]
  contents <- readFile filename
  let logLines = filter (not . null) $ lines contents
      logs = map parseLogLine logLines
      goodLogs' = rights logs

  -- track number of failed and successful parsing
  let numFailed = (length $ lefts logs)
  do
    if  numFailed > 0
    then do putStrLn $ (show numFailed) ++
              " Log lines failed to parse..."
            mapM_ (putStrLn ) (lefts logs)
    else return ()

  let numSuccess = (length $ rights logs)
  putStrLn ("There are  " ++ (show numSuccess) ++ " good logs")
