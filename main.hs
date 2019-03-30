{-# LANGUAGE OverloadedStrings #-}
module Main where
import NginxParser
import System.Environment
import System.Exit
import Data.Either
import Data.Ord
import Data.Text (pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


instance ToRow NginxLog where
  toRow log = [SQLText $ pack $ hostname log
              ,SQLText $ pack $ ip_addr  log
              ,SQLText $ pack $ date     log
              ,SQLText $ pack $ method   log
              ,SQLText $ pack $ endpoint log
              ,SQLText $ pack $ protocol log
              ,SQLText $ pack $ status   log
              ]

instance FromRow NginxLog where
  fromRow = do
    NginxLog <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

createTableQuery = "CREATE TABLE IF NOT EXISTS nginxlogs\
                    \ (id INTEGER PRIMARY KEY, \
                    \  hostname text, \
                    \  ip_addr  text, \
                    \  date     text, \
                    \  method   text, \
                    \  endpoint text, \
                    \  protocol text, \
                    \  status   text  );"

main = do
  args <- getArgs
  if (length args) < 2
    then do putStrLn "Usage: nginxlogs.log sqlitedb.db"
            exitWith (ExitFailure 2)
    else do

  let filename = args !! 0
      dbname   = args !! 1

  -- filename -> [NginxLog]
  contents <- readFile filename
  let logLines = filter (not . null) $ lines contents
      logs = map parseLogLine logLines
      goodLogs' = rights logs

  let numFailed = (length $ lefts logs)
  do
    if  numFailed > 0
    then do putStrLn $ (show numFailed) ++
              " Log lines failed to parse..."
            mapM_ (putStrLn . show) (lefts logs)
    else return ()

  let numSuccess = (length $ rights logs)
  putStrLn ("Inserting " ++ (show numSuccess) ++ " into database")

  conn <- open dbname
  execute_ conn createTableQuery

  let insertLog log =
        execute conn "INSERT INTO nginxlogs \
          \(hostname,ip_addr,date,method,endpoint,protocol,status) \
          \VALUES (?,?,?,?,?,?,?)"
        log

  mapM_ insertLog goodLogs'

  --r <- query_ conn "SELECT * from nginxlogs limit 20" :: IO [NginxLog]
  --mapM_ print r

  close conn
  putStrLn "done!!!"
