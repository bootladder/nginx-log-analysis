{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import System.Environment
import System.Exit
import Data.List (intercalate)
import Data.Either
import Data.Ord
import Data.Int (Int64)
import Data.Text (pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow


data NginxLog = NginxLog {
  id       :: Int64,
  hostname :: String,
  ip_addr  :: String,
  date     :: String,
  method   :: String,
  endpoint :: String,
  protocol :: String,
  status   :: String
  } deriving (Show)

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
    then do putStrLn "Usage: access.log test.db"
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


-- PARSE --------------------------------------------------
parseLogLine :: String -> Either String NginxLog
parseLogLine line =
  case (parse nginxParser "" line) of
    Left err -> Left (show err)
    Right log -> Right log

nginxParser :: Parser NginxLog
nginxParser = do
  hostname <- pHostname <* whiteSpace
  ipAddr   <- pIpAddr   <* whiteSpace
  pTwoDashes            <* whiteSpace
  date     <- pDate     <* whiteSpace
  char '\"'
  method   <- pMethod   <* whiteSpace
  endpoint <- pEndpoint <* whiteSpace
  protocol <- pProtocol <* whiteSpace
  status   <- pStatus   <* whiteSpace
  return $ NginxLog 0 hostname ipAddr date method endpoint protocol status

whiteSpace = skipMany space

--https://gist.github.com/peat/2212696
pHostname :: Parser String
pHostname = do
  segments <- (sepBy1 pSegment pSeparator)
  return $ intercalate "." segments
  where
    pSegment   = do segment <- many1 (alphaNum <|> char '-')
                    if last segment == '-'
                      then fail "domain segment must not end with hyphen"
                      else return $ segment
    pSeparator = char '.'

pIpAddr :: Parser String
pIpAddr = do
  segments <- (sepBy (many digit) (char '.'))
  return $ intercalate "." segments

pTwoDashes = do {char '-'; whiteSpace; char '-'; return ()}
pDate      = do {char '['; manyTill anyChar (try (char ']'))}
pMethod    = do manyTill anyChar (try space)
pEndpoint  = do manyTill (anyChar <|> oneOf "/-_") (try space)
pProtocol  = do manyTill anyChar (try (char '\"'))
pStatus    = do many digit

