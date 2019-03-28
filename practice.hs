module Main where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import System.Environment
import Data.List

main = do
  args <- getArgs
  contents <- readFile $ args !! 0
  let logLines = filter (not . null) $ lines contents
      logs = map parseLogLine logLines
  mapM (putStrLn . show) logs


data NginxLog = NginxLog {
  hostname :: String,
  ip_addr :: String,
  date :: String,
  request :: String,
  status :: String
                         } deriving (Show)

parseLogLine :: String -> NginxLog
parseLogLine line = case (parse nginxParser "" line) of
  Left err -> NginxLog "fail" "fail" "fail" "fail" "fail"
  Right log -> log

nginxParser :: Parser NginxLog
nginxParser = do
  hostname <- pHostname
  ipAddr   <- pIpAddr
  pTwoDashes
  date     <- pDate
  request  <- pRequest
  status   <- pStatus
  return $ NginxLog hostname ipAddr date request status

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
  return "hello"

pTwoDashes :: Parser String
pTwoDashes = do
  return "hello"

pDate :: Parser String
pDate = do
  return "hello"

pRequest :: Parser String
pRequest = do
  return "hello"

pStatus :: Parser String
pStatus = do
  return "hello"

