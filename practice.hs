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
  hostname <- pHostname <* whiteSpace
  ipAddr   <- pIpAddr   <* whiteSpace
  pTwoDashes            <* whiteSpace
  date     <- pDate     <* whiteSpace
  request  <- pRequest  <* whiteSpace
  status   <- pStatus   <* whiteSpace
  return $ NginxLog hostname ipAddr date request status

whiteSpace :: Parser ()
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
  byte1 <- many digit
  char '.'
  byte2 <- many digit
  char '.'
  byte3 <- many digit
  char '.'
  byte4 <- many digit
  return $ byte1 ++ "." ++ byte2 ++ "." ++ byte3 ++ "." ++ byte4

pTwoDashes :: Parser ()
pTwoDashes = do
  char '-'
  whiteSpace
  char '-'
  return ()

pDate :: Parser String
pDate = do
  char '['
  manyTill anyChar (try (char ']'))

pRequest :: Parser String
pRequest = do
  char '\"'
  manyTill anyChar (try (char '\"'))

pStatus :: Parser String
pStatus = do
  many digit

