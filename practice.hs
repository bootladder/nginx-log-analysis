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
      goodLogs = filter (\log -> case log of
                            Left err -> False
                            Right a -> True
                        ) logs
      goodLogs' = map (\log -> case log of
                          Right a -> a) goodLogs
      hs = hostnames goodLogs'

  putStrLn $ "The hostnames are " ++ (show hs)
  let f h = putStrLn $ "Hits to " ++ h ++ " " ++ (show $ countHitsByHostname h goodLogs')
  mapM f hs

  let es = endpoints goodLogs'
  putStrLn $ "The Endpoints are : " ++ (show $ es)
  mapM (\e -> putStrLn $ "Hits to " ++ e ++ " " ++
         (show $ countHitsByEndpoint e goodLogs')) es

data NginxLog = NginxLog {
  hostname :: String,
  ip_addr  :: String,
  date     :: String,
  method   :: String,
  endpoint :: String,
  protocol :: String,
  status   :: String
                         } deriving (Show)

hostnames logs = getUniqueFields hostname logs
endpoints logs = getUniqueFields endpoint logs

getUniqueFields :: (a -> String) -> [a] -> [String]
getUniqueFields field records=
  foldl f [] records
  where f acc record =
          if (field record) `elem` acc
          then acc
          else acc ++ [field record]


countHitsByHostname :: String -> [NginxLog] -> Int
countHitsByHostname matcher logs =
  countMatchingFields hostname matcher logs

countHitsByEndpoint :: String -> [NginxLog] -> Int
countHitsByEndpoint matcher logs =
  countMatchingFields endpoint matcher logs

countMatchingFields :: (a -> String) -> String -> [a] -> Int
countMatchingFields field matcher records =
  foldl f 0 records
  where f acc record = if (field record) == matcher
                    then acc + 1
                    else acc

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
  return $ NginxLog hostname ipAddr date method endpoint protocol status

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

pDate = do
  char '['
  manyTill anyChar (try (char ']'))

pMethod   = do manyTill anyChar (try space)
pEndpoint = do manyTill (anyChar <|> oneOf "/-_") (try space)
pProtocol = do manyTill anyChar (try (char '\"'))
pStatus   = do many digit
