module NginxParser where
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.Int (Int64)

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


-- PARSE --------------------------------------------------
parseLogLine :: String -> Either String NginxLog
parseLogLine line =
  case (parse nginxParser "" line) of
    Left err -> Left $ (show err) ++ "\n" ++ line
    Right log -> Right log

nginxParser :: Parser NginxLog
nginxParser = do
  hostname <- (pHostname <|> (string "_"))  <* whiteSpace
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
-- also check for underscore being the hostname "_"
pHostname :: Parser String
pHostname = do
  segments <- (sepBy1 (pSegment)  pSeparator)
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
