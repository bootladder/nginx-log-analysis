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
  mapM (run simple) logLines
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

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
  Left err -> do
    putStr "Hello parse error at "
    print err
  Right x -> print x

simple :: Parser String
simple = do
  pHostname

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

openCloseParens :: Parser ()
openCloseParens = do
  char '('
  char ')'
  return ()

parens :: Parser ()
parens  = do
  char '('
  parens
  char ')'
  parens
  <|> return ()

testOr :: Parser ()
testOr = do
  char '('
  char 'a' <|> char 'b'
  char ')'
  return ()

testOr' :: Parser ()
testOr' = do
  try (string "(a)") <|> string "(b)"
  return ()

nesting :: Parser Int
nesting = do
  char '('
  n <- nesting
  char ')'
  m <- nesting
  return (max (n+1) m)
  <|> return 0

word :: Parser String
word = do {
  c <- letter;
  do
    cs <- word
    return (c:cs)
  <|> return [c]
  }

word' :: Parser String
word' = many1 letter <?> "word"

sentence :: Parser [String]
sentence = do
  words <- sepBy1 word separator
  oneOf ".?!" <?> "end of sentence"
  return words

separator :: Parser ()
separator = skipMany1 (space <|> char ',' <?> "")
