import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

main = do
  args <- getArgs
  fileContents <- readFile $ head args
  let logLines = lines fileContents
      requests = map getRequestFromLogLine logLines
  putStrLn $ concat requests


getRequestFromLogLine :: String -> String
getRequestFromLogLine line =
  let w = words line
      wordsLines = map (\x -> x++"\n") w
      z = parse pHostname "comment" line
  in
    case z of
      Right a -> a
      Left a -> "hello"

pHostname :: GenParser Char st String
pHostname = do
  s <- many1 letter
  return s

number :: GenParser Char st Int
number = do
  nn <- many1 digit
  return (read nn :: Int)

number1 :: GenParser Char st String
number1 = many1 digit

threeNumbers :: GenParser Char st [Int]
threeNumbers = do
  n1 <- number
  spaces
  n2 <- number
  spaces
  n3 <- number
  spaces
  eof
  return [n1,n2,n3]

