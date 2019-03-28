
run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
  Left err -> do
    putStr "Hello parse error at "
    print err
  Right x -> print x

simple :: Parser String
simple = do
  pHostname

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
