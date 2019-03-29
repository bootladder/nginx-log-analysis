
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

  let hs = hostnames goodLogs'
  putStrLn $ "The hostnames are " ++ (show hs)
  mapM (\h -> putStrLn $ "Hits to " ++ h ++ " " ++
         (show $ countHitsByHostname h goodLogs')) hs

  let es = endpoints goodLogs'
  putStrLn $ "The Endpoints are : " ++ (show es)
  mapM (\e -> putStrLn $ "Hits to " ++ e ++ " " ++
         (show $ countHitsByEndpoint e goodLogs')) es

  putStrLn $ "The top 10 Endpoints are : "
  let endpointsAndHits = map f es
        where f e = (e, countHitsByEndpoint e goodLogs')
      sortedEndpointsAndHits =
        reverse $ sortBy (comparing snd) endpointsAndHits
      top10 = take 10 sortedEndpointsAndHits

  putStrLn $ show top10

-- PROCESS ------------------------------------------------

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

countHitsByEndpoint matcher logs =
  countMatchingFields endpoint matcher logs

countMatchingFields :: (a -> String) -> String -> [a] -> Int
countMatchingFields field matcher records =
  foldl f 0 records
  where f acc record = if (field record) == matcher
                    then acc + 1
                    else acc
