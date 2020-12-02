import Utils

data Policy = Policy Char Int Int deriving (Eq,Show)

policyAndPassword :: Parser (Policy,String)
policyAndPassword = do
  min <- integer
  char '-'
  max <- integer
  space
  character <- letter
  string ": "
  password <- many letter
  return (Policy character min max, password)

inputFileParser :: Parser [(Policy,String)]
inputFileParser = do
  ps <- policyAndPassword `sepBy` endOfLine
  eof
  return ps

doesPolicyMatchPasswordPart1 :: (Policy, String) -> Bool
doesPolicyMatchPasswordPart1 ((Policy char min max), password) = numOccurences >= min && numOccurences <= max
  where numOccurences = length $ filter (== char) password

doesPolicyMatchPasswordPart2 :: (Policy, String) -> Bool
doesPolicyMatchPasswordPart2 ((Policy char fst snd), password) = fstChar /= sndChar && (fstChar == char || sndChar == char)
  where fstChar = password !! pred fst
        sndChar = password !! pred snd

part1 = do
  Right policiesAndPasswords <- parseFile inputFileParser "day2_input.txt"
  print . length $ filter doesPolicyMatchPasswordPart1 policiesAndPasswords

part2 = do
  Right policiesAndPasswords <- parseFile inputFileParser "day2_input.txt"
  print . length $ filter doesPolicyMatchPasswordPart2 policiesAndPasswords
