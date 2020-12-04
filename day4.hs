import Utils
import Data.Char
import qualified Data.Map as M
import Debug.Trace

passportElement :: Parser (String,String)
passportElement = do
  k <- many1 alphaNum
  char ':'
  v <- many1 (char '#' <|> alphaNum)
  return (k,v)

passport :: Parser (M.Map String String)
passport = do
  elements <- passportElement `endBy` space -- space matches both whitespace and newlines
  return $ foldr (\(k,v) m -> M.insert k v m) M.empty elements

passportFile :: Parser [M.Map String String]
passportFile = do
  passports <- passport `sepBy` endOfLine
  eof
  return passports

isPassportValidPart1 :: M.Map String String -> Bool
isPassportValidPart1 m = all (\k -> M.member k m) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isPassportValidPart2 :: M.Map String String -> Bool
isPassportValidPart2 m = all isJust [checkBYR m, checkIYR m, checkEYR m, checkHGT m, checkHCL m, checkECL m, checkPID m]

checkBYR :: M.Map String String -> Maybe ()
checkBYR m = do
  byr <- M.lookup "byr" m
  guard $ length byr == 4
  guard $ all isDigit byr
  guard $ let numericValue = (read byr :: Int) in numericValue >= 1920 && numericValue <= 2002
  return ()

checkIYR :: M.Map String String -> Maybe ()
checkIYR m = do
  iyr <- M.lookup "iyr" m
  guard $ length iyr == 4
  guard $ all isDigit iyr
  guard $ let numericValue = (read iyr :: Int) in numericValue >= 2010 && numericValue <= 2020

checkEYR :: M.Map String String -> Maybe ()
checkEYR m = do
  eyr <- M.lookup "eyr" m
  guard $ length eyr == 4
  guard $ all isDigit eyr
  guard $ let numericValue = (read eyr :: Int) in numericValue >= 2020 && numericValue <= 2030

checkHGT m = do
  hgt <- M.lookup "hgt" m
  guard $ case hgt of
    ['1',x,y,'c','m'] -> let numericValue = (read [x,y] :: Int) in numericValue >= 50 && numericValue <= 93
    [x,y,'i','n'] -> let numericValue = (read [x,y] :: Int) in numericValue >= 59 && numericValue <= 76
    _ -> False

checkHCL m = do
  hcl <- M.lookup "hcl" m
  guard $ head hcl == '#'
  guard $ all isHexDigit (tail hcl)

checkECL m = do
  ecl <- M.lookup "ecl" m
  guard $ ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

checkPID m = do
  pid <- M.lookup "pid" m
  guard $ length pid == 9
  guard $ all isDigit pid

part1 = do
  Right passports <- parseFile passportFile "day4_input.txt"
  print . length $ filter isPassportValidPart1 passports

part2 = do
  Right passports <- parseFile passportFile "day4_input.txt"
  print . length $ filter isPassportValidPart2 passports
