module Utils (
  module Control.Monad.Identity,
  module Text.Parsec,
  module Data.Maybe,
  Parser,
  parseFile,
  integer,
  safeLookup
  ) where

import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec

-- type definitions
type Parser a = ParsecT T.Text () Identity a

parseFile :: Parser a -> FilePath -> IO (Either ParseError a)
parseFile p fp = parse p fp <$> TIO.readFile fp

-- Tiny utility function because Text.Parsec.Number is apparently not in the stdlib
-- Little bit more sophisticated to also parse negative numbers
integer :: Parser Int
integer = read <$> (plus <|> minus <|> number)
  where plus   = char '+' *> number
        minus  = (:) <$> char '-' <*> number
        number = many1 digit

-- For looking up keys you know are there.
safeLookup key map = fromJust $ M.lookup key map
