
module Parser where

import Data.Char (isDigit)
import Data.Text
import qualified Data.List as List

data ParseResult a = Success a String | Error String deriving (Show)

data Parser a = Parser {runParser :: String -> ParseResult a}



parserA :: Parser Char
parserA = Parser inner where
  inner "" = Error "End of input"
  inner (first:rest) = 
    if first == 'a' then
      Success 'a' rest
    else
      Error "Expected 'a'"

satisfies :: (Char -> Bool) -> Parser Char
satisfies predicate = Parser inner where
  inner "" = Error "End of input"
  inner (first:rest) = 
    if predicate first then
      Success first rest
    else
      Error ("Unexpected character " ++ show first)


andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pa pb = Parser inner where
  inner "" = Error "End of input"
  inner input = 
    case runParser pa input of
      Success a rest -> 
        case runParser pb rest of
          Success b remaining -> Success (a,b) remaining
          Error err -> Error err
      Error err -> Error err

string :: String -> Parser String
string "" = Parser (\input -> Success "" input)
string (c:cs) = Parser inner where
  inner "" = Error "End of input"
  inner input =
    case runParser (andThen (char c) (string cs) ) input of
      Success (p, ps) rest -> Success (p:ps) rest
      Error err -> Error err

orElse :: Parser a -> Parser a -> Parser a
orElse pa pb = Parser inner where
  inner "" = Error "End of input"
  inner input = 
    case runParser pa input of
      Success a rest -> Success a rest 
      Error _ -> runParser pb input

many :: Parser a -> Parser [a]
many p = Parser inner where
  inner "" = Success [] ""
  inner input =
    case runParser p input of
      Success r rest -> 
        case runParser (many p) rest of
          Success rs remaining -> Success (r:rs) remaining
      Error _ -> Success [] input

some :: Parser a -> Parser [a]
some p = Parser inner where
  inner "" = Error "End of input"
  inner input =
    case runParser p input of
      Success r rest ->
        case runParser (many p) rest of
          Success rs remaining -> Success (r:rs) remaining
      Error err -> Error err

pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = Parser inner where
  inner "" = Error "End of input"
  inner input = 
    case runParser p input of
      Success r rest -> Success (f r) rest
      Error err -> Error err

alpha :: Parser Char
alpha = lower `orElse` upper

pThen :: Parser a -> Parser b -> Parser b
pThen pa pb = pMap snd (pa `andThen` pb)

number :: Parser Int
number = pMap read (some digit)

lower :: Parser Char
lower = satisfies (\c -> elem c ['a'..'z'])

upper :: Parser Char
upper = satisfies (\c -> elem c ['A'..'Z'])

char :: Char -> Parser Char
char c = satisfies (c ==)

digit :: Parser Char
digit = satisfies isDigit

