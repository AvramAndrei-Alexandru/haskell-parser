module MiniLisp where

import Parser
import Control.Monad

data LispAtom = Number Int | Symbol String deriving (Eq, Show)
data LispValue = List [LispValue] | Atom LispAtom deriving (Eq, Show)

ws :: Parser String
ws = orElse (many (char ' ')) (many (char '\n'))

between :: Parser a -> Parser b -> Parser c -> Parser c
between pL pR p = pL `pThen` pMap fst (p `andThen` pR)
  
ident :: Parser String
ident = pMap convertor (some alpha `andThen` many ((some alpha `orElse` some digit) `orElse` some (char '?')))

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = pMap merge (p `andThen` (many (sep `pThen` p)))

merge :: (a, [a]) -> [a]
merge tuple = (fst tuple) : (snd tuple)

lisp :: Parser LispValue
lisp = Parser inner where
    inner "" = Error "End of input"
    inner s = 
        case runParser lispList s of
            Success ok1 notOk1 -> Success (List  ok1) notOk1
            Error err ->
                case runParser lispAtom s of
                    Success ok2 notOk2 -> Success (Atom ok2) notOk2
                    Error err -> Error err


lispAtom :: Parser LispAtom
lispAtom = Parser inner where
    inner "" = Error "End of input"
    inner s = 
        case runParser (some digit) s of
            Success result rest -> Success (Number (read result)) rest
            Error err1 -> 
                case runParser ident2 s of
                    Success ok notOk -> Success (Symbol ok) notOk
                    Error err2 -> Error err2

lispList :: Parser [LispValue] 
lispList = Parser inner where
    inner "" = Error "End of input"
    inner s =
        case runParser (simplifiedBetween (many (some lisp `pThen2` ws))) s of
            Success ok notOk -> Success (concat ok) notOk
            Error err -> Error err

simplifiedBetween :: Parser c -> Parser c
simplifiedBetween p = between (char '(') (char ')') p

convertor :: ([Char], [[Char]]) -> String
convertor tuple =
    fst tuple ++ join (snd tuple)


pThen2 :: Parser a -> Parser b -> Parser a
pThen2 pa pb = pMap fst (pa `andThen` pb)


ident2 :: Parser String
ident2 = pMap convertor (some alpha `andThen` many (some alpha `orElse` some digit))