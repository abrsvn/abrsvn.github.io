module ParserCombinatorsMini where

import BasicDef
import Data.List
import Data.Char

type ParseState partialParse remainingInput = [(partialParse,[remainingInput])]
type Parser input parse = [input] -> ParseState parse input

succeed :: parse -> Parser input parse
succeed finalParse remainingInputs = [(finalParse,remainingInputs)]

failp :: Parser input parse
failp remainingInputs = []

symbol :: Eq terminal => terminal -> Parser terminal terminal
symbol t []                 = []
symbol t (x:xs) | t == x    = [(t,xs)]
                | otherwise = []

token :: Eq terminal => [terminal] -> Parser terminal [terminal]
token ts xs | ts == take n xs   = [(ts,drop n xs)]
            | otherwise         = []
         where n = length ts

satisfy :: (input -> Bool) -> Parser input input
satisfy p []                 = []
satisfy p (i:is) | p i       = [(i,is)]
                 | otherwise = []

digit :: Parser Char Char
digit = satisfy isDigit

just :: Parser input parse -> Parser input parse
just p = filter (null.snd) . p

infixr 4 <|>
(<|>) :: Parser input parse -> Parser input parse -> Parser input parse
(p1 <|> p2) xs = p1 xs ++ p2 xs

(<*>) :: Parser input [parse] -> Parser input [parse] -> Parser input [parse]
(p <*> q) xs = [(r1 ++ r2,zs) | (r1,ys) <- p xs, (r2,zs) <- q ys]

infixl 7 <$>
(<$>) :: (input -> parse) -> Parser s input -> Parser s parse
(f <$> p) xs = [ (f x,ys) | (x,ys) <- p xs ]

type PARSER input category = Parser input (ParseTree category input)

epsilonT :: PARSER input category
epsilonT = succeed EmptyTree

symbolT :: Eq input => input -> PARSER input category
symbolT s = (\ x -> Leaf x) <$> symbol s

infixl 6 <:>
(<:>) :: Parser input category -> Parser input [category] -> Parser input [category]
(p <:> q) xs = [(r:rs,zs) | (r,ys)  <- p xs, (rs,zs) <- q ys]

collect :: [Parser input category] -> Parser input [category]
collect []     = succeed []
collect (p:ps) = p <:> collect ps

parseAs :: category -> [PARSER input category] -> PARSER input category
parseAs label ps = (\ xs -> Branch label xs) <$> collect ps

many :: Parser input category -> Parser input [category]
many p = (p <:> many p) <|> (succeed [])

parseManyAs :: category -> PARSER input category -> PARSER input category
parseManyAs l p = (\ xs -> Branch l xs) <$> many p
