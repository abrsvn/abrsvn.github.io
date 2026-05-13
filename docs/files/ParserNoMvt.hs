module ParserNoMvt where

import Data.List
import Data.Char
import BasicDef
import ParserCombinators
import Lexicon
import Tree2Tex

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

agreeC :: ParseTree Cat Cat -> ParseTree Cat Cat -> Bool
agreeC t1 t2 = agree (t2c t1) (t2c t2)

leafP :: CatLabel -> PARSER Cat Cat
leafP label []     = []
leafP label (c:cs) = [(Leaf c,cs) | catLabel c == label ]

assignT :: Feat ->  ParseTree Cat Cat
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]

sRule :: PARSER Cat Cat
sRule = \ xs ->
       [(Branch (Cat "" "S" [] []) [np',vp],zs) |
        (np,ys) <- parseNP xs,
        (vp,zs) <- parseVP ys,
        np' <- assignT Nom np,
        agreeC np vp,
        subcatList (t2c vp) == []]

parseSent :: PARSER Cat Cat
parseSent = sRule

npRule :: PARSER Cat Cat
npRule = \ xs ->
  [(Branch (Cat "" "NP" fs []) [det,cn],zs) |
   (det,ys) <- parseDET xs,
   (cn,zs) <- parseCN  ys,
   fs <- combine (t2c det) (t2c cn),
   agreeC det cn]

parseNP :: PARSER Cat Cat
parseNP = leafP "NP" <|> npRule

ppRule :: PARSER Cat Cat
ppRule = \ xs ->
   [(Branch (Cat "" "PP" fs []) [prep,np'],zs) |
    (prep,ys) <- parsePrep xs,
    (np,zs) <- parseNP ys,
    np' <- assignT AccOrDat np,
    fs <- combine (t2c prep) (t2c np')]

parsePP :: PARSER Cat Cat
parsePP = ppRule

parseNPorPP :: PARSER Cat Cat
parseNPorPP = parseNP <|> parsePP

parseNPsorPPs :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNPsorPPs = many parseNPorPP

parseDET :: PARSER Cat Cat
parseDET = leafP "DET"

parseCN :: PARSER Cat Cat
parseCN = leafP "CN"

parsePrep :: PARSER Cat Cat
parsePrep = leafP "PREP"

parseAux :: PARSER Cat Cat
parseAux = leafP "AUX"

parseVP :: PARSER Cat Cat
parseVP = finVpRule <|> auxVpRule

vpRule :: PARSER Cat Cat
vpRule = \xs ->
    [(Branch (Cat "" "VP" (fs (t2c vp)) []) (vp:xps),zs) |
     (vp,ys) <- leafP "VP" xs,
     subcatlist <- [subcatList (t2c vp)],
     (xps,zs) <- parseNPsorPPs ys,
     match subcatlist (map t2c xps)]

match :: [Cat] -> [Cat] -> Bool
match [] [] = True
match _  [] = False
match [] _ = False
match (x:xs) (y:ys) = catLabel x == catLabel y && agree x y && match xs ys

finVpRule :: PARSER Cat Cat
finVpRule = \xs -> [(vp',ys) | (vp,ys) <- vpRule xs, vp' <- assignT Tense vp]

auxVpRule :: PARSER Cat Cat
auxVpRule = \xs ->
    [(Branch (Cat "" "VP" (fs (t2c aux)) []) [aux,inf'],zs) |
     (aux,ys) <- parseAux xs,
     (inf,zs) <- vpRule ys,
     inf' <- assignT Infl inf]

prs :: String -> [ParseTree Cat Cat]
prs string = let ws = lexer string
     in [s | catlist <- collectCats lexicon ws, (s,[]) <- parseSent catlist]
