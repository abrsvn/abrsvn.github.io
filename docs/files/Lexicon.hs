module Lexicon where

import Data.List
import Data.Char (toLower)

data Feat = Masc | Fem | Neutr | MascOrFem | Sg | Pl | Fst | Snd | Thrd | Nom | AccOrDat | Pers | Refl | Wh | Tense | Infl | On | With | By | To | From deriving (Eq,Show,Ord,Enum,Bounded)

features :: [Feat]
features = [minBound..maxBound]

type Agreement = [Feat]

gender, number, person, gcase, pronType, tense, prepType :: Agreement -> Agreement
gender = filter (`elem` [MascOrFem,Masc,Fem,Neutr])
number = filter (`elem` [Sg,Pl])
person = filter (`elem` [Fst,Snd,Thrd])
gcase = filter (`elem` [Nom,AccOrDat])
pronType = filter (`elem` [Pers,Refl,Wh])
tense = filter (`elem` [Tense,Infl])
prepType = filter (`elem` [On,With,By,To,From])

prune :: Agreement -> Agreement
prune fs = if (Masc `elem` fs || Fem `elem` fs) then (delete MascOrFem fs) else fs

type CatLabel = String
type Phon = String

data Cat = Cat Phon CatLabel Agreement [Cat] deriving Eq

instance Show Cat where
    show (Cat phon label agr subcatlist) = "\"" ++ phon ++ "\"" ++ " " ++ label ++ show agr
    -- show (Cat phon label agr subcatlist) = "\"" ++ phon ++ "\"" ++ " " ++ label ++ show agr ++ " [" ++ intercalate " " [show subcat | subcat <- subcatlist] ++ "]"

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 = [feats | length (gender feats) <= 1, length (number feats) <= 1, length (person feats) <= 1, length (gcase feats) <= 1, length (pronType feats) <= 1, length (tense feats) <= 1, length (prepType feats) <= 1]
    where feats = prune . nub . sort $ fs cat1 ++ fs cat2

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not . null $ combine cat1 cat2

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) = [Cat phon label fs' subcatlist | fs' <- combine c (Cat "" "" [f] [])]

lexicon :: String -> [Cat]
lexicon "i" = [Cat "i" "NP" [Pers,Fst,Sg,Nom] []]
lexicon "me" = [Cat "me" "NP" [Pers,Fst,Sg,AccOrDat] []]
lexicon "we" = [Cat "we" "NP" [Pers,Fst,Pl,Nom] []]
lexicon "us" = [Cat "us" "NP" [Pers,Fst,Pl,AccOrDat] []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd] []]
lexicon "he" = [Cat "he" "NP" [Pers,Thrd,Sg,Nom,Masc] []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem] []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] []]
lexicon "it" = [Cat "it" "NP" [Pers,Thrd,Sg,Neutr] []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom] []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] []]

lexicon "myself" = [Cat "myself" "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves" = [Cat "ourselves" "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself" = [Cat "yourself" "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself" = [Cat "himself" "NP" [Refl,Sg,Thrd,AccOrDat,Masc] []]
lexicon "herself" = [Cat "herself" "NP" [Refl,Sg,Thrd,AccOrDat,Fem] []]
lexicon "itself" = [Cat "itself" "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]

lexicon "who" = [Cat "who" "NP" [Wh,Thrd,MascOrFem] [], Cat "who" "REL" [MascOrFem] []]
lexicon "whom" = [Cat "whom" "NP" [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], Cat "whom" "REL" [Sg,MascOrFem,AccOrDat] []]
lexicon "what" = [Cat "what" "NP" [Wh,Thrd,AccOrDat,Neutr] []]
lexicon "that" = [Cat "that" "REL" [] [], Cat "that" "DET" [Sg] []]
lexicon "which" = [Cat "which" "REL" [Neutr] [], Cat "which" "DET" [Wh] []]

lexicon "snowwhite" = [Cat "snowwhite" "NP" [Thrd,Fem,Sg] []]
lexicon "alice" = [Cat "alice" "NP" [Thrd,Fem,Sg] []]
lexicon "dorothy" = [Cat "dorothy" "NP" [Thrd,Fem,Sg] []]
lexicon "goldilocks" = [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
lexicon "littlemook" = [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu" = [Cat "atreyu" "NP" [Thrd,Masc,Sg] []]

lexicon "every" = [Cat "every" "DET" [Sg] []]
lexicon "all" = [Cat "all" "DET" [Pl] []]
lexicon "some" = [Cat "some" "DET" [] []]
lexicon "several" = [Cat "several" "DET" [Pl] []]
lexicon "a" = [Cat "a" "DET" [Sg] []]
lexicon "no" = [Cat "no" "DET" [] []]
lexicon "the" = [Cat "the" "DET" [] []]

lexicon "most" = [Cat "most" "DET" [Pl] []]
lexicon "many" = [Cat "many" "DET" [Pl] []]
lexicon "few" = [Cat "few" "DET" [Pl] []]
lexicon "this" = [Cat "this" "DET" [Sg] []]
lexicon "these" = [Cat "these" "DET" [Pl] []]
lexicon "those" = [Cat "those" "DET" [Pl] []]

lexicon "less_than" = [Cat "less_than" "NumMod" [Pl] []]
lexicon "more_than" = [Cat "more_than" "NumMod" [Pl] []]

lexicon "thing" = [Cat "thing" "CN" [Sg,Neutr,Thrd] []]
lexicon "things" = [Cat "things" "CN" [Pl,Neutr,Thrd] []]
lexicon "person" = [Cat "person" "CN" [Sg,Masc,Thrd] []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd] []]
lexicon "boy" = [Cat "boy" "CN" [Sg,Masc,Thrd] []]
lexicon "boys" = [Cat "boys" "CN" [Pl,Masc,Thrd] []]
lexicon "man" = [Cat "man" "CN" [Sg,Masc,Thrd] []]
lexicon "men" = [Cat "men" "CN" [Pl,Masc,Thrd] []]
lexicon "girl" = [Cat "girl" "CN" [Sg,Fem,Thrd] []]
lexicon "girls" = [Cat "girls" "CN" [Pl,Fem,Thrd] []]
lexicon "woman" = [Cat "woman" "CN" [Sg,Fem,Thrd] []]
lexicon "women" = [Cat "women" "CN" [Pl,Fem,Thrd] []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd] []]
lexicon "dwarf" = [Cat "dwarf" "CN" [Sg,Masc,Thrd] []]
lexicon "dwarfs" = [Cat "dwarfs" "CN" [Pl,Masc,Thrd] []]
lexicon "dwarves" = [Cat "dwarves" "CN" [Pl,Masc,Thrd] []]
lexicon "giant" = [Cat "giant" "CN" [Sg,Masc,Thrd] []]
lexicon "giants" = [Cat "giants" "CN" [Pl,Masc,Thrd] []]

lexicon "wizard" = [Cat "wizard" "CN" [Sg,Masc,Thrd] []]
lexicon "wizards" = [Cat "wizards" "CN" [Pl,Masc,Thrd] []]
lexicon "sword" = [Cat "sword" "CN" [Sg,Neutr,Thrd] []]
lexicon "swords" = [Cat "swords" "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger" = [Cat "dagger" "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers" = [Cat "daggers" "CN" [Pl,Neutr,Thrd] []]

lexicon "did" = [Cat "did" "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled" = [Cat "smiled" "VP" [Tense] []]
lexicon "smile" = [Cat "smile" "VP" [Infl] []]
lexicon "laughed" = [Cat "laughed" "VP" [Tense] []]
lexicon "laugh" = [Cat "laugh" "VP" [Infl] []]
lexicon "cheered" = [Cat "cheered" "VP" [Tense] []]
lexicon "cheer" = [Cat "cheer" "VP" [Infl] []]
lexicon "shuddered" = [Cat "shuddered" "VP" [Tense] []]
lexicon "shudder" = [Cat "shudder" "VP" [Infl] []]

lexicon "loved" = [Cat "loved" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "love" = [Cat "love" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]
lexicon "admired" = [Cat "admired" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "admire" = [Cat "admire" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]

lexicon "helped" = [Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "help" = [Cat "help" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]
lexicon "defeated" = [Cat "defeated" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "defeat" = [Cat "defeat" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]

lexicon "gave" = [Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "NP" [AccOrDat] []]]
lexicon "give" = [Cat "give" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "give" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "NP" [AccOrDat] []]]
lexicon "sold" = [Cat "sold" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "sold" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "NP" [AccOrDat] []]]
lexicon "sell" = [Cat "sell" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "sell" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "NP" [AccOrDat] []]]

lexicon "kicked" = [Cat "kicked" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [With] []], Cat "kicked" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "kick" = [Cat "kick" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [With] []], Cat "kick" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]

lexicon "took" = [Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [From] []], Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]]
lexicon "take" = [Cat "take" "VP" [Infl] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [From] []], Cat "take" "VP" [Infl] [Cat "" "NP" [AccOrDat] []]]

lexicon "on" = [Cat "on" "PREP" [On] []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "by" = [Cat "by" "PREP" [By] []]
lexicon "to" = [Cat "to" "PREP" [To] []]
lexicon "from" = [Cat "from" "PREP" [From] []]

lexicon "and" = [Cat "and" "CONJ" [] []]
lexicon "." = [Cat "." "CONJ" [] []]
lexicon "if" = [Cat "if" "COND" [] []]
lexicon "then" = [Cat "then" "THEN" [] []]

lexicon _ = []


type Words = [String]

lexer :: String -> Words
lexer = preproc . words . (map toLower) . scan

scan :: String -> String
scan []                      = []
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

preproc :: Words -> Words
preproc [] = []
preproc ["."] = []
preproc ["?"] = []
preproc (",":xs) = preproc xs

preproc ("did":"not":xs) = "didn't" : preproc xs
preproc ("nothing":xs) = "no" : "thing" : preproc xs
preproc ("nobody":xs) = "no" : "person" : preproc xs
preproc ("something":xs) = "some" : "thing" : preproc xs
preproc ("somebody":xs) = "some" : "person" : preproc xs
preproc ("everything":xs) = "every" : "thing" : preproc xs
preproc ("everybody":xs) = "every" : "person" : preproc xs
preproc ("less":"than":xs) = "less_than" : preproc xs
preproc ("more":"than":xs) = "more_than" : preproc xs
preproc ("at":"least":xs) = "at_least" : preproc xs
preproc ("at":"most":xs) = "at_most" : preproc xs
preproc (x:xs) = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = [c | c <- db w]

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words =
    let listing = map (\ x -> (x,lookupWord db x)) words
        unknown = map fst (filter (null.snd) listing)
    in  if unknown /= [] then error ("unknown words: " ++ show unknown)
        else initCats (map snd listing)

initCats :: [[Cat]] -> [[Cat]]
initCats [] = [[]]
initCats (cs:rests) = [c:rest | c <- cs, rest <- initCats rests]
