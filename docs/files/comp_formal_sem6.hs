{- ghci 1 -}
:l Lexicon
{- ghci 2 -}
:i Feat
{- ghci 3 -}
features
{- ghci 4 -}
:i Agreement
{- ghci 5 -}
:t gender
{- ghci 6 -}
gender features
{- ghci 7 -}
number features
{- ghci 8 -}
person features
{- ghci 9 -}
gcase features
{- ghci 10 -}
pronType features
{- ghci 11 -}
tense features
{- ghci 12 -}
prepType features
{- ghci 13 -}
gender features
{- ghci 14 -}
prune $ gender features
{- ghci 15 -}
number features
{- ghci 16 -}
prune $ number features
{- ghci 17 -}
:i Cat
{- ghci 18 -}
:i Phon
{- ghci 19 -}
:i CatLabel
{- ghci 20 -}
:i Agreement
{- ghci 21 -}
:t Cat
{- ghci 22 -}
Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
{- ghci 23 -}
Cat "" "NP" [Thrd,Fem,Sg] []
{- ghci 24 -}
Cat "littlemook" "NP" [Thrd,Masc,Sg] []
{- ghci 25 -}
Cat "every" "DET" [Sg] []
{- ghci 26 -}
Cat "all" "DET" [Pl] []
{- ghci 27 -}
Cat "some" "DET" [] []
{- ghci 28 -}
Cat "several" "DET" [Pl] []
{- ghci 29 -}
Cat "a" "DET" [Sg] []
{- ghci 30 -}
Cat "did" "AUX" [] []
{- ghci 31 -}
Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
{- ghci 32 -}
Cat "and" "CONJ" [] []
{- ghci 33 -}
phon $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
{- ghci 34 -}
catLabel $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
{- ghci 35 -}
fs $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
{- ghci 36 -}
subcatList $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
{- ghci 37 -}
let { cat1 = Cat "goldilocks" "NP" [Thrd,Fem,Sg] [] ; cat2 = Cat "runs" "VP" [Tense,Sg] [] ; cat3 = Cat "run" "VP" [Tense,Pl] [] }
{- ghci 38 -}
combine cat1 cat2
{- ghci 39 -}
combine cat1 cat3
{- ghci 40 -}
agree cat1 cat2
{- ghci 41 -}
agree cat1 cat3
{- ghci 42 -}
assign Tense $ Cat "run" "VP" [Pl] []
{- ghci 43 -}
lexer "I loved her."
{- ghci 44 -}
lexer "She despised me."
{- ghci 45 -}
collectCats lexicon $ lexer "I loved her."
{- ghci 46 -}
collectCats lexicon $ lexer "She despised me."
{- ghci 47 -}
:l ParserNoMvt
{- ghci 48 -}
:t t2c
{- ghci 49 -}
:i ParseTree
{- ghci 50 -}
:t Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
{- ghci 51 -}
Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
{- ghci 52 -}
:t Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
{- ghci 53 -}
Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
{- ghci 54 -}
t2c $ Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
{- ghci 55 -}
Cat "runs" "VP" [Tense,Sg] []
{- ghci 56 -}
Leaf (Cat "runs" "VP" [Tense,Sg] [])
{- ghci 57 -}
t2c $ Leaf (Cat "runs" "VP" [Tense,Sg] [])
{- ghci 58 -}
:t Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
{- ghci 59 -}
Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
{- ghci 60 -}
t2c $ Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
{- ghci 61 -}
:t agreeC
{- ghci 62 -}
agreeC (Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])) (Leaf (Cat "runs" "VP" [Tense,Sg] []))
{- ghci 63 -}
agreeC (Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])) (Leaf (Cat "runs" "VP" [Tense,Pl] []))
{- ghci 64 -}
:t assignT
{- ghci 65 -}
assignT Nom $ Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
{- ghci 66 -}
assignT Sg $ Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
{- ghci 67 -}
:t leafP
{- ghci 68 -}
:i CatLabel
{- ghci 69 -}
:t leafP "NP"
{- ghci 70 -}
leafP "NP" [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
{- ghci 71 -}
leafP "NP" [Cat "runs" "VP" [Tense,Sg] []]
{- ghci 72 -}
:t leafP "VP"
{- ghci 73 -}
leafP "VP" [Cat "runs" "VP" [Tense,Sg] []]
{- ghci 74 -}
leafP "VP" [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
{- ghci 75 -}
:t parseSent
{- ghci 76 -}
:i PARSER
{- ghci 77 -}
parseSent $ [Cat "goldilocks" "NP" [Thrd,Fem,Sg] [], Cat "runs" "VP" [Tense,Sg] []]
{- ghci 78 -}
parseSent $ [Cat "goldilocks" "NP" [Thrd,Fem,Sg] [], Cat "runs" "VP" [Tense,Sg] [], Cat "quickly" "AdvP" [] []]
{- ghci 79 -}
parseNP [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
{- ghci 80 -}
parseNP [Cat "every" "DET" [Sg] [], Cat "princess" "CN" [Sg,Fem,Thrd] []]
{- ghci 81 -}
parseDET [Cat "every" "DET" [Sg] []]
{- ghci 82 -}
parseCN [Cat "princess" "CN" [Sg,Fem,Thrd] []]
{- ghci 83 -}
parsePrep [Cat "with" "PREP" [With] []]
{- ghci 84 -}
parsePP [Cat "with" "PREP" [With] [], Cat "every" "DET" [Sg] [], Cat "princess" "CN" [Sg,Fem,Thrd] []]
{- ghci 85 -}
vpRule [Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] []]
{- ghci 86 -}
vpRule [Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] [], Cat "to" "PREP" [To] [], Cat "alice" "NP" [Thrd,Fem,Sg] []]
{- ghci 87 -}
vpRule [Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] []]
{- ghci 88 -}
vpRule [Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] [], Cat "to" "PREP" [To] [], Cat "alice" "NP" [Thrd,Fem,Sg] []]
{- ghci 89 -}
parseAux [Cat "didn't" "AUX" [] []]
{- ghci 90 -}
auxVpRule [Cat "didn't" "AUX" [] [], Cat "smile" "VP" [Infl] []]
{- ghci 91 -}
auxVpRule [Cat "didn't" "AUX" [] [], Cat "smiled" "VP" [Tense] []]
{- ghci 92 -}
:l ParserNoMvt
{- ghci 93 -}
prs "I loved her."
{- ghci 94 -}
prs "I loved her." !! 0
{- ghci 95 -}
prs "She didn't love me."
{- ghci 96 -}
prs "She didn't love me." !! 0
{- ghci 97 -}
prs "She despised me."
{- ghci 98 -}
writeTree2Tex ((prs "I loved her.") !! 0)
{- ghci 99 -}
writeTree2Tex ((prs "She didn't love me.") !! 0)
{- ghci 100 -}
writeTree2Tex ((prs "The dwarf didn't defeat the giant.") !! 0)
