-- PARSING AN ENG. FRAGMENT (no mvt)

:l ParserNoMvt

-- we first define 3 useful functions

-- 1. a function from trees to categories

:t t2c
:i ParseTree

:t Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
:t Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
t2c $ Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])

Cat "runs" "VP" [Tense,Sg] []
Leaf (Cat "runs" "VP" [Tense,Sg] [])
t2c $ Leaf (Cat "runs" "VP" [Tense,Sg] [])

:t Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]
t2c $ Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]

-- 2. a function that checks whether 2 trees agree

:t agreeC

agreeC (Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])) (Leaf (Cat "runs" "VP" [Tense,Sg] []))
agreeC (Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])) (Leaf (Cat "runs" "VP" [Tense,Pl] []))

-- 3. a function that assigns an agreement feature to a category

:t assignT

assignT Nom $ Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] [])
assignT Sg $ Branch (Cat "" "S" [] []) [Leaf (Cat "goldilocks" "NP" [Thrd,Fem,Sg] []), Leaf (Cat "runs" "VP" [Tense,Sg] [])]


-- we can now start defining our parser combinators (including the basic parsers)

-- we begin with a parser for leaves

:t leafP
:i CatLabel

:t leafP "NP"
leafP "NP" [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
leafP "NP" [Cat "runs" "VP" [Tense,Sg] []]

:t leafP "VP"
leafP "VP" [Cat "runs" "VP" [Tense,Sg] []]
leafP "VP" [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]

-- sentence parser

:t parseSent
:i PARSER

parseSent $ [Cat "goldilocks" "NP" [Thrd,Fem,Sg] [], Cat "runs" "VP" [Tense,Sg] []]
parseSent $ [Cat "goldilocks" "NP" [Thrd,Fem,Sg] [], Cat "runs" "VP" [Tense,Sg] [], Cat "quickly" "AdvP" [] []]

-- NP, DET, CN and PP parsers

parseNP [Cat "goldilocks" "NP" [Thrd,Fem,Sg] []]
parseNP [Cat "every" "DET" [Sg] [], Cat "princess" "CN" [Sg,Fem,Thrd] []]

parseDET [Cat "every" "DET" [Sg] []]
parseCN [Cat "princess" "CN" [Sg,Fem,Thrd] []]

parsePrep [Cat "with" "PREP" [With] []]

parsePP [Cat "with" "PREP" [With] [], Cat "every" "DET" [Sg] [], Cat "princess" "CN" [Sg,Fem,Thrd] []]

-- VP parser

-- VPs are assembled by means of a rule that parses a VP first and then check that the following items in the list of remaining inputs match the sub-categorization list of the VP; if they do, those items are subsumed under the VP branch

vpRule [Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] []]

vpRule [Cat "took" "VP" [Tense] [Cat "" "NP" [AccOrDat] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] [], Cat "to" "PREP" [To] [], Cat "alice" "NP" [Thrd,Fem,Sg] []]

vpRule [Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] []]

vpRule [Cat "gave" "VP" [Tense] [Cat "" "NP" [AccOrDat] [], Cat "" "PP" [To] []], Cat "a" "DET" [Sg] [], Cat "sword" "CN" [Sg,Neutr,Thrd] [], Cat "to" "PREP" [To] [], Cat "alice" "NP" [Thrd,Fem,Sg] []]

-- we also have a rule for finite VPs resulting from combining an auxiliary and a VP

parseAux [Cat "didn't" "AUX" [] []]

auxVpRule [Cat "didn't" "AUX" [] [], Cat "smile" "VP" [Infl] []]
auxVpRule [Cat "didn't" "AUX" [] [], Cat "smiled" "VP" [Tense] []]


-- finally, we can assemble all of these functions into a single function that takes us from strings directly to parse trees.

:l ParserNoMvt

prs "I loved her."
prs "I loved her." !! 0

prs "She didn't love me."
prs "She didn't love me." !! 0

prs "She despised me."

writeTree2Tex "tree1" (prs "I loved her.")
writeTree2Tex "tree2" (prs "She didn't love me.")
writeTree2Tex "tree3" (prs "The dwarf didn't defeat the giant.")
