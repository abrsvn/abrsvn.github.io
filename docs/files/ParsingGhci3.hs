-- PARSING AND INTERPRETING A MORE REALISTIC ENGLISH FRAGMENT

-- defining a more realistic lexicon

:l Lexicon

-- we define a set of agreement features

:i Feat

features

-- agreement morphology consists of feature bundles

:i Agreement

-- we also define grammatically relevant subsets of these features

:t gender

gender features
number features
person features
gcase features
pronType features
tense features
prepType features

-- finally, we define a function that will eliminate the underspecified gender feature MascOrFem whenever the fully specified gender features Masc or Fem are added to the feature bundle

{-
prune :: Agreement -> Agreement
prune fs = if   (Masc `elem` fs || Fem `elem` fs) then (delete MascOrFem fs) else fs
-}

gender features
prune $ gender features
number features
prune $ number features

-- we can now define syntactic categories as a list consisting of a phonological representation, a category label, an agreement feature bundle and a subcategorization list

:i Cat
:i Phon
:i CatLabel
:i Agreement

-- here are a couple of examples of categories

:t Cat
Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
Cat "" "NP" [Thrd,Fem,Sg] []
Cat "littlemook" "NP" [Thrd,Masc,Sg] []

Cat "every" "DET" [Sg] []
Cat "all" "DET" [Pl] []
Cat "some" "DET" [] []
Cat "several" "DET" [Pl] []
Cat "a" "DET" [Sg] []

Cat "did" "AUX" [] []

Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]

Cat "and" "CONJ" [] []

-- we define 4 functions that enable us to extract the individual components of the categories

phon $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
catLabel $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
fs $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]
subcatList $ Cat "helped" "VP" [Tense] [Cat "" "NP" [AccOrDat] []]

-- we are now ready to define our lexicon as a function from strings (the words themselves) to lists of categories (lists b/c some words might be ambiguous)
-- see the Lexicon.hs file for many examples

-- we define a way to combine the feature bundles of 2 categories (the empty list [] indicates failure to combine)

{-
combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 =
  [feats | length (gender   feats) <= 1,
           length (number   feats) <= 1,
           length (person   feats) <= 1,
           length (gcase    feats) <= 1,
           length (pronType feats) <= 1,
           length (tense    feats) <= 1,
           length (prepType feats) <= 1]
  where
    feats = prune . nub . sort $ fs cat1 ++ fs cat2
-}

:{
let
cat1 = Cat "goldilocks" "NP" [Thrd,Fem,Sg] []
cat2 = Cat "runs" "VP" [Tense,Sg] []
cat3 = Cat "run" "VP" [Tense,Pl] []
:}

combine cat1 cat2
combine cat1 cat3

-- we can determine whether 2 categories agree this way: they agree if we combine them and we don't get an empty list

{-
agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not . null $ combine cat1 cat2
-}

agree cat1 cat2
agree cat1 cat3

-- finally, we define a function in which a particular agreement feature is assigned to a category

{-
assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) =
  [Cat phon label fs' subcatlist |
         fs' <- combine c (Cat "" "" [f] [])]
-}

assign Tense $ Cat "run" "VP" [Pl] []

-- we are now ready to take an incoming string, identify the lexical items it contains (after some preprocessing -- see the definitions of preproc and scan in the Lexicon.hs file) and extract their categories from te lexicon

{-
type Words = [String]

lexer :: String -> Words
lexer = preproc . words . (map toLower) . scan
-}

lexer "I loved her."
lexer "She despised me."

{-
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
-}

collectCats lexicon $ lexer "I loved her."
collectCats lexicon $ lexer "She despised me."
