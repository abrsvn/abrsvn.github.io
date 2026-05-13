-- PARSER COMBINATORS

:l ParserCombinatorsMini

-- we define two types that will be essential for all our subsequent definitions

:i ParseState

-- a parse state is a list of pairs, each pair consisting of the partial parse that has been assembled up to that point and the remaining list of inputs that still need to be parsed
-- parsing is really traveling through the space of parse states in a way regulated by the grammar based on which we do parsing
-- we define a parse state as a *list* of (partialParse,[remainingInput]) pairs because we might have a locally or globally ambiguous sentence with multiple partial (for local ambiguity) or final (for global ambiguity) parses

:i Parser

-- a parser is just a function from a list of inputs to a parse state, i.e., to a list of (partial-parse, remaining-inputs) pairs


-- at a first glance, parsing is treated as a problem of segmenting text / strings and grouping smaller parts of text into larger parts of text.
-- but (generative) grammars are not about text / strings, but about trees and assembling / composing bigger trees out of smaller trees: the lexicon provides the elementary trees and the grammar rules tell us how to build larger trees from smaller trees
-- an example grammar from the "Parsing" chapter of the textbook is given below:

{-
LEXICON:
NP -> Alice | Dorothy
VP -> smiled | laughed
D -> every | some | no
N -> dwarf | wizard

PHRASE STRUCTURE RULES:
S -> NP VP
NP -> D N
-}

-- the main intuition behind parser combinators is that we should assemble parsers compositionally based on a lexicon and a grammar just as trees are generated compositionally based on a lexicon and a grammar
-- we do not segment text / strings and assemble larger texts / strings out of smaller texts / strings, but we assemble larger parsers out of smaller parsers
-- in particular, we will have basic parsers for the lexicon and parser combinators corresponding to the phrase structure rules


-- some basic parsers

-- succeed: take a particular parse and irrespective of the input, declare the input successfully parsed as the parse you took as argument

{-
succeed :: parse -> Parser input parse
succeed finalParse remainingInputs = [(finalParse,remainingInputs)]
-}

:t succeed
:t succeed "PARSING DONE"
:t succeed "PARSING DONE" "some input here"
succeed "PARSING DONE" "some input here"

-- failp: this is the parser that always fails for any input that it takes as argument; the empty set of parse state [] is returned for every input

{-
failp :: Parser input parse
failp remainingInputs = []
-}

:t failp
:t failp "some input here"
failp "some input here"

-- we can parse characters / symbols (terminals) by having parsers specialized for each of them

{-
symbol :: Eq terminal => terminal -> Parser terminal terminal
symbol t []                 = []
symbol t (x:xs) | t == x    = [(t,xs)]
                | otherwise = []
-}

:t symbol

:t symbol 'a'
:t symbol 'b'
:t symbol 'c'

symbol 'a' "abc"
symbol 'a' "bac"
symbol 'b' "bac"
symbol 'c' "cab"

-- we can parse a sequence of characters / symbols at a time by specifying a parser for lists of terminals rather than simply terminals

{-
token :: Eq terminal => [terminal] -> Parser terminal [terminal]
token ts xs | ts == take n xs   = [(ts,drop n xs)]
            | otherwise         = []
         where n = length ts
-}

:t token

:t token "a"
:t token "ab"
:t token "b"
:t token "abc"

token "a" "abcd"
token "a" "bacd"

token "ab" "abcd"
:t token "ab" "abcd"

token ["ab"] ["ab","cd"]

-- we can also define 'looser' parsers that parse an input if it satisfies a predicate (this is what the function 'reads' in the Prelude actually is ...)

{-
satisfy :: (input -> Bool) -> Parser input input
satisfy p []                 = []
satisfy p (i:is) | p i       = [(i,is)]
                 | otherwise = []

digit :: Parser Char Char
digit = satisfy isDigit
-}

digit "1a"
digit "a1"

-- just: finally, we define a parser "modifier" / "restrictor", i.e., a function from parsers to parsers that restricts a parser to final inputs, i.e., we can parse those inputs only if the remaining list of inputs is empty

{-
just :: Parser input parse -> Parser input parse
just p = filter (null.snd) . p
-}

digit "1"
(just digit) "1"
digit "1a"
(just digit) "1a"


-- parser combinators

-- just: can be thought of as a unary parser combinator: it takes a parser as an argument and returns another parser

-- we now turn to binary (or higher arity) parser combinators, i.e., functions that take multiple parsers and assemble them into one single parser
-- these parser combinators correspond to the phrase structure rules

-- just as we have "choice" / optional rewrite in the lexical or phrase-structure rules (e.g., VP -> smiled | laughed), we have a parser combinator that takes two parsers and parses an input if at least one of the two parser parses the input

{-
infixr 4 <|>
(<|>) :: Parser input parse -> Parser input parse -> Parser input parse
(p1 <|> p2) xs = p1 xs ++ p2 xs
-}

:t (<|>)

:t digit
:t (symbol 'a')

let digitOrSymbolA = digit <|> (symbol 'a')
digitOrSymbolA "1a"
digitOrSymbolA "9a"
digitOrSymbolA "a1"
digitOrSymbolA "b1"

let tokenAorB = (token "a") <|> (token "b")
tokenAorB "abc"
tokenAorB "bac"
tokenAorB "cab"

-- we also want to be able to sequence 2 parsers, e.g., to parse a sentence according to this rule S -> NP VP, we first want to parse an NP, then a VP.

{-
(<*>) :: Parser input [parse] -> Parser input [parse] -> Parser input [parse]
(p <*> q) xs = [ (r1 ++ r2,zs) | (r1,ys) <- p xs,
                                 (r2,zs) <- q ys ]
-}

-- a simple example is a parser that parses "a" first, and then "b"

let tokenAthenB = (token "a") <*> (token "b")
tokenAthenB "abc"
tokenAthenB "bac"
tokenAthenB "cab"

let tokenBthenA = (token "b") <*> (token "a")
tokenBthenA "abc"
tokenBthenA "bac"

-- finally we define an operator that enables us to apply functions to the parses produced by a parser; the reason for this is that the parsers we want to build are parsers that take text as input and output trees, not simply text.

{-
infixl 7 <$>
(<$>) :: (input -> parse) -> Parser s input -> Parser s parse
(f <$> p) xs = [ (f x,ys) | (x,ys) <- p xs ]
-}

-- a simple example is a function that parses / identifies digits in strings and then turns them into integers

:i ord

:{
let
digitize :: Parser Char Int
digitize = f <$> digit
  where f c = ord c - ord '0'
:}

digit "1a"
:t (digit "1a")
digitize "1a"
:t (digitize "1a")


-- Simple application of parser combinators: The Palindrome Grammar

-- we first specialize our general Parser type to a parser that outputs trees

:i PARSER
:i ParseTree

-- given this specialized PARSER type, we define a succeed parser for the empty tree

{-
epsilonT :: PARSER input category
epsilonT = succeed EmptyTree
-}

epsilonT "i output the EmptyTree for any input"

-- we also define a PARSER that will build a Leaf sub-tree on top of any basic symbol

{-
symbolT :: Eq input => input -> PARSER input category
symbolT s = (\ x -> Leaf x) <$> symbol s
-}

symbol 'a' "abc"
symbolT 'a' "abc"

-- our palindrome parser can now be stated very concisely as follows

:{
let
palindrome :: PARSER Char String
palindrome =
  epsilonT <|> symbolT 'a' <|> symbolT 'b' <|> symbolT 'c'
           <|> parseAs "A-pair" [symbolT 'a', palindrome, symbolT 'a']
           <|> parseAs "B-pair" [symbolT 'b', palindrome, symbolT 'b']
           <|> parseAs "C-pair" [symbolT 'c', palindrome, symbolT 'c']
:}

-- compare this with our previous definition of the parser

{-
parse :: String -> [ParseTree String String]
parse = \ xs ->
    [EmptyTree | null xs ]
 ++ [Leaf "a"  | xs == "a"]
 ++ [Leaf "b"  | xs == "b"]
 ++ [Leaf "c"  | xs == "c"]
 ++ [Branch "A-pair" [Leaf "a", t, Leaf "a"] | ["a",ys,"a"] <- splitN 3 xs, t <- parse ys]
 ++ [Branch "B-pair" [Leaf "b", t, Leaf "b"] | ["b",ys,"b"] <- splitN 3 xs, t <- parse ys]
 ++ [Branch "C-pair" [Leaf "c", t, Leaf "c"] | ["c",ys,"c"] <- splitN 3 xs, t <- parse ys]
-}

palindrome "aa"
palindrome "abba"
palindrome "abccba"
palindrome "abcacba"

[parse | (parse,input) <- palindrome "abcacba", input==""]

head [parse | (parse,input) <- palindrome "abcacba", input==""]


-- Another simple application of parser combinators -- parsing the grammar of the very simple Eng. fragment we mentioned above

{-
LEXICON:
NP -> Alice | Dorothy
VP -> smiled | laughed
D -> every | some | no
N -> dwarf | wizard

PHRASE STRUCTURE RULES:
S -> NP VP
NP -> D N
-}

-- we will now define a parser for this grammar

-- a first simple attempt is this:

:{
let
pS,pNP,pVP,pD,pN :: Parser String String
pS = pNP <*> pVP
pNP = symbol "Alice" <|> symbol "Dorothy" <|> (pD <*> pN)
pVP = symbol "smiled" <|> symbol "laughed"
pD = symbol "every" <|> symbol "some" <|> symbol "no"
pN = symbol "dwarf" <|> symbol "wizard"
:}

pS ["Alice", "smiled"]
pS ["no", "wizard", "smiled"]

-- once again, we need post-processing of parser output
-- but we need post-processing not for single parsers, but for sequential compositions of parsers
-- a parser like pNP <*> pVP should output a branch with the results of the parser pNP and pVP as daughter nodes
-- in the general case, the rhs of a rule can be any list of terminals and nonterminals, so we need to be able to decompose such lists
-- the previous version of parser composition was composing parsers of the same kind

{-
(<*>) :: Parser input [parse] -> Parser input [parse] -> Parser input [parse]
(p <*> q) xs = [ (r1 ++ r2,zs) | (r1,ys) <- p xs,
                                 (r2,zs) <- q ys ]
-}


-- we switch to a version of sequential composition of parsers that can accommodate lists of more than 2 categories on the rhs
-- in particular, first parser outputs a category of some sort (can be a list etc.), while the second parser outputs a list of such categories (list of lists etc.); the result is a parser of the more general kind, i.e., a parser that outputs a list of categories

{-
infixl 6 <:>
(<:>) :: Parser input category -> Parser input [category] -> Parser input [category]
(p <:> q) xs = [(r:rs,zs) | (r,ys) <- p xs,
                            (rs,zs) <- q ys]
-}

-- we use this parser composition <:> to define a function that collects the results of a list of parsers operating one after the other

{-
collect :: [Parser input category] -> Parser input [category]
collect []     = succeed []
collect (p:ps) = p <:> collect ps
-}

-- instead of defining a sentence parser as pNP <*> pVP, like above, we can now define it as collect [pNP,pVP]


:{
let
pS = collect [pNP, pVP]
pNP = symbol "Alice" <|> symbol "Dorothy"
pVP = symbol "smiled" <|> symbol "laughed"
:}

pS ["Alice", "smiled"]

-- finally, we also want to construct a parse tree based on the list of outputs we get after collecting multiple parsers
-- for that, we use a parser parseAs, which builds and labels non-leaf nodes.
-- the parser parseAs takes as its first argument a nonterminal (the category label), and as its second argument a list of parsers.
-- e.g., to handle a rule S -> NP VP, the function parseAs takes "S" as its label and assumes that a list of parsers for NPs and VPs has been constructed

{-
parseAs :: category -> [PARSER input category] -> PARSER input category
parseAs label ps = (\ xs -> Branch label xs) <$> collect ps
-}

-- we have to change the basic parsers also from symbol (parsers that output strings) to symbolT (parsers that output trees, in particular, leaves)

:{
let
pS = parseAs "S" [pNP, pVP]
pNP = symbolT "Alice" <|> symbolT "Dorothy"
pVP = symbolT "smiled" <|> symbolT "laughed"
:}

pS ["Alice", "smiled"]

-- the full parser specification is this:

:{
let
pS, pNP, pVP, pD, pN :: PARSER String String
pS = parseAs "S" [pNP,pVP]
pNP = symbolT "Alice" <|> symbolT "Dorothy" <|> parseAs "NP" [pD,pN]
pVP = symbolT "smiled" <|> symbolT "laughed"
pD = symbolT "every" <|> symbolT "some" <|> symbolT "no"
pN = symbolT "man" <|> symbolT "woman"
:}

pS ["Alice", "smiled"]
pS ["no", "woman", "smiled"]
