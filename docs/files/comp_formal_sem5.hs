{- ghci 1 -}
:l BasicDef
{- ghci 2 -}
:i ParseTree
{- ghci 3 -}
let snowwhite = Branch "S" [Branch "NP" [Leaf "Snow White"], Branch "VP" [Branch "TV" [Leaf "loved"], Branch "NP" [Leaf "the dwarfs"]]]
{- ghci 4 -}
:t snowwhite
{- ghci 5 -}
snowwhite
{- ghci 6 -}
:t EmptyTree
{- ghci 7 -}
:t Leaf
{- ghci 8 -}
:t Leaf "Snow White"
{- ghci 9 -}
:t Branch
{- ghci 10 -}
:t Branch "NP"
{- ghci 11 -}
:t Branch "NP" [Leaf "Snow White"]
{- ghci 12 -}
:t nodeLabel
{- ghci 13 -}
nodeLabel (Branch "NP" [Leaf "Snow White"])
{- ghci 14 -}
nodeLabel (EmptyTree)
{- ghci 15 -}
nodeLabel (Leaf "Snow White")
{- ghci 16 -}
pos snowwhite
{- ghci 17 -}
subtree snowwhite []
{- ghci 18 -}
subtree snowwhite [0]
{- ghci 19 -}
subtree snowwhite [0,0]
{- ghci 20 -}
subtree snowwhite [1]
{- ghci 21 -}
subtree snowwhite [2]
{- ghci 22 -}
subtree snowwhite [1,0]
{- ghci 23 -}
subtree snowwhite [1,1]
{- ghci 24 -}
subtree snowwhite [1,1,1]
{- ghci 25 -}
subtrees snowwhite
{- ghci 26 -}
length $ subtrees snowwhite
{- ghci 27 -}
let tree1 = Leaf "Snow White"
{- ghci 28 -}
tree1
{- ghci 29 -}
pos tree1
{- ghci 30 -}
zip [0..] []
{- ghci 31 -}
zip [0..] ['a']
{- ghci 32 -}
zip [0..] ['a','b']
{- ghci 33 -}
let tree2 = Branch "NP" [Leaf "Snow White"]
{- ghci 34 -}
tree2
{- ghci 35 -}
pos tree2
{- ghci 36 -}
let tree3 = Branch "S" [Leaf "Snow White", Leaf "left"]
{- ghci 37 -}
tree3
{- ghci 38 -}
pos tree3
{- ghci 39 -}
let tree4 = Branch "S" [Leaf "Snow White", Leaf "left", Leaf "early"]
{- ghci 40 -}
tree4
{- ghci 41 -}
pos tree4
{- ghci 42 -}
nodeLabel tree4
{- ghci 43 -}
let tree5 = Branch "S" [Leaf "Snow White", Leaf "left", Branch "AdvP" [Leaf "early", Leaf "and", Leaf "quickly"]]
{- ghci 44 -}
tree5
{- ghci 45 -}
pos tree5
{- ghci 46 -}
tree1
{- ghci 47 -}
:t tree1
{- ghci 48 -}
pos tree1
{- ghci 49 -}
subtree tree1 []
{- ghci 50 -}
subtree tree1 [0]
{- ghci 51 -}
tree2
{- ghci 52 -}
:t tree2
{- ghci 53 -}
pos tree2
{- ghci 54 -}
subtree tree2 []
{- ghci 55 -}
subtree tree2 [0]
{- ghci 56 -}
subtree tree2 [1]
{- ghci 57 -}
subtree tree2 [0,0]
{- ghci 58 -}
tree4
{- ghci 59 -}
:t tree4
{- ghci 60 -}
pos tree4
{- ghci 61 -}
subtree tree4 []
{- ghci 62 -}
subtree tree4 [0]
{- ghci 63 -}
subtree tree4 [1]
{- ghci 64 -}
subtree tree4 [2]
{- ghci 65 -}
subtree tree4 [3]
{- ghci 66 -}
subtree tree4 [0,0]
{- ghci 67 -}
subtrees tree1
{- ghci 68 -}
subtrees tree2
{- ghci 69 -}
subtrees tree4
{- ghci 70 -}
let snowwhite = Branch "S" [Branch "NP" [Leaf "Snow White"], Branch "VP" [Branch "TV" [Leaf "loved"], Branch "NP" [Leaf "the dwarfs"]]]
{- ghci 71 -}
snowwhite
{- ghci 72 -}
properDominance snowwhite
{- ghci 73 -}
nodeProperDominance snowwhite
{- ghci 74 -}
dominance snowwhite
{- ghci 75 -}
nodeDominance snowwhite
{- ghci 76 -}
sisterhood snowwhite
{- ghci 77 -}
nodeSisterhood snowwhite
{- ghci 78 -}
cCommand snowwhite
{- ghci 79 -}
nodeCCommand snowwhite
{- ghci 80 -}
branchingPos snowwhite
{- ghci 81 -}
nodeBranchingPos snowwhite
{- ghci 82 -}
precedence snowwhite
{- ghci 83 -}
nodePrecedence snowwhite
{- ghci 84 -}
split2 "a"
{- ghci 85 -}
split2 "ab"
{- ghci 86 -}
split2 "abc"
{- ghci 87 -}
splitN 1 "a"
{- ghci 88 -}
splitN 3 "a"
{- ghci 89 -}
splitN 4 "a"
{- ghci 90 -}
splitN 3 "ab"
{- ghci 91 -}
splitN 3 "abc"
{- ghci 92 -}
gener 0 "abc"
{- ghci 93 -}
gener 1 "abc"
{- ghci 94 -}
gener 2 "abc"
{- ghci 95 -}
length $ gener 2 "abc"
{- ghci 96 -}
gener 3 "abc"
{- ghci 97 -}
length $ gener 3 "abc"
{- ghci 98 -}
gener 4 "abc"
{- ghci 99 -}
length $ gener 4 "abc"
{- ghci 100 -}
take 100 $ generateAll "abc"
{- ghci 101 -}
take 400 $ generateAll "abc"
{- ghci 102 -}
:l Palindromes
{- ghci 103 -}
:t recognize
{- ghci 104 -}
recognize "aa"
{- ghci 105 -}
recognize "aba"
{- ghci 106 -}
recognize "ab"
{- ghci 107 -}
recognize "abca"
{- ghci 108 -}
:t generate
{- ghci 109 -}
take 100 generate
{- ghci 110 -}
:t parse
{- ghci 111 -}
parse "aa"
{- ghci 112 -}
parse "abba"
{- ghci 113 -}
parse "abccba"
{- ghci 114 -}
parse "abcacba"
{- ghci 115 -}
:l ParserCombinatorsMini
{- ghci 116 -}
:i ParseState
{- ghci 117 -}
:i Parser
{- ghci 118 -}
:t succeed
{- ghci 119 -}
:t succeed "PARSING DONE"
{- ghci 120 -}
:t succeed "PARSING DONE" "some input here"
{- ghci 121 -}
succeed "PARSING DONE" "some input here"
{- ghci 122 -}
:t failp
{- ghci 123 -}
:t failp "some input here"
{- ghci 124 -}
failp "some input here"
{- ghci 125 -}
:t symbol
{- ghci 126 -}
:t symbol 'a'
{- ghci 127 -}
:t symbol 'b'
{- ghci 128 -}
:t symbol 'c'
{- ghci 129 -}
symbol 'a' "abc"
{- ghci 130 -}
symbol 'a' "bac"
{- ghci 131 -}
symbol 'b' "bac"
{- ghci 132 -}
symbol 'c' "cab"
{- ghci 133 -}
:t token
{- ghci 134 -}
:t token "a"
{- ghci 135 -}
:t token "ab"
{- ghci 136 -}
:t token "b"
{- ghci 137 -}
:t token "abc"
{- ghci 138 -}
token "a" "abcd"
{- ghci 139 -}
token "a" "bacd"
{- ghci 140 -}
token "ab" "abcd"
{- ghci 141 -}
:t token "ab" "abcd"
{- ghci 142 -}
token ["ab"] ["ab","cd"]
{- ghci 143 -}
digit "1a"
{- ghci 144 -}
digit "a1"
{- ghci 145 -}
digit "1"
{- ghci 146 -}
(just digit) "1"
{- ghci 147 -}
digit "1a"
{- ghci 148 -}
(just digit) "1a"
{- ghci 149 -}
:t (<|>)
{- ghci 150 -}
:t digit
{- ghci 151 -}
:t (symbol 'a')
{- ghci 152 -}
let digitOrSymbolA = digit <|> (symbol 'a')
{- ghci 153 -}
digitOrSymbolA "1a"
{- ghci 154 -}
digitOrSymbolA "9a"
{- ghci 155 -}
digitOrSymbolA "a1"
{- ghci 156 -}
digitOrSymbolA "b1"
{- ghci 157 -}
let tokenAorB = (token "a") <|> (token "b")
{- ghci 158 -}
tokenAorB "abc"
{- ghci 159 -}
tokenAorB "bac"
{- ghci 160 -}
tokenAorB "cab"
{- ghci 161 -}
let tokenAthenB = (token "a") <*> (token "b")
{- ghci 162 -}
tokenAthenB "abc"
{- ghci 163 -}
tokenAthenB "bac"
{- ghci 164 -}
tokenAthenB "cab"
{- ghci 165 -}
let tokenBthenA = (token "b") <*> (token "a")
{- ghci 166 -}
tokenBthenA "abc"
{- ghci 167 -}
tokenBthenA "bac"
{- ghci 168 -}
:i ord
{- ghci 169 -}
let { digitize :: Parser Char Int ; digitize = f <$> digit where f c = ord c - ord '0' }
{- ghci 170 -}
digit "1a"
{- ghci 171 -}
:t (digit "1a")
{- ghci 172 -}
digitize "1a"
{- ghci 173 -}
:t (digitize "1a")
{- ghci 174 -}
:i PARSER
{- ghci 175 -}
:i ParseTree
{- ghci 176 -}
epsilonT "i output the EmptyTree for any input"
{- ghci 177 -}
symbol 'a' "abc"
{- ghci 178 -}
symbolT 'a' "abc"
{- ghci 179 -}
let { palindrome :: PARSER Char String ; palindrome = epsilonT <|> symbolT 'a' <|> symbolT 'b' <|> symbolT 'c' <|> parseAs "A-pair" [symbolT 'a', palindrome, symbolT 'a'] <|> parseAs "B-pair" [symbolT 'b', palindrome, symbolT 'b'] <|> parseAs "C-pair" [symbolT 'c', palindrome, symbolT 'c'] }
{- ghci 180 -}
palindrome "aa"
{- ghci 181 -}
palindrome "abba"
{- ghci 182 -}
palindrome "abccba"
{- ghci 183 -}
palindrome "abcacba"
{- ghci 184 -}
[parse | (parse,input) <- palindrome "abcacba", input==""]
{- ghci 185 -}
head [parse | (parse,input) <- palindrome "abcacba", input==""]
{- ghci 186 -}
let { pS,pNP,pVP,pD,pN :: Parser String String ; pS = pNP <*> pVP ; pNP = symbol "Alice" <|> symbol "Dorothy" <|> (pD <*> pN) ; pVP = symbol "smiled" <|> symbol "laughed" ; pD = symbol "every" <|> symbol "some" <|> symbol "no" ; pN = symbol "dwarf" <|> symbol "wizard" }
{- ghci 187 -}
pS ["Alice", "smiled"]
{- ghci 188 -}
pS ["no", "wizard", "smiled"]
{- ghci 189 -}
let { pS = collect [pNP, pVP] ; pNP = symbol "Alice" <|> symbol "Dorothy" ; pVP = symbol "smiled" <|> symbol "laughed" }
{- ghci 190 -}
pS ["Alice", "smiled"]
{- ghci 191 -}
let { pS = parseAs "S" [pNP, pVP] ; pNP = symbolT "Alice" <|> symbolT "Dorothy" ; pVP = symbolT "smiled" <|> symbolT "laughed" }
{- ghci 192 -}
pS ["Alice", "smiled"]
{- ghci 193 -}
let { pS, pNP, pVP, pD, pN :: PARSER String String ; pS = parseAs "S" [pNP,pVP] ; pNP = symbolT "Alice" <|> symbolT "Dorothy" <|> parseAs "NP" [pD,pN] ; pVP = symbolT "smiled" <|> symbolT "laughed" ; pD = symbolT "every" <|> symbolT "some" <|> symbolT "no" ; pN = symbolT "man" <|> symbolT "woman" }
{- ghci 194 -}
pS ["Alice", "smiled"]
{- ghci 195 -}
pS ["no", "woman", "smiled"]
