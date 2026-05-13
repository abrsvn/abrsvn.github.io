-- INTRODUCTION: basic definitions for parsing

:l BasicDef

-- we define parse trees

:i ParseTree

-- here's an example

let snowwhite = Branch "S" [Branch "NP" [Leaf "Snow White"], Branch "VP" [Branch "TV" [Leaf "loved"], Branch "NP" [Leaf "the dwarfs"]]]
:t snowwhite
snowwhite

-- let's examine the value constructors more closely and also how the type parameters of the ParseTree type work

:t EmptyTree
:t Leaf
:t Leaf "Snow White"
:t Branch
:t Branch "NP"
:t Branch "NP" [Leaf "Snow White"]

-- we also have a convenience function that retrieves the label / category of the root node of a parse tree, if that label exists

:t nodeLabel
nodeLabel (Branch "NP" [Leaf "Snow White"])
nodeLabel (EmptyTree)
nodeLabel (Leaf "Snow White")

-- we can index the positions / nodes in a tree so that we can define various relations between them: dominance, c-command, precedes etc.

pos snowwhite

-- and we can identify the sub-trees at various positions

subtree snowwhite []
subtree snowwhite [0]
subtree snowwhite [0,0]
subtree snowwhite [1]
subtree snowwhite [2]
subtree snowwhite [1,0]
subtree snowwhite [1,1]
subtree snowwhite [1,1,1]

subtrees snowwhite
length $ subtrees snowwhite


-- let's understand the position function a bit better

{-
type Pos = [Int]
pos ::  ParseTree label subtree -> [Pos]
pos EmptyTree = [[]]
pos (Leaf _) = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, p <- pos t ]
-}

let tree1 = Leaf "Snow White"
tree1
pos tree1

zip [0..] []
zip [0..] ['a']
zip [0..] ['a','b']

let tree2 = Branch "NP" [Leaf "Snow White"]
tree2
pos tree2

let tree3 = Branch "S" [Leaf "Snow White", Leaf "left"]
tree3
pos tree3

let tree4 = Branch "S" [Leaf "Snow White", Leaf "left", Leaf "early"]
tree4
pos tree4
nodeLabel tree4

let tree5 = Branch "S" [Leaf "Snow White", Leaf "left", Branch "AdvP" [Leaf "early", Leaf "and", Leaf "quickly"]]
tree5
pos tree5


-- let's examine the definition of subtree more closely

{-
subtree :: ParseTree label subtree -> Pos -> Maybe (ParseTree label subtree)
subtree t [] = Just t
subtree t@(Branch _ ts) p@(i:is) = if p `elem` (pos t) then (subtree (ts!!i) is) else Nothing
subtree (Leaf _) (i:is) = Nothing
subtree (EmptyTree) (i:is) = Nothing

subtrees :: ParseTree label subtree -> [Maybe (ParseTree label subtree)]
subtrees t = [ subtree t p | p <- pos t ]
-}

tree1
:t tree1
pos tree1
subtree tree1 []
subtree tree1 [0]

tree2
:t tree2
pos tree2
subtree tree2 []
subtree tree2 [0]
subtree tree2 [1]
subtree tree2 [0,0]

tree4
:t tree4
pos tree4
subtree tree4 []
subtree tree4 [0]
subtree tree4 [1]
subtree tree4 [2]
subtree tree4 [3]
subtree tree4 [0,0]

subtrees tree1
subtrees tree2
subtrees tree4

-- we also define the usual structural relations over nodes in the tree: dominance, sisterhood, c-command, branching nodes, precedence

let snowwhite = Branch "S" [Branch "NP" [Leaf "Snow White"], Branch "VP" [Branch "TV" [Leaf "loved"], Branch "NP" [Leaf "the dwarfs"]]]
snowwhite

properDominance snowwhite
nodeProperDominance snowwhite

dominance snowwhite
nodeDominance snowwhite

sisterhood snowwhite
nodeSisterhood snowwhite

cCommand snowwhite
nodeCCommand snowwhite

branchingPos snowwhite
nodeBranchingPos snowwhite

precedence snowwhite
nodePrecedence snowwhite


-- we also define a function that splits the incoming input string into 2 or more substrings

{-
split2 :: [a] -> [([a],[a])]
split2 [] = [([],[])]
split2 (x:xs) = [([],(x:xs))] ++ (map (\(ys,zs) -> ((x:ys),zs)) (split2 xs))
-}

split2 "a"
split2 "ab"
split2 "abc"

{-
splitN :: Int -> [a] -> [[[a]]]
splitN n xs
  | n <= 1 = error "cannot split"
  | n == 2 = [ [ys,zs] | (ys,zs) <- split2 xs ]
  | otherwise = [ ys:rs   | (ys,zs) <- split2 xs, rs <- splitN (n-1) zs ]
-}

splitN 1 "a"
splitN 3 "a"
splitN 4 "a"
splitN 3 "ab"
splitN 3 "abc"

-- and finally we define a function that generates all finite strings defined over an input alphabet

{-
gener :: Int -> String -> [String]
gener 0 alphabet = [[]]
gener n alphabet = [ x:xs | x <- alphabet, xs <- gener (n-1) alphabet ]
-}

gener 0 "abc"
gener 1 "abc"
gener 2 "abc"
length $ gener 2 "abc"
gener 3 "abc"
length $ gener 3 "abc"
gener 4 "abc"
length $ gener 4 "abc"

{-
gener' :: Int -> String -> [String]
gener' n alphabet = gener n alphabet ++ gener' (n+1) alphabet

generateAll  :: String -> [String]
generateAll alphabet = gener' 0 alphabet
-}

take 100 $ generateAll "abc"
take 400 $ generateAll "abc"


-- RECOGNITION, GENERATION and PARSING for a simple palindrome grammar

-- we can put the last couple of notions to work by defining a recognizer, generator and parser for a simple grammar for palindromes

:l Palindromes

:t recognize
recognize "aa"
recognize "aba"
recognize "ab"
recognize "abca"

:t generate
take 100 generate

:t parse
parse "aa"
parse "abba"
parse "abccba"
parse "abcacba"
