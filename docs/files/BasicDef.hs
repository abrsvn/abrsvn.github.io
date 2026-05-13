module BasicDef where

import Data.List
import Data.Maybe (fromJust)

data ParseTree nonterminal terminal =  EmptyTree | Leaf terminal | Branch nonterminal [ParseTree nonterminal terminal] deriving Eq

instance (Show terminal, Show nonterminal) => Show (ParseTree nonterminal terminal) where
  show EmptyTree = "\n"
  show (Leaf terminal) = "\n" ++ show terminal
  show (Branch nonterminal subtrees) = "\n[" ++ show nonterminal ++ ": " ++ intercalate " " [show subtree | subtree <- subtrees] ++ "]"

nodeLabel :: ParseTree nonterminal terminal -> Maybe nonterminal
nodeLabel (Branch nonterminal _) = Just nonterminal
nodeLabel (Leaf _) = Nothing
nodeLabel EmptyTree = Nothing

type Pos = [Int]
pos ::  ParseTree nonterminal terminal -> [Pos]
pos EmptyTree = [[]]
pos (Leaf _) = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts, p <- pos t ]

subtree :: ParseTree nonterminal terminal -> Pos -> Maybe (ParseTree nonterminal terminal)
subtree t [] = Just t
subtree t@(Branch _ ts) p@(i:is) = if p `elem` (pos t) then (subtree (ts!!i) is) else Nothing
subtree (Leaf _) (i:is) = Nothing
subtree (EmptyTree) (i:is) = Nothing

subtrees :: ParseTree nonterminal terminal -> [Maybe (ParseTree nonterminal terminal)]
subtrees t = [ subtree t p | p <- pos t ]

nodeLabelMaybe :: Maybe (ParseTree nonterminal terminal) -> Maybe nonterminal
nodeLabelMaybe (Just (Branch nonterminal _)) = Just nonterminal
nodeLabelMaybe (Just (Leaf _)) = Nothing
nodeLabelMaybe (Just (EmptyTree)) = Nothing
nodeLabelMaybe Nothing = Nothing

prefix :: Eq a => [a] -> [a] -> Bool
prefix []     ys     = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

type Rel a = [(a,a)]

properDominance :: ParseTree nonterminal terminal -> Rel Pos
properDominance t = [ (p,q) | p <- pos t, q <- pos t, p /= q, prefix p q ]
nodeProperDominance t = [(fromJust $ nodeLabelMaybe (subtree t pos1), fromJust $ nodeLabelMaybe (subtree t pos2)) | (pos1,pos2) <- (properDominance t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]
-- nodeProperDominance t = [((fromJust $ nodeLabelMaybe (subtree t pos1)) ++ (show pos1), (fromJust $ nodeLabelMaybe (subtree t pos2)) ++ (show pos2)) | (pos1,pos2) <- (properDominance t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]

dominance :: ParseTree nonterminal terminal -> Rel Pos
dominance t = [ (p,q) | p <- pos t, q <- pos t, prefix p q ]
nodeDominance t = [(fromJust $ nodeLabelMaybe (subtree t pos1), fromJust $ nodeLabelMaybe (subtree t pos2)) | (pos1,pos2) <- (dominance t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]
-- nodeDominance t = [((fromJust $ nodeLabelMaybe (subtree t pos1)) ++ (show pos1), (fromJust $ nodeLabelMaybe (subtree t pos2)) ++ (show pos2)) | (pos1,pos2) <- (dominance t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]

sisters :: Pos -> Pos -> Bool
sisters [i] [j] = i /= j
sisters (i:is) (j:js) = i == j && sisters is js
sisters _ _ = False

sisterhood :: ParseTree nonterminal terminal -> Rel Pos
sisterhood t = [ (p,q) | p <- pos t, q <- pos t, sisters p q ]
nodeSisterhood t = [(fromJust $ nodeLabelMaybe (subtree t pos1), fromJust $ nodeLabelMaybe (subtree t pos2)) | (pos1,pos2) <- (sisterhood t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]
-- nodeSisterhood t = [((fromJust $ nodeLabelMaybe (subtree t pos1)) ++ (show pos1), (fromJust $ nodeLabelMaybe (subtree t pos2)) ++ (show pos2)) | (pos1,pos2) <- (sisterhood t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

cCommand :: ParseTree nonterminal terminal -> Rel Pos
cCommand t = (sisterhood t) @@ (dominance t)
nodeCCommand t = [(fromJust $ nodeLabelMaybe (subtree t pos1), fromJust $ nodeLabelMaybe (subtree t pos2)) | (pos1,pos2) <- (cCommand t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]
-- nodeCCommand t = [((fromJust $ nodeLabelMaybe (subtree t pos1)) ++ (show pos1), (fromJust $ nodeLabelMaybe (subtree t pos2)) ++ (show pos2)) | (pos1,pos2) <- (cCommand t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]

branchingPos :: ParseTree nonterminal terminal -> [Pos]
branchingPos t = let ps = pos t in [ p | p <- ps, (p++[0]) `elem` ps, (p++[1]) `elem` ps ]
nodeBranchingPos t = [fromJust $ nodeLabelMaybe (subtree t pos) | pos <- (branchingPos t), (nodeLabelMaybe (subtree t pos)) /= Nothing]
-- nodeBranchingPos t = [(fromJust $ nodeLabelMaybe (subtree t pos)) ++ (show pos) | pos <- (branchingPos t), (nodeLabelMaybe (subtree t pos)) /= Nothing]

precede :: Pos -> Pos -> Bool
precede (i:is) (j:js) = i < j || (i == j && precede is js)
precede _ _ = False

precedence :: ParseTree nonterminal terminal -> Rel Pos
precedence t = [ (p,q) | p <- pos t, q <- pos t, precede p q ]
nodePrecedence t = [(fromJust $ nodeLabelMaybe (subtree t pos1), fromJust $ nodeLabelMaybe (subtree t pos2)) | (pos1,pos2) <- (precedence t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]
-- nodePrecedence t = [((fromJust $ nodeLabelMaybe (subtree t pos1)) ++ (show pos1), (fromJust $ nodeLabelMaybe (subtree t pos2)) ++ (show pos2)) | (pos1,pos2) <- (precedence t), (nodeLabelMaybe (subtree t pos1)) /= Nothing,  (nodeLabelMaybe (subtree t pos2)) /= Nothing]

split2 :: [a] -> [([a],[a])]
split2 [] = [([],[])]
split2 (x:xs) = [([],(x:xs))] ++ (map (\(ys,zs) -> ((x:ys),zs)) (split2 xs))

splitN :: Int -> [a] -> [[[a]]]
splitN n xs
  | n <= 1 = error "cannot split"
  | n == 2 = [ [ys,zs] | (ys,zs) <- split2 xs ]
  | otherwise = [ ys:rs   | (ys,zs) <- split2 xs, rs <- splitN (n-1) zs ]

gener :: Int -> String -> [String]
gener 0 alphabet = [[]]
gener n alphabet = [ x:xs | x <- alphabet, xs <- gener (n-1) alphabet ]

gener' :: Int -> String -> [String]
gener' n alphabet = gener n alphabet ++ gener' (n+1) alphabet

generateAll  :: String -> [String]
generateAll alphabet = gener' 0 alphabet
