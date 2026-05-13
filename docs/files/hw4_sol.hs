{- ghci 1 -}
let text = "Pierre Vinken , 61 years old , will join the board " ++ "as a nonexecutive director Nov. 29 .\nMr. Vinken " ++ "is chairman of Elsevier N.V. , the Dutch publishing group ."
{- ghci 2 -}
let { itemNotInList :: (Eq a) => a -> [a] -> Bool ; itemNotInList x [] = True ; itemNotInList x (y:ys) | x == y = False | otherwise = itemNotInList x ys }
{- ghci 3 -}
let { addItemIfNew :: (Eq a) => a -> [a] -> [a] ; addItemIfNew x ys = if itemNotInList x ys then x:ys else ys }
{- ghci 4 -}
let { nub' :: (Eq a) => [a] -> [a] ; nub' xs = foldr addItemIfNew [] xs }
{- ghci 5 -}
let ws_text = concat $ map words $ lines text
{- ghci 6 -}
ws_text
{- ghci 7 -}
nub' ws_text
{- ghci 8 -}
let { countToken :: (Eq a, Num b) => a -> [a] -> b ; countToken _ [] = 0 ; countToken x (y:ys) | x == y = 1 + countToken x ys | otherwise = countToken x ys }
{- ghci 9 -}
let { countItemsInList :: (Eq a, Num b) => [a] -> [a] -> [(a,b)] ; countItemsInList [] _ = [] ; countItemsInList (x:xs) ys = (x,countToken x ys) : countItemsInList xs ys }
{- ghci 10 -}
let { tokenCounts :: (Eq a, Num b) => [a] -> [(a,b)] ; tokenCounts xs = countItemsInList (nub' xs) xs }
{- ghci 11 -}
tokenCounts ws_text
{- ghci 12 -}
import qualified Data.Set as S
{- ghci 13 -}
let setNub xs = S.toList $ S.fromList xs
{- ghci 14 -}
setNub ws_text
