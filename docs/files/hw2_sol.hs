{- ghci 1 -}
:l textBrown
{- ghci 2 -}
let text = "Pierre Vinken , 61 years old , will join the board " ++ "as a nonexecutive director Nov. 29 .\nMr. Vinken " ++ "is chairman of Elsevier N.V. , the Dutch publishing group ."
{- ghci 3 -}
let ls_text = lines text
{- ghci 4 -}
ls_text
{- ghci 5 -}
ls_text !! 0
{- ghci 6 -}
ls_text !! 1
{- ghci 7 -}
let ws_s1 = words (ls_text !! 0)
{- ghci 8 -}
ws_s1
{- ghci 9 -}
let ws_s2 = words (ls_text !! 1)
{- ghci 10 -}
ws_s2
{- ghci 11 -}
"Vinken" `elem` ws_s1
{- ghci 12 -}
"Vinken" `elem` ws_s2
{- ghci 13 -}
not ("chairman" `elem` ws_s1)
{- ghci 14 -}
not ("chairman" `elem` ws_s2)
{- ghci 15 -}
"chairman" `notElem` ws_s1
{- ghci 16 -}
"chairman" `notElem` ws_s2
{- ghci 17 -}
import Data.List (nub)
{- ghci 18 -}
nub ws_s1
{- ghci 19 -}
nub ws_s2
{- ghci 20 -}
map words ls_text
{- ghci 21 -}
let ws_text = concat (map words ls_text)
{- ghci 22 -}
ws_text
{- ghci 23 -}
nub ws_text
{- ghci 24 -}
let { countToken :: (Eq a, Num b) => a -> [a] -> b ; countToken _ [] = 0 ; countToken x (y:ys) | x == y = 1 + countToken x ys | otherwise = countToken x ys }
{- ghci 25 -}
countToken "," ws_text
{- ghci 26 -}
countToken "the" ws_text
{- ghci 27 -}
countToken "Vinken" ws_text
{- ghci 28 -}
countToken ","  (nub ws_text)
{- ghci 29 -}
countToken "the"  (nub ws_text)
{- ghci 30 -}
let { countItemsInList :: (Eq a, Num b) => [a] -> [a] -> [(a,b)] ; countItemsInList [] _ = [] ; countItemsInList (x:xs) ys = (x,countToken x ys) : countItemsInList xs ys }
{- ghci 31 -}
let { tokenCounts :: (Eq a, Num b) => [a] -> [(a,b)] ; tokenCounts xs = countItemsInList (nub xs) xs }
{- ghci 32 -}
tokenCounts ws_text
{- ghci 33 -}
map words (lines textBrown)
{- ghci 34 -}
let ws_textBrown = concat (map words (lines textBrown))
{- ghci 35 -}
tokenCounts ws_textBrown
