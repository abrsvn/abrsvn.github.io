{- ghci 1 -}
:t foldl
{- ghci 2 -}
:!hoogle --info foldl
{- ghci 3 -}
let { sum' :: (Num a) => [a] -> a ; sum' xs = foldl (\ acc x -> acc + x) 0 xs }
{- ghci 4 -}
sum' [3,5,2,1]
{- ghci 5 -}
let { sum' :: (Num a) => [a] -> a ; sum' = foldl (+) 0 }
{- ghci 6 -}
sum' [3,5,2,1]
{- ghci 7 -}
let { elem' :: (Eq a) => a -> [a] -> Bool ; elem' y ys = foldl (\ acc x -> if x == y then True else acc) False ys }
{- ghci 8 -}
3 `elem'` [2,3,5,7]
{- ghci 9 -}
9 `elem'` [2,3,5,7]
{- ghci 10 -}
let { elem'' :: (Eq a) => a -> [a] -> Bool ; elem'' y = foldl (\ acc x -> if x == y then True else acc) False }
{- ghci 11 -}
3 `elem''` [2,3,5,7]
{- ghci 12 -}
9 `elem''` [2,3,5,7]
{- ghci 13 -}
:t foldr
{- ghci 14 -}
:!hoogle --info foldr
{- ghci 15 -}
let { map' :: (a -> b) -> [a] -> [b] ; map' f xs = foldr (\ x acc -> f x : acc) [] xs }
{- ghci 16 -}
map' (+3) [1,2,3]
{- ghci 17 -}
let { map'' :: (a -> b) -> [a] -> [b] ; map'' f = foldr (\ x acc -> f x : acc) [] }
{- ghci 18 -}
map'' (+3) [1,2,3]
{- ghci 19 -}
let { map''' :: (a -> b) -> [a] -> [b] ; map''' f = foldl (\ acc x -> acc ++ [f x]) [] }
{- ghci 20 -}
map''' (+3) [1,2,3]
{- ghci 21 -}
:t foldl1
{- ghci 22 -}
:!hoogle --info foldl1
{- ghci 23 -}
:t foldr1
{- ghci 24 -}
:!hoogle --info foldr1
{- ghci 25 -}
let sum' = foldl1 (+)
{- ghci 26 -}
sum' [1..10]
{- ghci 27 -}
:!hoogle --info "foldl'"
{- ghci 28 -}
:!hoogle --info "foldl1'"
{- ghci 29 -}
:!hoogle --info "foldr'"
{- ghci 30 -}
:!hoogle --info "foldr1'"
{- ghci 31 -}
let { maximum' :: (Ord a) => [a] -> a ; maximum' = foldr1 (\ x acc -> if x > acc then x else acc) }
{- ghci 32 -}
let { reverse' :: [a] -> [a] ; reverse' = foldl (\ acc x -> x : acc) [] }
{- ghci 33 -}
let { product' :: (Num a) => [a] -> a ; product' = foldr1 (*) }
{- ghci 34 -}
let { filter' :: (a -> Bool) -> [a] -> [a] ; filter' p = foldr (\ x acc -> if p x then x : acc else acc) [] }
{- ghci 35 -}
let { head' :: [a] -> a ; head' = foldr1 (\ x _ -> x) }
{- ghci 36 -}
let { last' :: [a] -> a ; last' = foldl1 (\ _ x -> x) }
{- ghci 37 -}
:!hoogle --info scanl
{- ghci 38 -}
:!hoogle --info scanr
{- ghci 39 -}
scanl (+) 0 [3,5,2,1]
{- ghci 40 -}
scanl1 (+) [3,5,2,1]
{- ghci 41 -}
scanl (flip (:)) [] [3,2,1]
{- ghci 42 -}
scanr (+) 0 [3,5,2,1]
{- ghci 43 -}
scanr1 (+) [3,5,2,1]
{- ghci 44 -}
scanl1 (\ acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
{- ghci 45 -}
let { sqrtSums :: Int ; sqrtSums = 1 + length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) }
{- ghci 46 -}
sqrtSums
{- ghci 47 -}
sum $ map sqrt [1..131]
{- ghci 48 -}
sum $ map sqrt [1..130]
