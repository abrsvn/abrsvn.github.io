{- ghci 1 -}
let { maximum' :: (Ord a) => [a] -> a ; maximum' [] = error "maximum of empty list" ; maximum' [x] = x ; maximum' (x:xs) | x > maxTail = x | otherwise = maxTail where maxTail = maximum' xs }
{- ghci 2 -}
maximum' []
{- ghci 3 -}
maximum' [1]
{- ghci 4 -}
maximum' [2,5,1]
{- ghci 5 -}
let { maximum'' :: (Ord a) => [a] -> a ; maximum'' [] = error "maximum of empty list" ; maximum'' [x] = x ; maximum'' (x:xs) = max x (maximum'' xs) }
{- ghci 6 -}
maximum'' []
{- ghci 7 -}
maximum'' [1]
{- ghci 8 -}
maximum'' [2,5,1]
{- ghci 9 -}
let { replicate' :: (Num i, Ord i) => i -> a -> [a] ; replicate' n x | n <= 0 = [] | otherwise = x:replicate' (n-1) x }
{- ghci 10 -}
replicate' 3 5
{- ghci 11 -}
replicate' 8 "abc"
{- ghci 12 -}
replicate' 1.1 "abc"
{- ghci 13 -}
replicate' 0.1 "abc"
{- ghci 14 -}
replicate' 0 "abc"
{- ghci 15 -}
let { replicate'' :: (Integral i) => i -> a -> [a] ; replicate'' n x | n <= 0 = [] | otherwise = x:replicate'' (n-1) x }
{- ghci 16 -}
replicate'' 3 5
{- ghci 17 -}
replicate'' 8 "abc"
{- ghci 18 -}
replicate'' 1.1 "abc"
{- ghci 19 -}
replicate'' 0.1 "abc"
{- ghci 20 -}
replicate'' 0 "abc"
{- ghci 21 -}
let { take' :: (Integral i) => i -> [a] -> [a] ; take' n _ | n <= 0 = [] ; take' _ [] = [] ; take' n (x:xs) = x:take' (n-1) xs }
{- ghci 22 -}
take' 0 "hello"
{- ghci 23 -}
take' 4 ""
{- ghci 24 -}
take' 4 "hello"
{- ghci 25 -}
take' 4.2 "hello"
{- ghci 26 -}
let { reverse' :: [a] -> [a] ; reverse' [] = [] ; reverse' (x:xs) = reverse' xs ++ [x] }
{- ghci 27 -}
reverse' []
{- ghci 28 -}
reverse' "Hi"
{- ghci 29 -}
reverse' "semaphore"
{- ghci 30 -}
let { repeat' :: a -> [a] ; repeat' x = x:repeat' x }
{- ghci 31 -}
take 7 (repeat' 3)
{- ghci 32 -}
let { zip' :: [a] -> [b] -> [(a,b)] ; zip' _ [] = [] ; zip' [] _ = [] ; zip' (x:xs) (y:ys) = (x,y):zip' xs ys }
{- ghci 33 -}
zip' [1..3] ['a','b']
{- ghci 34 -}
let { elem' :: (Eq a) => a -> [a] -> Bool ; elem' a [] = False ; elem' a (x:xs) | a == x = True | otherwise = a `elem'` xs }
{- ghci 35 -}
elem' 2 [1,2,5,6]
{- ghci 36 -}
elem' 'e' "hello"
{- ghci 37 -}
elem' 'r' "hello"
{- ghci 38 -}
elem' 2 "hello"
{- ghci 39 -}
let { quicksort :: (Ord a) => [a] -> [a] ; quicksort [] = [] ; quicksort (x:xs) = let { smaller = quicksort [a | a <- xs, a <= x] ; bigger = quicksort [a | a <- xs, a > x] } in smaller ++ [x] ++ bigger }
{- ghci 40 -}
quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
{- ghci 41 -}
quicksort "the quick brown fox jumps over the lazy dog"
{- ghci 42 -}
quicksort [5,1,9,4,6,7,3]
