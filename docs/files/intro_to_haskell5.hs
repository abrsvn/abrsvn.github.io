{- ghci 1 -}
max 4 5
{- ghci 2 -}
(max 4) 5
{- ghci 3 -}
:t max
{- ghci 4 -}
let { multThreeInts :: Int -> Int -> Int -> Int ; multThreeInts x y z = x * y * z }
{- ghci 5 -}
:t multThreeInts
{- ghci 6 -}
multThreeInts 3 5 9
{- ghci 7 -}
:t (multThreeInts 3)
{- ghci 8 -}
:t (multThreeInts 3 5)
{- ghci 9 -}
:t (multThreeInts 3 5 9)
{- ghci 10 -}
let multTwoIntsWith9 = multThreeInts 9
{- ghci 11 -}
:t multTwoIntsWith9
{- ghci 12 -}
let multOneIntWith18 = multTwoIntsWith9 2
{- ghci 13 -}
:t multOneIntWith18
{- ghci 14 -}
multOneIntWith18 10
{- ghci 15 -}
multThreeInts 9 2 10
{- ghci 16 -}
let { compareWith100 :: (Num a, Ord a) => a -> Ordering ; compareWith100 x = compare 100 x }
{- ghci 17 -}
:t compareWith100
{- ghci 18 -}
compareWith100 99
{- ghci 19 -}
let { compareWith100 :: (Num a, Ord a) => a -> Ordering ; compareWith100 = compare 100 }
{- ghci 20 -}
:t compareWith100
{- ghci 21 -}
let { divideBy10 :: (Floating a) => a -> a ; divideBy10 = (/10) }
{- ghci 22 -}
:t divideBy10
{- ghci 23 -}
divideBy10 200
{- ghci 24 -}
let { isUpperLetter :: Char -> Bool ; isUpperLetter = (`elem` ['A'..'Z']) }
{- ghci 25 -}
:t isUpperLetter
{- ghci 26 -}
isUpperLetter 'E'
{- ghci 27 -}
isUpperLetter 'e'
{- ghci 28 -}
:t (subtract 4)
{- ghci 29 -}
:t (`subtract` 4)
{- ghci 30 -}
subtract 4 9
{- ghci 31 -}
4 `subtract` 9
{- ghci 32 -}
multThreeInts 3 4
{- ghci 33 -}
let { applyTwice :: (a -> a) -> a -> a ; applyTwice f x = f (f x) }
{- ghci 34 -}
:t applyTwice
{- ghci 35 -}
applyTwice succ 10
{- ghci 36 -}
applyTwice (+3) 10
{- ghci 37 -}
applyTwice (++ " HAHA") "HEY"
{- ghci 38 -}
applyTwice ("HAHA " ++) "HEY"
{- ghci 39 -}
applyTwice (multThreeInts 2 2) 9
{- ghci 40 -}
applyTwice (3:) [1]
{- ghci 41 -}
let { zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c] ; zipWith' _ [] _ = [] ; zipWith' _ _ [] = [] ; zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys }
{- ghci 42 -}
:t zipWith'
{- ghci 43 -}
zipWith' (+) [4,2,5,6] [2,6,2,3]
{- ghci 44 -}
zipWith' max [6,3,2,1] [7,3,1,5]
{- ghci 45 -}
zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
{- ghci 46 -}
zipWith' (*) (replicate 5 2) [1..]
{- ghci 47 -}
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
{- ghci 48 -}
let { flip' :: (a -> b -> c) -> (b -> a -> c) ; flip' f = g where g x y = f y x }
{- ghci 49 -}
flip' zip [1,2,3,4,5] "hello"
{- ghci 50 -}
zipWith (flip' div) [2,2..] [10,8,6,4,2]
{- ghci 51 -}
let { flip'' :: (a -> b -> c) -> (b -> a -> c) ; flip'' f y x = f x y }
{- ghci 52 -}
flip'' zip [1,2,3,4,5] "hello"
{- ghci 53 -}
zipWith (flip'' div) [2,2..] [10,8,6,4,2]
