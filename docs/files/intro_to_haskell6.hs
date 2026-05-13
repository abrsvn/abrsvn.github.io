{- ghci 1 -}
:t map
{- ghci 2 -}
:!hoogle --info map
{- ghci 3 -}
map (+3) [1,5,3,1,6]
{- ghci 4 -}
map (++ "!") ["BIFF", "BANG", "POW"]
{- ghci 5 -}
map (replicate 3) [3..6]
{- ghci 6 -}
map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
{- ghci 7 -}
map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
{- ghci 8 -}
map (+3) [1,5,3,1,6]
{- ghci 9 -}
[x+3 | x <- [1,5,3,1,6]]
{- ghci 10 -}
let listOfFuns = map (*) [0..]
{- ghci 11 -}
(listOfFuns !! 4) 5
{- ghci 12 -}
:t filter
{- ghci 13 -}
:!hoogle --info filter
{- ghci 14 -}
filter (>3) [1,5,3,2,1,6,4,3,2,1]
{- ghci 15 -}
filter (==3) [1,2,3,4,5]
{- ghci 16 -}
filter even [1..10]
{- ghci 17 -}
let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
{- ghci 18 -}
filter (`elem` ['a'..'z']) "abcABC"
{- ghci 19 -}
filter (`elem` ['A'..'Z']) "abcABC"
{- ghci 20 -}
filter (`elem` [1..20]) (filter (`elem` [10..30]) [5,10,15,20,25,30,35])
{- ghci 21 -}
let inBothLists x = x `elem` [1..20] && x `elem` [10..30] in filter inBothLists [5,10,15,20,25,30,35]
{- ghci 22 -}
let { quicksort :: (Ord a) => [a] -> [a] ; quicksort [] = [] ; quicksort (x:xs) = let smaller = quicksort (filter (<=x) xs) ; bigger = quicksort (filter (>x) xs) in smaller ++ [x] ++ bigger }
{- ghci 23 -}
quicksort [1,4,2,15,2001,1000,4,7]
{- ghci 24 -}
let { largestDivisible :: (Integral a) => a ; largestDivisible = head (filter p [100000,99999..]) where p x = x `mod` 3829 == 0 }
{- ghci 25 -}
largestDivisible
{- ghci 26 -}
:t takeWhile
{- ghci 27 -}
takeWhile (/=' ') "elephants know how to party"
{- ghci 28 -}
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
{- ghci 29 -}
sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
{- ghci 30 -}
let { chain :: (Integral a) => a -> [a] ; chain 1 = [1] ; chain n | even n =  n:chain (n `div` 2) | odd n  =  n:chain (n*3 + 1) }
{- ghci 31 -}
chain 13
{- ghci 32 -}
chain 10
{- ghci 33 -}
chain 1
{- ghci 34 -}
chain 30
{- ghci 35 -}
let { numLongChains :: Int ; numLongChains = length (filter isLong (map chain [1..100])) where isLong xs = length xs > 15 }
{- ghci 36 -}
numLongChains
{- ghci 37 -}
(\ x -> x + 3) 7
{- ghci 38 -}
(\ s -> s ++ "man") "Bat"
{- ghci 39 -}
(\ xs -> "john" `elem` xs) ["john","bill","mary","liz"]
{- ghci 40 -}
(\ xs -> "john" `elem` xs) ["bill","mary","liz"]
{- ghci 41 -}
let { numLongChains :: Int ; numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100])) }
{- ghci 42 -}
numLongChains
{- ghci 43 -}
map (+3) [1,6,3,2]
{- ghci 44 -}
map (\ x -> x + 3) [1,6,3,2]
{- ghci 45 -}
let { addThree :: (Num a) => a -> a -> a -> a ; addThree x y z = x + y + z }
{- ghci 46 -}
let { addThree :: (Num a) => a -> a -> a -> a ; addThree = \x -> \y -> \z -> x + y + z }
{- ghci 47 -}
zipWith (\ a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
{- ghci 48 -}
map (\ (a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
{- ghci 49 -}
map (\ (a,b) -> a + b) [(1,2,2),(3,5,5),(6,3,3),(2,6,6),(2,5,5)]
{- ghci 50 -}
map (\ (a,b,c) -> a + b) [(1,2,2),(3,5,5),(6,3,3),(2,6,6),(2,5,5)]
{- ghci 51 -}
let { flip' :: (a -> b -> c) -> b -> a -> c ; flip' f = \x y -> f y x }
{- ghci 52 -}
take 4 ['a'..'z']
{- ghci 53 -}
(flip' take) ['a'..'z'] 4
{- ghci 54 -}
filter (\ (a, b, c) -> a+b+c == 24) (filter (\ (a, b, c) -> a^2+b^2==c^2) [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]])
{- ghci 55 -}
:t ($)
{- ghci 56 -}
sum (map sqrt [1..100])
{- ghci 57 -}
sum $ map sqrt [1..100]
{- ghci 58 -}
sqrt 3 + 4 + 9
{- ghci 59 -}
sqrt $ 3 + 4 + 9
{- ghci 60 -}
sum (filter (> 10) (map (*2) [2..10]))
{- ghci 61 -}
sum $ filter (> 10) $ map (*2) [2..10]
{- ghci 62 -}
map ($ 3) [(4+), (10*), (^2), sqrt, succ, pred]
{- ghci 63 -}
:t (.)
{- ghci 64 -}
(negate . (* 3)) 2
{- ghci 65 -}
(negate . (* 3)) (-2)
{- ghci 66 -}
map (\ x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
{- ghci 67 -}
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
{- ghci 68 -}
map (\ xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
{- ghci 69 -}
map (negate . sum . tail) [[1..5],[3..6],[1..7]]
{- ghci 70 -}
sum (replicate 5 (max 6.7 8.9))
{- ghci 71 -}
(sum . replicate 5 . max 6.7) 8.9
{- ghci 72 -}
sum . replicate 5 . max 6.7 $ 8.9
{- ghci 73 -}
sum . replicate 5 . maximum $ [6.7, 8.9]
{- ghci 74 -}
replicate 10 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
{- ghci 75 -}
replicate 10 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
{- ghci 76 -}
let fn x = ceiling (negate (tan (cos (max pi x))))
{- ghci 77 -}
:t fn
{- ghci 78 -}
fn 0
{- ghci 79 -}
fn $ 2*pi
{- ghci 80 -}
let fn = ceiling . negate . tan . cos . max pi
{- ghci 81 -}
:t fn
{- ghci 82 -}
fn 0
{- ghci 83 -}
fn $ 2*pi
