{- ghci 1 -}
:t 'a'
{- ghci 2 -}
:t True
{- ghci 3 -}
:t "HELLO!"
{- ghci 4 -}
:t (True, 'a')
{- ghci 5 -}
:t 4 == 5
{- ghci 6 -}
let removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
{- ghci 7 -}
:t removeNonUppercase
{- ghci 8 -}
let { removeNonUppercase :: [Char] -> [Char] ; removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] }
{- ghci 9 -}
let { addThree :: Int -> Int -> Int -> Int ; addThree x y z = x + y + z }
{- ghci 10 -}
:t addThree
{- ghci 11 -}
let { factorial :: Integer -> Integer ; factorial n = product [1..n] }
{- ghci 12 -}
factorial 50
{- ghci 13 -}
let { circumference :: Float -> Float ; circumference r = 2 * pi * r }
{- ghci 14 -}
circumference 4.0
{- ghci 15 -}
let { circumference' :: Double -> Double; circumference' r = 2 * pi * r }
{- ghci 16 -}
circumference' 4.0
{- ghci 17 -}
:t head
{- ghci 18 -}
:t fst
{- ghci 19 -}
:t (==)
{- ghci 20 -}
:t elem
{- ghci 21 -}
5 == 5
{- ghci 22 -}
5 /= 5
{- ghci 23 -}
'a' == 'a'
{- ghci 24 -}
"Ho Ho" == "Ho Ho"
{- ghci 25 -}
3.432 == 3.432
{- ghci 26 -}
:! hoogle --info Ord
{- ghci 27 -}
:t (>)
{- ghci 28 -}
"Abracadabra" < "Zebra"
{- ghci 29 -}
5 >= 2
{- ghci 30 -}
:t compare
{- ghci 31 -}
"Abracadabra" `compare` "Zebra"
{- ghci 32 -}
5 `compare` 3
{- ghci 33 -}
:t show
{- ghci 34 -}
show 3
{- ghci 35 -}
show 5.334
{- ghci 36 -}
show True
{- ghci 37 -}
:t read
{- ghci 38 -}
read "True" || False
{- ghci 39 -}
read "8.2" + 3.8
{- ghci 40 -}
read "5" - 2
{- ghci 41 -}
read "[1,2,3,4]" ++ [3]
{- ghci 42 -}
read "4"
{- ghci 43 -}
:t read
{- ghci 44 -}
read "5" :: Int
{- ghci 45 -}
read "5" :: Float
{- ghci 46 -}
(read "5" :: Float) * 4
{- ghci 47 -}
read "[1,2,3,4]" :: [Int]
{- ghci 48 -}
read "[1,2,3,4]" :: [Double]
{- ghci 49 -}
read "(3, 'a')" :: (Int, Char)
{- ghci 50 -}
['a'..'e']
{- ghci 51 -}
[LT .. GT]
{- ghci 52 -}
[3..5]
{- ghci 53 -}
succ 'B'
{- ghci 54 -}
:! hoogle --info Bounded
{- ghci 55 -}
minBound :: Int
{- ghci 56 -}
maxBound :: Int
{- ghci 57 -}
minBound :: Char
{- ghci 58 -}
maxBound :: Char
{- ghci 59 -}
minBound :: Bool
{- ghci 60 -}
maxBound :: Bool
{- ghci 61 -}
:t minBound
{- ghci 62 -}
:t maxBound
{- ghci 63 -}
maxBound :: (Bool, Int, Char)
{- ghci 64 -}
:! hoogle --info Num
{- ghci 65 -}
:t 20
{- ghci 66 -}
20 :: Int
{- ghci 67 -}
20 :: Integer
{- ghci 68 -}
20 :: Float
{- ghci 69 -}
20 :: Double
{- ghci 70 -}
:t (*)
{- ghci 71 -}
(5 :: Int) * (6 :: Integer)
{- ghci 72 -}
5 * (6 :: Integer)
{- ghci 73 -}
:t 5 * (6 :: Integer)
{- ghci 74 -}
:! hoogle --info Integral
{- ghci 75 -}
:! hoogle --info Floating
{- ghci 76 -}
:t fromIntegral
{- ghci 77 -}
:t length
{- ghci 78 -}
length [1,2,3,4] + 3.2
{- ghci 79 -}
fromIntegral (length [1,2,3,4]) + 3.2
{- ghci 80 -}
:t fromIntegral
