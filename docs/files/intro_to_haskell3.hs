{- ghci 1 -}
let { lucky :: (Integral a) => a -> String ; lucky 7 = "LUCKY NUMBER SEVEN!" ; lucky x = "Sorry, you're out of luck, pal!" }
{- ghci 2 -}
lucky 7
{- ghci 3 -}
lucky 19
{- ghci 4 -}
let { sayMe :: (Integral a) => a -> String ; sayMe 1 = "One!" ; sayMe 2 = "Two!" ; sayMe 3 = "Three!" ; sayMe 4 = "Four!" ; sayMe 5 = "Five!" ; sayMe x = "Not between 1 and 5" }
{- ghci 5 -}
sayMe 1
{- ghci 6 -}
sayMe 5
{- ghci 7 -}
sayMe 6
{- ghci 8 -}
:l sayMe
{- ghci 9 -}
sayMe' 5
{- ghci 10 -}
sayMe' 6
{- ghci 11 -}
let factorial' n = product [1..n]
{- ghci 12 -}
factorial' 3
{- ghci 13 -}
factorial' 6
{- ghci 14 -}
let { factorial :: (Integral a) => a -> a ; factorial 0 = 1 ; factorial n = n * factorial (n - 1) }
{- ghci 15 -}
factorial 0
{- ghci 16 -}
factorial 3
{- ghci 17 -}
factorial 53
{- ghci 18 -}
let { charName :: Char -> String ; charName 'a' = "Albert" ; charName 'b' = "Broseph" ; charName 'c' = "Cecil" }
{- ghci 19 -}
charName 'a'
{- ghci 20 -}
charName 'b'
{- ghci 21 -}
charName 'h'
{- ghci 22 -}
let { charName' :: Char -> String ; charName' 'a' = "Albert" ; charName' 'b' = "Broseph" ; charName' 'c' = "Cecil" ; charName' _ = "I don't know this." }
{- ghci 23 -}
charName' 'a'
{- ghci 24 -}
charName' 'b'
{- ghci 25 -}
charName' 'h'
{- ghci 26 -}
let { addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a) ; addVectors' a b = (fst a + fst b, snd a + snd b) }
{- ghci 27 -}
addVectors' (1, 2.3) (1, 1)
{- ghci 28 -}
let { addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) ; addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) }
{- ghci 29 -}
:t addVectors
{- ghci 30 -}
addVectors (1, 2) (3, 4.2)
{- ghci 31 -}
let { first :: (a, b, c) -> a ; first (x, _, _) = x ; second :: (a, b, c) -> b ; second (_, y, _) = y ; third :: (a, b, c) -> c ; third (_, _, z) = z }
{- ghci 32 -}
first (1, 2, 3)
{- ghci 33 -}
second (1, 2, 3)
{- ghci 34 -}
third (1, 2, 3)
{- ghci 35 -}
first (1, 2)
{- ghci 36 -}
let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
{- ghci 37 -}
[a+b | (a,b) <- xs]
{- ghci 38 -}
let xs = [("abc","def"),("ghi", ""),("ABC","DEF"),("","GHI"),("the","end")]
{- ghci 39 -}
[[y] ++ ". " ++ [z] ++ "." | ((y:ys),(z:zs)) <- xs]
{- ghci 40 -}
let { head' :: [a] -> a ; head' [] = error "Can't call head on an empty list, dummy!" ; head' (x:_) = x }
{- ghci 41 -}
head' [4,5,6]
{- ghci 42 -}
head' "Hello"
{- ghci 43 -}
head' []
{- ghci 44 -}
:t error
{- ghci 45 -}
:i error
{- ghci 46 -}
:!hoogle --info error
{- ghci 47 -}
let { tell :: (Show a) => [a] -> String ; tell [] = "The list is empty" ; tell (x:[]) = "The list has one element: " ++ show x ; tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y ; tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y }
{- ghci 48 -}
tell []
{- ghci 49 -}
tell "h"
{- ghci 50 -}
tell "he"
{- ghci 51 -}
tell "hello"
{- ghci 52 -}
let { length' :: (Num b) => [a] -> b ; length' [] = 0 ; length' (_:xs) = 1 + length' xs }
{- ghci 53 -}
length' []
{- ghci 54 -}
length' "hello"
{- ghci 55 -}
length' "hello world"
{- ghci 56 -}
length' "ham"
{- ghci 57 -}
let { sum' :: (Num a) => [a] -> a ; sum' [] = 0 ; sum' (x:xs) = x + sum' xs }
{- ghci 58 -}
sum' []
{- ghci 59 -}
sum' [1..10]
{- ghci 60 -}
sum' [1..100]
{- ghci 61 -}
let { firstLetter :: String -> String ; firstLetter "" = "Empty string, whoops!" ; firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] }
{- ghci 62 -}
firstLetter "Dracula"
{- ghci 63 -}
firstLetter "Tonsil"
{- ghci 64 -}
firstLetter "tonsil"
{- ghci 65 -}
firstLetter ""
{- ghci 66 -}
firstLetter []
{- ghci 67 -}
let { bmiTell :: (Floating a, Ord a) => a -> String ; bmiTell bmi | bmi <= 18.5 = "underweight range" | bmi <= 25.0 = "normal range" | bmi <= 30.0 = "overweight range" | otherwise = "obese range" }
{- ghci 68 -}
bmiTell 24.3
{- ghci 69 -}
bmiTell 34.0
{- ghci 70 -}
let { bmiTell :: (RealFloat a) => a -> a -> String ; bmiTell weight height | weight / height ^ 2 <= 18.5 = "underweight range" | weight / height ^ 2 <= 25.0 = "normal range" | weight / height ^ 2 <= 30.0 = "overweight range" | otherwise = "obese range" }
{- ghci 71 -}
let { max' :: (Ord a) => a -> a -> a ; max' a b | a > b = a | otherwise = b }
{- ghci 72 -}
max' 2 5
{- ghci 73 -}
let { max'' :: (Ord a) => a -> a -> a; max'' a b | a > b = a | otherwise = b }
{- ghci 74 -}
max'' 2 5
{- ghci 75 -}
let { myCompare :: (Ord a) => a -> a -> Ordering ; a `myCompare` b | a > b = GT | a == b = EQ | otherwise = LT }
{- ghci 76 -}
3 `myCompare` 2
{- ghci 77 -}
let { bmiTell :: (Floating a, Ord a) => a -> a -> String ; bmiTell weight height | bmi <= 18.5 = "underweight range" | bmi <= 25.0 = "normal range" | bmi <= 30.0 = "overweight range" | otherwise   = "obese range" where bmi = weight / height ^ 2 }
{- ghci 78 -}
bmiTell 85 1.90
{- ghci 79 -}
let { bmiTell :: (Floating a, Ord a) => a -> a -> String ; bmiTell weight height | bmi <= skinny = "underweight range" | bmi <= normal = "normal range" | bmi <= fat = "overweight range" | otherwise = "obese range" where { bmi = weight / height ^ 2 ; skinny = 18.5 ; normal = 25.0 ; fat = 30.0 } }
{- ghci 80 -}
bmiTell 85 1.90
{- ghci 81 -}
let { bmiTell :: (Floating a, Ord a) => a -> a -> String ; bmiTell weight height | bmi <= skinny = "underweight range" | bmi <= normal = "normal range" | bmi <= fat = "overweight range" | otherwise = "obese range" where { bmi = weight / height ^ 2 ; (skinny, normal, fat) = (18.5, 25.0, 30.0) } }
{- ghci 82 -}
bmiTell 85 1.90
{- ghci 83 -}
let { initials :: String -> String -> String ; initials firstname lastname = [f] ++ ". " ++ [l] ++ "." where { (f:_) = firstname ; (l:_) = lastname } }
{- ghci 84 -}
initials "John" "Doe"
{- ghci 85 -}
let { initials' :: String -> String -> String ; initials' firstname@(f:_) lastname@(l:_) = [f] ++ ". " ++ [l] ++ "." }
{- ghci 86 -}
initials' "John" "Doe"
{- ghci 87 -}
let { initials'' :: String -> String -> String ; initials'' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "." }
{- ghci 88 -}
initials'' "John" "Doe"
{- ghci 89 -}
let { calcBmis :: (Floating a) => [(a, a)] -> [a] ; calcBmis xs = [bmi w h | (w, h) <- xs] where bmi weight height = weight / height ^ 2 }
{- ghci 90 -}
calcBmis [(80, 1.75), (75, 1.80)]
{- ghci 91 -}
let { cylinder :: (Floating a) => a -> a -> a ; cylinder r h = let sideArea = 2 * pi * r * h ; topArea = pi * r ^2 in sideArea + 2 * topArea }
{- ghci 92 -}
cylinder 2 7
{- ghci 93 -}
[if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
{- ghci 94 -}
4 * (if 10 > 5 then 10 else 0) + 2
{- ghci 95 -}
4 * (let a = 9 in a + 1) + 2
{- ghci 96 -}
[let square x = x * x in (square 5, square 3, square 2)]
{- ghci 97 -}
let a = 100; b = 200; c = 300 in a*b*c
{- ghci 98 -}
let foo = "Hey "; bar = "there!" in foo ++ bar
{- ghci 99 -}
let a = 100; b = 200; c = 300; in a*b*c
{- ghci 100 -}
let foo = "Hey "; bar = "there!"; in foo ++ bar
{- ghci 101 -}
let (a,b,c) = (1,2,3)
{- ghci 102 -}
a
{- ghci 103 -}
b
{- ghci 104 -}
c
{- ghci 105 -}
(let (a,b,c) = (1,2,3) in a+b+c) * 100
{- ghci 106 -}
let { calcBmis :: (Floating a) => [(a, a)] -> [a] ; calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] }
{- ghci 107 -}
calcBmis [(80, 1.87), (63, 1.62)]
{- ghci 108 -}
let { calcBmis :: (Floating a) => [(a, a)] -> [a] ; calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] }
{- ghci 109 -}
let zoot x y z = x * y + z
{- ghci 110 -}
zoot 3 9 2
{- ghci 111 -}
let boot x y z = x * y + z in boot 3 4 2
{- ghci 112 -}
boot
{- ghci 113 -}
let { head' :: [a] -> a ; head' [] = error "No head for empty lists!" ; head' (x:_) = x }
{- ghci 114 -}
head' "whassup"
{- ghci 115 -}
head' ""
{- ghci 116 -}
let { head'' :: [a] -> a ; head'' xs = case xs of { [] -> error "No head for empty lists!" ; (x:_) -> x } }
{- ghci 117 -}
head'' "whassup"
{- ghci 118 -}
head'' ""
{- ghci 119 -}
let { lessThanTwo :: (Integral a) => a -> String ; lessThanTwo x | x < 2 = case x of { 0 -> "zero" ; 1 -> "one" ; x -> "negative number" } | otherwise = "two or more" }
{- ghci 120 -}
lessThanTwo 0
{- ghci 121 -}
lessThanTwo 1
{- ghci 122 -}
lessThanTwo (-5)
{- ghci 123 -}
lessThanTwo 5
{- ghci 124 -}
let { describeList :: [a] -> String ; describeList xs = "The list is " ++ case xs of { [] -> "empty." ; [x] -> "a singleton list." ; xs -> "a longer list." } }
{- ghci 125 -}
describeList []
{- ghci 126 -}
describeList [1]
{- ghci 127 -}
describeList [1..5]
{- ghci 128 -}
let { describeList' :: [a] -> String ; describeList' xs = "The list is " ++ what xs where { what [] = "empty." ; what [x] = "a singleton list." ; what xs = "a longer list." } }
{- ghci 129 -}
describeList' []
{- ghci 130 -}
describeList' [1]
{- ghci 131 -}
describeList' [1..5]
