{- ghci 1 -}
:show imports
{- ghci 2 -}
import Data.List
{- ghci 3 -}
:show imports
{- ghci 4 -}
nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
{- ghci 5 -}
nub "Lots of words and stuff"
{- ghci 6 -}
let { numUniques :: (Eq a) => [a] -> Int ; numUniques = length . nub }
{- ghci 7 -}
nub [1,2,4,5,4,2,37,7,2]
{- ghci 8 -}
numUniques [1,2,4,5,4,2,37,7,2]
{- ghci 9 -}
intersperse '.' "MONKEY"
{- ghci 10 -}
intersperse 0 [1,2,3,4,5,6]
{- ghci 11 -}
intercalate " " ["hey","there","guys"]
{- ghci 12 -}
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
{- ghci 13 -}
concat ["foo","bar","car"]
{- ghci 14 -}
concat [[3,4,5],[2,3,4],[2,1,1]]
{- ghci 15 -}
concat [[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]
{- ghci 16 -}
concat . concat $ [[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]
{- ghci 17 -}
replicate 4 1
{- ghci 18 -}
map (replicate 4) [1..3]
{- ghci 19 -}
concatMap (replicate 4) [1..3]
{- ghci 20 -}
and $ map (>4) [5,6,7,8]
{- ghci 21 -}
and $ map (==4) [4,4,4,3,4]
{- ghci 22 -}
or $ map (==4) [2,3,4,5,6,1]
{- ghci 23 -}
or $ map (>4) [1,2,3]
{- ghci 24 -}
any (==4) [2,3,5,6,1,4]
{- ghci 25 -}
all (>4) [6,9,10]
{- ghci 26 -}
all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
{- ghci 27 -}
any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
{- ghci 28 -}
take 10 $ iterate (*2) 1
{- ghci 29 -}
take 3 $ iterate (++ "hihi") "haha"
{- ghci 30 -}
splitAt 3 "heyman"
{- ghci 31 -}
splitAt 100 "heyman"
{- ghci 32 -}
splitAt (-3) "heyman"
{- ghci 33 -}
let (a,b) = splitAt 3 "foobar" in b ++ a
{- ghci 34 -}
takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
{- ghci 35 -}
takeWhile (/=' ') "This is a sentence"
{- ghci 36 -}
sum $ takeWhile (<10000) $ map (^3) [1..]
{- ghci 37 -}
dropWhile (/=' ') "This is a sentence"
{- ghci 38 -}
dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
{- ghci 39 -}
span (/=' ') "This is a sentence"
{- ghci 40 -}
let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
{- ghci 41 -}
break (==4) [1,2,3,4,5,6,7]
{- ghci 42 -}
span (/=4) [1,2,3,4,5,6,7]
{- ghci 43 -}
sort [8,5,3,2,1,6,4,2]
{- ghci 44 -}
sort "This will be sorted soon"
{- ghci 45 -}
group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
{- ghci 46 -}
map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
{- ghci 47 -}
inits "w00t"
{- ghci 48 -}
tails "w00t"
{- ghci 49 -}
let w = "w00t" in zip (inits w) (tails w)
{- ghci 50 -}
let { search :: (Eq a) => [a] -> [a] -> Bool ; search needle haystack = let nlen = length needle in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack) }
{- ghci 51 -}
search "cat" "i'm a cat burglar"
{- ghci 52 -}
"cat" `isInfixOf` "i'm a cat burglar"
{- ghci 53 -}
"Cat" `isInfixOf` "i'm a cat burglar"
{- ghci 54 -}
"cats" `isInfixOf` "i'm a cat burglar"
{- ghci 55 -}
"hey" `isPrefixOf` "hey there!"
{- ghci 56 -}
"hey" `isPrefixOf` "oh hey there!"
{- ghci 57 -}
"there!" `isSuffixOf` "oh hey there!"
{- ghci 58 -}
"there!" `isSuffixOf` "oh hey there"
{- ghci 59 -}
partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
{- ghci 60 -}
partition (>3) [1,3,5,6,3,2,1,0,3,7]
{- ghci 61 -}
span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
{- ghci 62 -}
find (>4) [1,2,3,4,5,6]
{- ghci 63 -}
find (>9) [1,2,3,4,5,6]
{- ghci 64 -}
:t find
{- ghci 65 -}
:!hoogle --info find
{- ghci 66 -}
:t elemIndex
{- ghci 67 -}
:!hoogle --info elemIndex
{- ghci 68 -}
4 `elemIndex` [1,2,3,4,5,6]
{- ghci 69 -}
10 `elemIndex` [1,2,3,4,5,6]
{- ghci 70 -}
import Data.Maybe (fromJust)
{- ghci 71 -}
fromJust (4 `elemIndex` [1,2,3,4,5,6])
{- ghci 72 -}
[1,2,3,4,5,6] !! fromJust (4 `elemIndex` [1,2,3,4,5,6])
{- ghci 73 -}
:t elemIndices
{- ghci 74 -}
:!hoogle --info elemIndices
{- ghci 75 -}
' ' `elemIndices` "Where are the spaces?"
{- ghci 76 -}
map ("Where are the spaces?" !!) (' ' `elemIndices` "Where are the spaces?")
{- ghci 77 -}
findIndex (==4) [5,3,2,1,6,4]
{- ghci 78 -}
findIndex (==7) [5,3,2,1,6,4]
{- ghci 79 -}
[5,3,2,1,6,4] !! fromJust (findIndex (==4) [5,3,2,1,6,4])
{- ghci 80 -}
findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
{- ghci 81 -}
map ("Where Are The Caps?" !!) (findIndices (`elem` ['A'..'Z']) "Where Are The Caps?")
{- ghci 82 -}
zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
{- ghci 83 -}
zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
{- ghci 84 -}
lines "first line\nsecond line\nthird line"
{- ghci 85 -}
unlines ["first line", "second line", "third line"]
{- ghci 86 -}
words "hey these are the words in this sentence"
{- ghci 87 -}
words "hey these           are    the words in this\nsentence"
{- ghci 88 -}
unwords ["hey","there","mate"]
{- ghci 89 -}
delete 'h' "hey there ghang!"
{- ghci 90 -}
delete 'h' . delete 'h' $ "hey there ghang!"
{- ghci 91 -}
delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
{- ghci 92 -}
[1..10] \\ [2,5,9]
{- ghci 93 -}
"He is a big baby" \\ "big"
{- ghci 94 -}
"He is a big baby" \\ " big"
{- ghci 95 -}
"He is a big baby" \\ "big "
{- ghci 96 -}
"He is a big baby" \\ "bbig  "
{- ghci 97 -}
delete 2 . delete 5 . delete 9 $ [1..10]
{- ghci 98 -}
"hey man" `union` "man what's up"
{- ghci 99 -}
[1..7] `union` [5..10]
{- ghci 100 -}
[1..7] `intersect` [5..10]
{- ghci 101 -}
insert 4 [3,5,1,2,8,2]
{- ghci 102 -}
insert 4 [1,3,4,4,1]
{- ghci 103 -}
sort [3,5,1,2,8,2]
{- ghci 104 -}
insert 4 $ sort [3,5,1,2,8,2]
{- ghci 105 -}
insert 'g' $ ['a'..'f'] ++ ['h'..'z']
{- ghci 106 -}
:t length
{- ghci 107 -}
let xs = [1..6] in sum xs / length xs
{- ghci 108 -}
:t genericLength
{- ghci 109 -}
let xs = [1..6] in sum xs / genericLength xs
