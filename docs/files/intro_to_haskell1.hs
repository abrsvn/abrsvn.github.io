{- ghci 1 -}
-- to comment out a line, use --
{- ghci 2 -}
2 + 15
{- ghci 3 -}
49 * 100
{- ghci 4 -}
5 / 2
{- ghci 5 -}
(50 * 100) - 4999
{- ghci 6 -}
50 * 100 - 4999
{- ghci 7 -}
50 * (100 - 4999)
{- ghci 8 -}
5 * -3
{- ghci 9 -}
5 * (-3)
{- ghci 10 -}
True && False
{- ghci 11 -}
True && True
{- ghci 12 -}
False || True
{- ghci 13 -}
not False
{- ghci 14 -}
not (True && True)
{- ghci 15 -}
5 == 5
{- ghci 16 -}
1 == 0
{- ghci 17 -}
5 /= 5
{- ghci 18 -}
5 /= 4
{- ghci 19 -}
"hello" == "hello"
{- ghci 20 -}
5 + "llama"
{- ghci 21 -}
5 == True
{- ghci 22 -}
succ 8
{- ghci 23 -}
min 9 10
{- ghci 24 -}
min 3.4 3.2
{- ghci 25 -}
max 100 101
{- ghci 26 -}
succ 9 + max 5 4 + 1
{- ghci 27 -}
(succ 9) + (max 5 4) + 1
{- ghci 28 -}
succ 9 * 10
{- ghci 29 -}
succ (9 * 10)
{- ghci 30 -}
div 92 10
{- ghci 31 -}
92 `div` 10
{- ghci 32 -}
let doubleMe x = x + x
{- ghci 33 -}
let { doubleMe x = x + x }
{- ghci 34 -}
let { doubleMe x = x + x ; }
{- ghci 35 -}
doubleMe 9
{- ghci 36 -}
doubleMe 8.3
{- ghci 37 -}
let { doubleUs x y = x*2 + y*2 }
{- ghci 38 -}
doubleUs 4 9
{- ghci 39 -}
doubleUs 2.3 34.2
{- ghci 40 -}
doubleUs 28 88 + doubleMe 123
{- ghci 41 -}
let { doubleUs x y = doubleMe x + doubleMe y }
{- ghci 42 -}
doubleUs 4 9
{- ghci 43 -}
doubleUs 2.3 34.2
{- ghci 44 -}
doubleUs 28 88 + doubleMe 123
{- ghci 45 -}
let { doubleSmallNumber x = if x > 100 then x else x*2 }
{- ghci 46 -}
doubleSmallNumber 99
{- ghci 47 -}
doubleSmallNumber 100
{- ghci 48 -}
doubleSmallNumber 101
{- ghci 49 -}
let { doubleSmallNumber2 x = (if x > 100 then x else x*2) + 1 }
{- ghci 50 -}
doubleSmallNumber2 99
{- ghci 51 -}
doubleSmallNumber2 100
{- ghci 52 -}
doubleSmallNumber2 101
{- ghci 53 -}
let { doubleSmallNumber' x = if x > 100 then x else x*2 + 1 }
{- ghci 54 -}
doubleSmallNumber' 99
{- ghci 55 -}
doubleSmallNumber' 100
{- ghci 56 -}
doubleSmallNumber' 101
{- ghci 57 -}
let { conanO'Brien = "It's a-me, Conan O'Brien!" }
{- ghci 58 -}
conanO'Brien
{- ghci 59 -}
let lostNumbers = [4,8,15,16,23,42]
{- ghci 60 -}
lostNumbers
{- ghci 61 -}
let badList = [1,2,'a',3,'b','c',4]
{- ghci 62 -}
let greeting1 = "hello"
{- ghci 63 -}
greeting1
{- ghci 64 -}
let greeting2 = ['h','e','l','l','o']
{- ghci 65 -}
greeting2
{- ghci 66 -}
[1,2,3,4] ++ [9,10,11,12]
{- ghci 67 -}
"hello" ++ " " ++ "world"
{- ghci 68 -}
['w','o'] ++ ['o','t']
{- ghci 69 -}
'A':" SMALL CAT"
{- ghci 70 -}
5:[1,2,3,4,5]
{- ghci 71 -}
[1,2,3,4] ++ [5]
{- ghci 72 -}
1:2:3:[]
{- ghci 73 -}
"Steve Buscemi" !! 6
{- ghci 74 -}
[9.4,33.2,96.2,11.2,23.25] !! 1
{- ghci 75 -}
[9.4,33.2,96.2,11.2] !! 6
{- ghci 76 -}
let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
{- ghci 77 -}
b
{- ghci 78 -}
b ++ [[1,1,1,1]]
{- ghci 79 -}
[6,6,6]:b
{- ghci 80 -}
b !! 2
{- ghci 81 -}
[3,2,1] > [2,1,0]
{- ghci 82 -}
[3,2,1] > [2,10,100]
{- ghci 83 -}
[3,4,2] > [3,4]
{- ghci 84 -}
[3,4,2] > [2,4]
{- ghci 85 -}
[3,4,2] == [3,4,2]
{- ghci 86 -}
head [5,4,3,2,1]
{- ghci 87 -}
tail [5,4,3,2,1]
{- ghci 88 -}
last [5,4,3,2,1]
{- ghci 89 -}
init [5,4,3,2,1]
{- ghci 90 -}
head []
{- ghci 91 -}
length [5,4,3,2,1,5,4,3,2,1]
{- ghci 92 -}
null [1,2,3]
{- ghci 93 -}
null []
{- ghci 94 -}
reverse [5,4,3,2,1]
{- ghci 95 -}
take 3 [5,4,3,2,1]
{- ghci 96 -}
take 1 [3,9,3]
{- ghci 97 -}
take 5 [1,2]
{- ghci 98 -}
take 0 [6,6,6]
{- ghci 99 -}
drop 3 [8,4,2,1,5,6]
{- ghci 100 -}
drop 0 [1,2,3,4]
{- ghci 101 -}
drop 100 [1,2,3,4]
{- ghci 102 -}
maximum [1,9,2,3,4]
{- ghci 103 -}
minimum [8,4,2,1,5,6]
{- ghci 104 -}
sum [5,2,1,6,3,2,5,7]
{- ghci 105 -}
product [6,2,1,2]
{- ghci 106 -}
product [1,2,5,6,7,9,2,0]
{- ghci 107 -}
4 `elem` [3,4,5,6]
{- ghci 108 -}
10 `elem` [3,4,5,6]
{- ghci 109 -}
elem 4 [3,4,5,6]
{- ghci 110 -}
elem 10 [3,4,5,6]
{- ghci 111 -}
elem [3,4,5,6] 4
{- ghci 112 -}
[1..20]
{- ghci 113 -}
['a'..'z']
{- ghci 114 -}
['K'..'Z']
{- ghci 115 -}
[2,4..20]
{- ghci 116 -}
[3,6..20]
{- ghci 117 -}
[1,2,4,8,16..100]
{- ghci 118 -}
[1,2..100]
{- ghci 119 -}
[20..1]
{- ghci 120 -}
[20,19..1]
{- ghci 121 -}
[0.1,0.3..1]
{- ghci 122 -}
[0.1,0.3..5.0]
{- ghci 123 -}
[13,26..24*13]
{- ghci 124 -}
take 24 [13,26..]
{- ghci 125 -}
take 10 (cycle [1,2,3])
{- ghci 126 -}
take 12 (cycle "LOL ")
{- ghci 127 -}
take 10 (repeat 5)
{- ghci 128 -}
replicate 3 10
{- ghci 129 -}
replicate 10 3
{- ghci 130 -}
[x*2 | x <- [1..10]]
{- ghci 131 -}
[x*2 | x <- [1..10], x*2 >= 12]
{- ghci 132 -}
[ x | x <- [50..100], x `mod` 7 == 3]
{- ghci 133 -}
let { boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] }
{- ghci 134 -}
[7..13]
{- ghci 135 -}
boomBangs [7..13]
{- ghci 136 -}
boomBangs [7..15]
{- ghci 137 -}
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
{- ghci 138 -}
[x*y | x <- [2,5,10], y <- [8,10,11]]
{- ghci 139 -}
[x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
{- ghci 140 -}
let nouns = ["hobo","frog","pope"]
{- ghci 141 -}
let adjectives = ["lazy","grouchy","scheming"]
{- ghci 142 -}
[adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
{- ghci 143 -}
let { length' xs = sum [1 | _ <- xs] }
{- ghci 144 -}
[1 | _ <- "hello"]
{- ghci 145 -}
sum [1 | _ <- "hello"]
{- ghci 146 -}
let { removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']] }
{- ghci 147 -}
removeNonUppercase "Hahaha! Ahahaha!"
{- ghci 148 -}
removeNonUppercase "IdontLIKEFROGS"
{- ghci 149 -}
let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
{- ghci 150 -}
[[ x | x <- xs, even x] | xs <- xxs]
{- ghci 151 -}
[(1,2),(8,11),(4,5)]
{- ghci 152 -}
[(1,2),(8,11,5),(4,5)]
{- ghci 153 -}
[("Two",2),("One",2)]
{- ghci 154 -}
[(1,2),("One",2)]
{- ghci 155 -}
fst (8,11)
{- ghci 156 -}
fst ("Wow", False)
{- ghci 157 -}
snd (8,11)
{- ghci 158 -}
snd ("Wow", False)
{- ghci 159 -}
zip [1,2,3,4,5] [5,5,5,5,5]
{- ghci 160 -}
zip [1..5] ["one", "two", "three", "four", "five"]
{- ghci 161 -}
zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
{- ghci 162 -}
zip [1..] ["apple", "orange", "cherry", "mango"]
{- ghci 163 -}
let triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
{- ghci 164 -}
take 30 triangles
{- ghci 165 -}
let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
{- ghci 166 -}
rightTriangles
{- ghci 167 -}
let rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
{- ghci 168 -}
rightTriangles'
