{- ghci 1 -}
data Person = Person { firstName :: String , lastName :: String , age :: Int }
{- ghci 2 -}
data Person = Person { firstName :: String , lastName :: String , age :: Int } deriving (Eq)
{- ghci 3 -}
let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
{- ghci 4 -}
let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
{- ghci 5 -}
let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
{- ghci 6 -}
mca == adRock
{- ghci 7 -}
mikeD == adRock
{- ghci 8 -}
mikeD == mikeD
{- ghci 9 -}
mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
{- ghci 10 -}
let beastieBoys = [mca, adRock, mikeD]
{- ghci 11 -}
mikeD `elem` beastieBoys
{- ghci 12 -}
data Person = Person { firstName :: String , lastName :: String , age :: Int } deriving (Eq, Show, Read)
{- ghci 13 -}
let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
{- ghci 14 -}
mikeD
{- ghci 15 -}
Person {firstName = "Elmo", lastName = "NA", age = 0}
{- ghci 16 -}
"mikeD is: " ++ show mikeD
{- ghci 17 -}
read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}" :: Person
{- ghci 18 -}
read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}"
{- ghci 19 -}
read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}" == mikeD
{- ghci 20 -}
read "Just 'f'" :: Maybe a
{- ghci 21 -}
read "Just 'f'" :: Maybe Char
{- ghci 22 -}
True `compare` False
{- ghci 23 -}
True < False
{- ghci 24 -}
True > False
{- ghci 25 -}
Nothing < Just 100
{- ghci 26 -}
Nothing > Just (-49999)
{- ghci 27 -}
Just 50 < Just 100
{- ghci 28 -}
Just 50 > Just 100
{- ghci 29 -}
Just 3 `compare` Just 2
{- ghci 30 -}
Just (*3) < Just (*2)
{- ghci 31 -}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
{- ghci 32 -}
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
{- ghci 33 -}
Wednesday
{- ghci 34 -}
show Wednesday
{- ghci 35 -}
read "Saturday" :: Day
{- ghci 36 -}
Saturday == Sunday
{- ghci 37 -}
Saturday == Saturday
{- ghci 38 -}
Saturday > Friday
{- ghci 39 -}
Saturday < Friday
{- ghci 40 -}
Monday `compare` Wednesday
{- ghci 41 -}
minBound :: Day
{- ghci 42 -}
maxBound :: Day
{- ghci 43 -}
succ Monday
{- ghci 44 -}
pred Saturday
{- ghci 45 -}
[Thursday .. Sunday]
{- ghci 46 -}
[minBound .. maxBound] :: [Day]
{- ghci 47 -}
type AssocList k v = [(k,v)]
{- ghci 48 -}
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
{- ghci 49 -}
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
{- ghci 50 -}
Empty
{- ghci 51 -}
5 `Cons` Empty
{- ghci 52 -}
4 `Cons` (5 `Cons` Empty)
{- ghci 53 -}
3 `Cons` (4 `Cons` (5 `Cons` Empty))
{- ghci 54 -}
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
{- ghci 55 -}
Empty
{- ghci 56 -}
5 `Cons` Empty
{- ghci 57 -}
4 `Cons` (5 `Cons` Empty)
{- ghci 58 -}
3 `Cons` (4 `Cons` (5 `Cons` Empty))
{- ghci 59 -}
data List a = Empty | Cons a (List a) deriving (Read, Eq, Ord)
{- ghci 60 -}
instance (Show a) => Show (List a) where { show Empty = "#" ; show (Cons x xs) = show x ++ "," ++ show xs }
{- ghci 61 -}
Empty
{- ghci 62 -}
5 `Cons` Empty
{- ghci 63 -}
4 `Cons` (5 `Cons` Empty)
{- ghci 64 -}
3 `Cons` (4 `Cons` (5 `Cons` Empty))
{- ghci 65 -}
let { infixr 5 .++ ; (.++) :: [a] -> [a] -> [a] ; [] .++ ys = ys ; (x : xs) .++ ys = x : (xs .++ ys) }
{- ghci 66 -}
:i (*)
{- ghci 67 -}
:i (+)
{- ghci 68 -}
"hello, " .++ "new function!"
{- ghci 69 -}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Read, Eq)
{- ghci 70 -}
instance (Show a) => Show (Tree a) where { show EmptyTree = "-" ; show (Node x left right) = "[" ++ show left ++ " " ++ show x ++ " " ++ show right ++ "]" }
{- ghci 71 -}
let { singleton :: a -> Tree a ; singleton x = Node x EmptyTree EmptyTree }
{- ghci 72 -}
let { treeInsert :: (Ord a) => a -> Tree a -> Tree a ; treeInsert x EmptyTree = singleton x ; treeInsert x (Node r left right) | x == r = Node x left right | x < r  = Node r (treeInsert x left) right | x > r  = Node r left (treeInsert x right) }
{- ghci 73 -}
let nums = [8,6,4,1,7,3,5]
{- ghci 74 -}
let numsTree = foldr treeInsert EmptyTree nums
{- ghci 75 -}
numsTree
{- ghci 76 -}
:t numsTree
{- ghci 77 -}
let { elemInTree :: (Ord a) => a -> Tree a -> Bool ; elemInTree x EmptyTree = False ; elemInTree x (Node r left right) | x == r = True | x < r  = elemInTree x left | x > r  = elemInTree x right }
{- ghci 78 -}
8 `elemInTree` numsTree
{- ghci 79 -}
100 `elemInTree` numsTree
{- ghci 80 -}
1 `elemInTree` numsTree
{- ghci 81 -}
10 `elemInTree` numsTree
