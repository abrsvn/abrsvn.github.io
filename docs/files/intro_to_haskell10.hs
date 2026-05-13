{- ghci 1 -}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
{- ghci 2 -}
:t Circle
{- ghci 3 -}
:t Rectangle
{- ghci 4 -}
let { surface :: Shape -> Float ; surface (Circle _ _ r) = pi * r ^ 2 ; surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) }
{- ghci 5 -}
surface $ Circle 10 20 10
{- ghci 6 -}
surface $ Rectangle 0 0 100 100
{- ghci 7 -}
Circle 10 20 5
{- ghci 8 -}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
{- ghci 9 -}
let { surface :: Shape -> Float ; surface (Circle _ _ r) = pi * r ^ 2 ; surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) }
{- ghci 10 -}
Circle 10 20 5
{- ghci 11 -}
Rectangle 50 230 60 90
{- ghci 12 -}
map (Circle 10 20) [4,5,6,7]
{- ghci 13 -}
data Point = Point Float Float deriving (Show)
{- ghci 14 -}
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
{- ghci 15 -}
let { surface :: Shape -> Float ; surface (Circle _ r) = pi * r ^ 2 ; surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1) }
{- ghci 16 -}
surface (Rectangle (Point 0 0) (Point 100 100))
{- ghci 17 -}
surface (Circle (Point 0 0) 24)
{- ghci 18 -}
let { nudge :: Shape -> Float -> Float -> Shape ; nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r ; nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)) }
{- ghci 19 -}
nudge (Circle (Point 34 34) 10) 5 10
{- ghci 20 -}
let { baseCircle :: Float -> Shape ; baseCircle r = Circle (Point 0 0) r ; baseRect :: Float -> Float -> Shape ; baseRect width height = Rectangle (Point 0 0) (Point width height) }
{- ghci 21 -}
nudge (baseRect 40 100) 60 23
{- ghci 22 -}
data Person = Person String String Int Float String String deriving (Show)
{- ghci 23 -}
:t Person
{- ghci 24 -}
let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
{- ghci 25 -}
guy
{- ghci 26 -}
let { firstName :: Person -> String ; firstName (Person firstname _ _ _ _ _) = firstname ; lastName :: Person -> String ; lastName (Person _ lastname _ _ _ _) = lastname ; age :: Person -> Int ; age (Person _ _ age _ _ _) = age ; height :: Person -> Float ; height (Person _ _ _ height _ _) = height ; phoneNumber :: Person -> String ; phoneNumber (Person _ _ _ _ number _) = number ; flavor :: Person -> String ; flavor (Person _ _ _ _ _ flavor) = flavor }
{- ghci 27 -}
let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
{- ghci 28 -}
firstName guy
{- ghci 29 -}
height guy
{- ghci 30 -}
flavor guy
{- ghci 31 -}
data Person' = Person' { firstName :: String , lastName :: String , age :: Int , height :: Float , phoneNumber :: String , flavor :: String } deriving (Show)
{- ghci 32 -}
:t Person'
{- ghci 33 -}
:t flavor
{- ghci 34 -}
:t firstName
{- ghci 35 -}
data Car = Car String String Int deriving (Show)
{- ghci 36 -}
Car "Ford" "Mustang" 1967
{- ghci 37 -}
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
{- ghci 38 -}
Car {company="Ford", model="Mustang", year=1967}
{- ghci 39 -}
Car {company="Ford", year=1967, model="Mustang"}
{- ghci 40 -}
let defaultCar = Car { company="", year=0, model="" }
{- ghci 41 -}
defaultCar
{- ghci 42 -}
let car1 = defaultCar { company="BMW" }
{- ghci 43 -}
car1
{- ghci 44 -}
let car2 = car1 { year=2013 }
{- ghci 45 -}
car2
{- ghci 46 -}
let car3 = car2 { model="X1" }
{- ghci 47 -}
car3
{- ghci 48 -}
data Car = Car String String Int deriving (Show)
{- ghci 49 -}
Car "Ford" "Mustang" 1967
{- ghci 50 -}
:t Car
{- ghci 51 -}
:!hoogle --info Maybe
{- ghci 52 -}
:t Just 'f'
{- ghci 53 -}
:t "hello"
{- ghci 54 -}
:t ["hello"]
{- ghci 55 -}
:t [["hello"]]
{- ghci 56 -}
:t [1..6]
{- ghci 57 -}
:t [[1..6]]
{- ghci 58 -}
:t [[[1..6]]]
{- ghci 59 -}
[1,2,3] ++ []
{- ghci 60 -}
["ha","ha","ha"] ++ []
{- ghci 61 -}
:t Nothing
{- ghci 62 -}
:t []
{- ghci 63 -}
data Vector a = Vector a a a deriving (Show)
{- ghci 64 -}
let { vplus :: (Num a) => Vector a -> Vector a -> Vector a ; (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n) }
{- ghci 65 -}
(Vector 1 2 3) `vplus` (Vector 4 5 6)
{- ghci 66 -}
let { vectMult :: (Num a) => Vector a -> a -> Vector a ; (Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m) }
{- ghci 67 -}
(Vector 1 2 3) `vectMult` 4
{- ghci 68 -}
let { scalarMult :: (Num a) => Vector a -> Vector a -> a ; (Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n }
{- ghci 69 -}
(Vector 1 2 3) `scalarMult` (Vector 4 4 4)
