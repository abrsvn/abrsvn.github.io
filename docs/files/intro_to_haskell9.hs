{- ghci 1 -}
import Data.Char
{- ghci 2 -}
all isAlphaNum "bobby283"
{- ghci 3 -}
all isAlphaNum "eddy the fish!"
{- ghci 4 -}
:t generalCategory
{- ghci 5 -}
generalCategory ' '
{- ghci 6 -}
generalCategory 'A'
{- ghci 7 -}
generalCategory 'a'
{- ghci 8 -}
generalCategory '.'
{- ghci 9 -}
generalCategory '9'
{- ghci 10 -}
map toUpper "wow that's great r2-d2"
{- ghci 11 -}
map toLower "WOW THAT'S GREAT R2-D2"
{- ghci 12 -}
let { encode :: Int -> String -> String ; encode shift msg = map chr $ map (+ shift) $ map ord msg }
{- ghci 13 -}
:!hoogle --info "ord :: Char -> Int"
{- ghci 14 -}
map ord "Heeeeey"
{- ghci 15 -}
map ((+3) . ord) "Heeeeey"
{- ghci 16 -}
:!hoogle --info "chr :: Int -> Char"
{- ghci 17 -}
map (chr . (+3) . ord) "Heeeeey"
{- ghci 18 -}
let { encode :: Int -> String -> String ; encode shift msg = map (chr . (+ shift) . ord) msg }
{- ghci 19 -}
encode 3 "Heeeeey"
{- ghci 20 -}
encode 4 "Heeeeey"
{- ghci 21 -}
encode 1 "abcd"
{- ghci 22 -}
encode 5 "Merry Christmas! Ho ho ho!"
{- ghci 23 -}
let decode shift msg = encode (negate shift) msg
{- ghci 24 -}
encode 3 "Im a little teapot"
{- ghci 25 -}
decode 3 "Lp#d#olwwoh#whdsrw"
{- ghci 26 -}
decode 5 . encode 5 $ "This is a sentence"
{- ghci 27 -}
import qualified Data.Set as S
{- ghci 28 -}
let text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
{- ghci 29 -}
let text2 = "The old man left his garbage can out " ++ "and now his trash is all over my lawn!"
{- ghci 30 -}
let set1 = S.fromList text1
{- ghci 31 -}
let set2 = S.fromList text2
{- ghci 32 -}
:t S.fromList
{- ghci 33 -}
set1
{- ghci 34 -}
:t set1
{- ghci 35 -}
set2
{- ghci 36 -}
S.intersection set1 set2
{- ghci 37 -}
S.difference set1 set2
{- ghci 38 -}
S.difference set2 set1
{- ghci 39 -}
S.union set1 set2
{- ghci 40 -}
S.empty
{- ghci 41 -}
S.null S.empty
{- ghci 42 -}
S.null $ S.fromList [3,4,5,5,4,3]
{- ghci 43 -}
S.size $ S.fromList [3,4,5,3,4,5]
{- ghci 44 -}
S.singleton 9
{- ghci 45 -}
S.insert 4 $ S.fromList [9,3,8,1]
{- ghci 46 -}
S.insert 8 $ S.fromList [5..10]
{- ghci 47 -}
S.delete 4 $ S.fromList [3,4,5,4,3,4,5]
{- ghci 48 -}
S.fromList [2,3,4] `S.isSubsetOf` S.fromList [1,2,3,4,5]
{- ghci 49 -}
S.fromList [1,2,3,4,5] `S.isSubsetOf` S.fromList [1,2,3,4,5]
{- ghci 50 -}
S.fromList [2,3,4,8] `S.isSubsetOf` S.fromList [1,2,3,4,5]
{- ghci 51 -}
S.fromList [1,2,3,4,5] `S.isProperSubsetOf` S.fromList [1,2,3,4,5]
{- ghci 52 -}
S.filter odd $ S.fromList [3,4,5,6,7,2,3,4]
{- ghci 53 -}
S.map (+1) $ S.fromList [3,4,5,6,7,2,3,4]
{- ghci 54 -}
let setNub xs = S.toList $ S.fromList xs
{- ghci 55 -}
setNub "HEY WHATS CRACKALACKIN"
{- ghci 56 -}
import Data.List
{- ghci 57 -}
nub "HEY WHATS CRACKALACKIN"
{- ghci 58 -}
:l Geometry
{- ghci 59 -}
sphereArea 4
{- ghci 60 -}
:l GeomAll
{- ghci 61 -}
Sphere.area 4
{- ghci 62 -}
Sphere.volume 4
{- ghci 63 -}
Cube.area 4
{- ghci 64 -}
Cube.volume 4
