{- ghci 1 -}
let { nouns, suffixes, adjectives :: [String] ; nouns = ["color", "faith", "use"] ; suffixes = ["ful", "less"] ; adjectives = [ x ++ y | x <- nouns, y <- suffixes] }
{- ghci 2 -}
nouns
{- ghci 3 -}
suffixes
{- ghci 4 -}
adjectives
{- ghci 5 -}
let text = "Pierre Vinken , 61 years old , will join the board " ++ "as a nonexecutive director Nov. 29 . Mr. Vinken " ++ "is chairman of Elsevier N.V. , the Dutch publishing group ."
{- ghci 6 -}
text
{- ghci 7 -}
let text_words = words text
{- ghci 8 -}
text_words
{- ghci 9 -}
length text_words
{- ghci 10 -}
let length_words = map length text_words
{- ghci 11 -}
length_words
{- ghci 12 -}
let min_length = minimum length_words
{- ghci 13 -}
min_length
{- ghci 14 -}
let max_length = maximum length_words
{- ghci 15 -}
max_length
{- ghci 16 -}
[word | word <- text_words, length word == min_length]
{- ghci 17 -}
[word | word <- text_words, length word == max_length]
{- ghci 18 -}
sum length_words
{- ghci 19 -}
length length_words
{- ghci 20 -}
fromIntegral (sum length_words) / fromIntegral (length length_words)
{- ghci 21 -}
let { drop' :: Int -> [a] -> [a] ; drop' _ [] = [] ; drop' n xs@(y:ys) | n <= 0 = xs | n > 0 = drop' (n-1) ys }
{- ghci 22 -}
drop' 5 "Supercalifragilisticexpialidocious"
{- ghci 23 -}
drop' 9 "Supercalifragilisticexpialidocious"
{- ghci 24 -}
drop' 0 "Supercalifragilisticexpialidocious"
{- ghci 25 -}
drop' (-3) "Supercalifragilisticexpialidocious"
{- ghci 26 -}
drop' 4 ""
{- ghci 27 -}
let { drop'' :: Int -> [a] -> [a] ; drop'' n xs = if n <= 0 || null xs then xs else drop'' (n-1) (tail xs) }
{- ghci 28 -}
drop'' 5 "Supercalifragilisticexpialidocious"
{- ghci 29 -}
drop'' 9 "Supercalifragilisticexpialidocious"
{- ghci 30 -}
drop'' 0 "Supercalifragilisticexpialidocious"
{- ghci 31 -}
drop'' (-3) "Supercalifragilisticexpialidocious"
{- ghci 32 -}
drop'' 4 ""
