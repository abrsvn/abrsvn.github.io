module Palindromes where

import BasicDef

recognize :: String -> Bool
recognize = \ xs ->
    null xs || xs == "a" || xs == "b" || xs == "c"
    || or [recognize ys | ["a",ys,"a"] <- splitN 3 xs]
    || or [recognize ys | ["b",ys,"b"] <- splitN 3 xs]
    || or [recognize ys | ["c",ys,"c"] <- splitN 3 xs]

generate = filter recognize (generateAll alphabet)
  where alphabet = ['a','b','c']

parse :: String -> [ParseTree String String]
parse = \ xs ->
    [EmptyTree | null xs ]
 ++ [Leaf "a"  | xs == "a"]
 ++ [Leaf "b"  | xs == "b"]
 ++ [Leaf "c"  | xs == "c"]
 ++ [Branch "A-pair" [Leaf "a", t, Leaf "a"] | ["a",ys,"a"] <- splitN 3 xs, t <- parse ys]
 ++ [Branch "B-pair" [Leaf "b", t, Leaf "b"] | ["b",ys,"b"] <- splitN 3 xs, t <- parse ys]
 ++ [Branch "C-pair" [Leaf "c", t, Leaf "c"] | ["c",ys,"c"] <- splitN 3 xs, t <- parse ys]
