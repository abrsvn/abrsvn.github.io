{- ghci 1 -}
:l L0syn
{- ghci 2 -}
:t P1
{- ghci 3 -}
:t P2
{- ghci 4 -}
:t Ng
{- ghci 5 -}
:t Cnj
{- ghci 6 -}
:t Dsj
{- ghci 7 -}
let form1 = P1 HasMustache Noam
{- ghci 8 -}
form1
{- ghci 9 -}
let form2 = P2 Loves John Dick
{- ghci 10 -}
form2
{- ghci 11 -}
let form3 = Ng form1
{- ghci 12 -}
form3
{- ghci 13 -}
let form4 = Ng $ Dsj [form1,form2]
{- ghci 14 -}
form4
{- ghci 15 -}
let form5 = Ng $ Cnj [form3,form4]
{- ghci 16 -}
form5
{- ghci 17 -}
:l L0sem
{- ghci 18 -}
entities
{- ghci 19 -}
entityPairs
{- ghci 20 -}
:t nameExp
{- ghci 21 -}
:t pred1Exp
{- ghci 22 -}
:t pred2Exp
{- ghci 23 -}
:t nameInt
{- ghci 24 -}
:t pred1Int
{- ghci 25 -}
:t pred2Int
{- ghci 26 -}
powerset [1..3]
{- ghci 27 -}
let exampleInt1 = allVals !! 0
{- ghci 28 -}
:t exampleInt1 (NameExp Dick)
{- ghci 29 -}
:t exampleInt1 (Pred1Exp HasMustache)
{- ghci 30 -}
:t exampleInt1 (Pred2Exp Knows)
{- ghci 31 -}
:t nameInt (exampleInt1 (NameExp Dick))
{- ghci 32 -}
:t pred1Int (exampleInt1 (Pred1Exp HasMustache))
{- ghci 33 -}
:t pred2Int (exampleInt1 (Pred2Exp Knows))
{- ghci 34 -}
nameInt (exampleInt1 (NameExp Dick))
{- ghci 35 -}
nameInt (exampleInt1 (NameExp Noam))
{- ghci 36 -}
nameInt (exampleInt1 (NameExp John))
{- ghci 37 -}
nameInt (exampleInt1 (NameExp Muhammad))
{- ghci 38 -}
[x | x <- entities, pred1Int (exampleInt1 (Pred1Exp HasMustache)) x]
{- ghci 39 -}
[x | x <- entities, pred1Int (exampleInt1 (Pred1Exp IsBald)) x]
{- ghci 40 -}
[(x,y) | x <- entities, y <- entities, pred2Int (exampleInt1 (Pred2Exp Knows)) y x]
{- ghci 41 -}
[(x,y) | x <- entities, y <- entities, pred2Int (exampleInt1 (Pred2Exp Loves)) y x]
{- ghci 42 -}
let exampleInt2 = allVals !! 2000009
{- ghci 43 -}
[x | x <- entities, pred1Int (exampleInt2 (Pred1Exp HasMustache)) x]
{- ghci 44 -}
[x | x <- entities, pred1Int (exampleInt2 (Pred1Exp IsBald)) x]
{- ghci 45 -}
[(x,y) | x <- entities, y <- entities, pred2Int (exampleInt2 (Pred2Exp Knows)) y x]
{- ghci 46 -}
[(x,y) | x <- entities, y <- entities, pred2Int (exampleInt2 (Pred2Exp Loves)) y x]
{- ghci 47 -}
eval exampleInt1 (P2 Loves Noam Noam)
{- ghci 48 -}
eval exampleInt2 (P2 Loves Noam Noam)
{- ghci 49 -}
eval exampleInt1 $ Ng (P2 Loves Noam Noam)
{- ghci 50 -}
eval exampleInt2 $ Ng (P2 Loves Noam Noam)
{- ghci 51 -}
2^4 * 2^4 * 2^16 * 2^16
{- ghci 52 -}
satisfiable (P1 HasMustache John)
{- ghci 53 -}
satisfiable $ Dsj [P1 HasMustache John, Ng (P1 HasMustache John)]
{- ghci 54 -}
satisfiable $ P2 Loves Noam Noam
{- ghci 55 -}
satisfiable $ Ng (P2 Loves Noam Noam)
{- ghci 56 -}
let cs1 = take 2000010 allVals
{- ghci 57 -}
length cs1
{- ghci 58 -}
let cs2 = update cs1 (P2 Loves Noam Noam)
{- ghci 59 -}
length cs2
