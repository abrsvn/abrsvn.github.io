{- ghci 1 -}
:l PropLsyn
{- ghci 2 -}
:t Ng
{- ghci 3 -}
:t Cnj
{- ghci 4 -}
:t Dsj
{- ghci 5 -}
:t P
{- ghci 6 -}
form1
{- ghci 7 -}
form2
{- ghci 8 -}
let form3 = P "jake likes chocolate"
{- ghci 9 -}
form3
{- ghci 10 -}
let form4 = P "sam likes vanilla"
{- ghci 11 -}
form4
{- ghci 12 -}
let form5 = Ng form3
{- ghci 13 -}
form5
{- ghci 14 -}
let form6 = Ng $ Dsj [form3,form4]
{- ghci 15 -}
form6
{- ghci 16 -}
let form7 = Ng $ Cnj [form5,form6]
{- ghci 17 -}
form7
{- ghci 18 -}
:l PropLsem
{- ghci 19 -}
let { form3 = P "jake likes chocolate" ; form4 = P "sam likes vanilla" ; form5 = Ng form3 ; form6 = Ng $ Dsj [form3,form4] ; form7 = Ng $ Cnj [form5,form6] }
{- ghci 20 -}
form5
{- ghci 21 -}
propNames form5
{- ghci 22 -}
form7
{- ghci 23 -}
propNames form7
{- ghci 24 -}
form5
{- ghci 25 -}
allVals form5
{- ghci 26 -}
form7
{- ghci 27 -}
allVals form7
{- ghci 28 -}
let models_form7 = allVals form7
{- ghci 29 -}
models_form7
{- ghci 30 -}
let model3_form7 = models_form7 !! 2
{- ghci 31 -}
model3_form7
{- ghci 32 -}
eval model3_form7 form7
{- ghci 33 -}
form7
{- ghci 34 -}
eval [] form7
{- ghci 35 -}
eval [("jake likes chocolate",False)] form7
{- ghci 36 -}
:i not
{- ghci 37 -}
:! hoogle --info not
{- ghci 38 -}
:i all
{- ghci 39 -}
:! hoogle --info all
{- ghci 40 -}
:i any
{- ghci 41 -}
:! hoogle --info any
{- ghci 42 -}
form7
{- ghci 43 -}
model3_form7
{- ghci 44 -}
eval model3_form7 form7
{- ghci 45 -}
tautology form7
{- ghci 46 -}
tautology $ Dsj [form7, Ng form7]
{- ghci 47 -}
satisfiable form7
{- ghci 48 -}
satisfiable $ Dsj [form7, Ng form7]
{- ghci 49 -}
satisfiable $ Cnj [form7, Ng form7]
{- ghci 50 -}
contradiction form7
{- ghci 51 -}
contradiction $ Dsj [form7, Ng form7]
{- ghci 52 -}
contradiction $ Cnj [form7, Ng form7]
{- ghci 53 -}
implies form7 (Dsj [form7, Ng form7])
{- ghci 54 -}
implies form7 (Cnj [form7, Ng form7])
{- ghci 55 -}
models_form7
{- ghci 56 -}
update models_form7 form7
{- ghci 57 -}
length $ models_form7
{- ghci 58 -}
length $ update models_form7 form7
{- ghci 59 -}
[model3_form7]
{- ghci 60 -}
update [model3_form7] form7
{- ghci 61 -}
update [] form7
