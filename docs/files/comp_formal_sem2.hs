{- ghci 1 -}
:l PredLsyn
{- ghci 2 -}
let form1 = Atom "left" [x]
{- ghci 3 -}
form1
{- ghci 4 -}
:t form1
{- ghci 5 -}
let form2 = Atom "hugged" [y,z]
{- ghci 6 -}
form2
{- ghci 7 -}
:t form2
{- ghci 8 -}
let form3 = Atom "it's raining" []
{- ghci 9 -}
form3
{- ghci 10 -}
:t form3
{- ghci 11 -}
x
{- ghci 12 -}
:t x
{- ghci 13 -}
:t Var
{- ghci 14 -}
let u = Var "u" []
{- ghci 15 -}
u
{- ghci 16 -}
:t u
{- ghci 17 -}
let u1 = Var "u" [1]
{- ghci 18 -}
u1
{- ghci 19 -}
:t u1
{- ghci 20 -}
let u2 = Var "u" [1,2,3,4]
{- ghci 21 -}
u2
{- ghci 22 -}
:t u2
{- ghci 23 -}
let form4 = Atom "hugged" [u1,u2]
{- ghci 24 -}
form4
{- ghci 25 -}
:t form4
{- ghci 26 -}
:t Eq
{- ghci 27 -}
let form5 = Eq u1 u2
{- ghci 28 -}
form5
{- ghci 29 -}
:t form5
{- ghci 30 -}
:t Neg
{- ghci 31 -}
let form6 = Neg form5
{- ghci 32 -}
form6
{- ghci 33 -}
:t form6
{- ghci 34 -}
:t Impl
{- ghci 35 -}
let form7 = Impl form6 form4
{- ghci 36 -}
form7
{- ghci 37 -}
:t form7
{- ghci 38 -}
:t Equi
{- ghci 39 -}
let form8 = Equi form6 form4
{- ghci 40 -}
form8
{- ghci 41 -}
:t form8
{- ghci 42 -}
:t Conj
{- ghci 43 -}
let form9 = Conj [form6,form4]
{- ghci 44 -}
form9
{- ghci 45 -}
:t form9
{- ghci 46 -}
:t Disj
{- ghci 47 -}
let form10 = Disj [form6,form4]
{- ghci 48 -}
form10
{- ghci 49 -}
:t form10
{- ghci 50 -}
:t Struct
{- ghci 51 -}
let t1 = Struct "mother_of" [u1]
{- ghci 52 -}
t1
{- ghci 53 -}
:t t1
{- ghci 54 -}
let form13 = Atom "hugged" [u1,t1]
{- ghci 55 -}
form13
{- ghci 56 -}
:t form13
{- ghci 57 -}
:t Forall
{- ghci 58 -}
:t Exists
{- ghci 59 -}
let form11 = Forall u1 $ Exists u2 $ Conj [form6,form4]
{- ghci 60 -}
form11
{- ghci 61 -}
:t form11
{- ghci 62 -}
let form12 = Forall u1 $ Exists u2 $ Impl form6 form4
{- ghci 63 -}
form12
{- ghci 64 -}
:t form12
{- ghci 65 -}
let form14 = Forall u1 $ Atom "hugged" [u1,t1]
{- ghci 66 -}
form14
{- ghci 67 -}
:t form14
{- ghci 68 -}
isVar u1
{- ghci 69 -}
isVar u2
{- ghci 70 -}
isVar t1
{- ghci 71 -}
varsInTerm u1
{- ghci 72 -}
t1
{- ghci 73 -}
varsInTerm t1
{- ghci 74 -}
let t2 = Struct "gift_from_to" [u2,t1]
{- ghci 75 -}
t2
{- ghci 76 -}
varsInTerm t2
{- ghci 77 -}
:l PredLsem
{- ghci 78 -}
let form1 = Atom "laugh" [x]
{- ghci 79 -}
form1
{- ghci 80 -}
:t form1
{- ghci 81 -}
eval entities int0 ass0 form1
{- ghci 82 -}
Neg form1
{- ghci 83 -}
eval entities int0 ass0 (Neg form1)
{- ghci 84 -}
Conj [form1,Neg form1]
{- ghci 85 -}
eval entities int0 ass0 (Conj [form1,Neg form1])
{- ghci 86 -}
Disj [form1,Neg form1]
{- ghci 87 -}
eval entities int0 ass0 (Disj [form1,Neg form1])
{- ghci 88 -}
Forall x form1
{- ghci 89 -}
eval entities int0 ass0 (Forall x form1)
{- ghci 90 -}
Exists x form1
{- ghci 91 -}
eval entities int0 ass0 (Exists x form1)
{- ghci 92 -}
let form2 = Forall x (Atom "love" [x,x]) -- we test whether "love" is reflexive (it's not)
{- ghci 93 -}
form2
{- ghci 94 -}
:t form2
{- ghci 95 -}
eval entities int0 ass0 form2
{- ghci 96 -}
let form3 = Forall x ((Atom "boy" [x]) `Impl` (Exists y (Conj [Atom "girl" [y], Atom "love" [y,x]])))
{- ghci 97 -}
form3
{- ghci 98 -}
:t form3
{- ghci 99 -}
eval entities int0 ass0 form3
{- ghci 100 -}
let form4 = Atom "laugh" [Struct "Alice" []]
{- ghci 101 -}
form4
{- ghci 102 -}
:t form4
{- ghci 103 -}
evl entities int0 fint0 ass0 form4
{- ghci 104 -}
let form5 = Atom "laugh" [Struct "Dorothy" []]
{- ghci 105 -}
form5
{- ghci 106 -}
:t form5
{- ghci 107 -}
evl entities int0 fint0 ass0 form5
{- ghci 108 -}
Neg form5
{- ghci 109 -}
evl entities int0 fint0 ass0 (Neg form5)
