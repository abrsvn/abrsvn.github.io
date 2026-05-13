-- First-order predicate logic -- Syntax

:l PredLsyn

-- here are a couple of examples of atomic formulas

let form1 = Atom "left" [x]
form1
:t form1

-- this should be read: form1 is a formula that contains at least one variable
-- we need to keep track of this "variable" component because we will later introduce constants and functional terms
-- we also need to keep track of this because we will want to collect the set of variables in a formula and decide whether they are free or bound

let form2 = Atom "hugged" [y,z]
form2
:t form2

let form3 = Atom "it's raining" []
form3
:t form3

-- the generic type when we don't have variables or constant / functional terms in a formula is simply a

-- the variables x, y, z (a subtype of terms) are predefined

x
:t x

-- we can define additional variables

let u = Var "u" []
u
:t u

let u1 = Var "u" [1]
u1
:t u1

let u2 = Var "u" [1,2,3,4]
u2
:t u2

-- and we can define new formulas in terms of these variables

let form4 = Atom "hugged" [u1,u2]
form4
:t form4

-- let's turn now to sentential operators

:t Eq
let form5 = Eq u1 u2
form5
:t form5

:t Neg
let form6 = Neg form5
form6
:t form6

:t Impl
let form7 = Impl form6 form4
form7
:t form7

:t Equi
let form8 = Equi form6 form4
form8
:t form8

:t Conj
let form9 = Conj [form6,form4]
form9
:t form9

:t Disj
let form10 = Disj [form6,form4]
form10
:t form10

-- as already indicated, terms subsume variables

-- we can build variables out of a variable name and an index

:t Var

-- we can build complex / structured terms out of a function name and a list of terms

:t Struct

let t1 = Struct "mother_of" [u1]
t1
:t t1

let form13 = Atom "hugged" [u1,t1]
form13
:t form13


-- finally, we turn to quantifiers

:t Forall
:t Exists

-- we can define new formulas like this

let form11 = Forall u1 $ Exists u2 $ Conj [form6,form4]
form11
:t form11

let form12 = Forall u1 $ Exists u2 $ Impl form6 form4
form12
:t form12

let form14 = Forall u1 $ Atom "hugged" [u1,t1]
form14
:t form14

-- we also have two convenience functions, one determining whether a term is a variable or not, another determining the set of variables in a term

isVar u1
isVar u2
isVar t1

varsInTerm u1
t1
varsInTerm t1

let t2 = Struct "gift_from_to" [u2,t1]
t2
varsInTerm t2


-- First-order predicate logic -- Semantics

:l PredLsem

-- let's take an atomic formula saying that x laughed

let form1 = Atom "laugh" [x]
form1
:t form1

-- we will interpret this formula relative to a basic interpretation function int0 (predefined in PredLsem.hs) and a variable assignment ass0 (also predefined in PredLsem.hs)
-- both int0 and ass0 are defined relative to the model in Model.hs, in particular relative to the domain of entities defined there
-- the set of laughing entities is laugh = {Alice,Goldilocks,E} and the denotation of the intransitive verb "Laugh" is this set, coded as follows

{-
int0 "laugh"   = \ [x]     -> laugh x
-}

-- the variable assignment ass0 maps all variables to entity Al:

{-
ass0 = \ v -> Alice
-}

-- the evaluation function takes a domain of entities (this is needed for quantified sentences), a basic interpretation function and an assignment and returns a truth value for the formula form1

eval entities int0 ass0 form1

-- we can evaluate the negation of form1 etc.

Neg form1
eval entities int0 ass0 (Neg form1)

Conj [form1,Neg form1]
eval entities int0 ass0 (Conj [form1,Neg form1])

Disj [form1,Neg form1]
eval entities int0 ass0 (Disj [form1,Neg form1])

-- we can evaluate the universal and existential generalizations of form1 too:

Forall x form1
eval entities int0 ass0 (Forall x form1)

Exists x form1
eval entities int0 ass0 (Exists x form1)

-- and here are a couple of examples of more complicated formulas

let form2 = Forall x (Atom "love" [x,x]) -- we test whether "love" is reflexive (it's not)
form2
:t form2
eval entities int0 ass0 form2

let form3 = Forall x ((Atom "boy" [x]) `Impl` (Exists y (Conj [Atom "girl" [y], Atom "love" [y,x]])))
form3
:t form3
eval entities int0 ass0 form3

-- we can also add 0-arity (name) constants

let form4 = Atom "laugh" [Struct "Alice" []]
form4
:t form4
evl entities int0 fint0 ass0 form4

let form5 = Atom "laugh" [Struct "Dorothy" []]
form5
:t form5
evl entities int0 fint0 ass0 form5

Neg form5
evl entities int0 fint0 ass0 (Neg form5)
