-- Syntax of propositional logic

:l PropLsyn

:t Ng
:t Cnj
:t Dsj

-- the 2 formulas below are predefined in the PropLsyn file as shown below:

{-
form1, form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]
-}

form1
form2

-- we can define more formulas

let form3 = P "jake likes chocolate"
form3

let form4 = P "sam likes vanilla"
form4

let form5 = Ng form3
form5

let form6 = Ng $ Dsj [form3,form4]
form6

let form7 = Ng $ Cnj [form5,form6]
form7


-- Semantics of propositional logic

:l PropLsem
:{
let
form3 = P "jake likes chocolate"
form4 = P "sam likes vanilla"
form5 = Ng form3
form6 = Ng $ Dsj [form3,form4]
form7 = Ng $ Cnj [form5,form6]
:}

-- we have a function that extracts the names of the atomic propositions from any formula

form5
propNames form5

form7
propNames form7

-- we have another function that generates all the possible valuations / models for the atomic propositions in any formula

allVals form5
allVals form7

-- let's take form7 and choose one model

let models_form7 = allVals form7
models_form7
let model2_form7 = models_form7 !! 2
model2_form7

-- and now let's evaluate form7 relative to this model

eval model2_form7 form7

-- note that evaluating relative to the null model gives an error

eval [] form7

-- we also get an error if we have info only about some of the atomic propositions

eval [("jake likes chocolate",False)] form7

-- we can check if a formula is a tautology (true in all models), satisfiable (true in at least one model) or a contradition (unsatisfiable)

tautology form7
tautology $ Dsj [form7, Ng form7]

satisfiable form7
satisfiable $ Dsj [form7, Ng form7]
satisfiable $ Cnj [form7, Ng form7]

contradiction form7
contradiction $ Dsj [form7, Ng form7]
contradiction $ Cnj [form7, Ng form7]

-- we can also test if a formula implies another formula (i.e., if there is a subset relation between the models that satisfy them)

implies form7 (Dsj [form7, Ng form7])
implies form7 (Cnj [form7, Ng form7])

-- finally, we can see the update contributed by each formula relative to a Context Set, i.e., relative to a set of models / valuations that are (currently) candidates for the actual model

update models_form7 form7
update [model2_form7] form7
update [] form7
