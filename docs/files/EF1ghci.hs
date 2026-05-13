-- THE SYNTAX OF OUR ENGLISH FRAGMENT

:l EF1syn

-- we have proper names

:t Alice

-- determiners and CNs that we can put together to form NPs

:t Every
:t Boy
:t NP1 Every Boy
:t NP1 A Sword

-- we can also add adjectives

:t Fake
:t RCN3 Fake Sword
:t NP2 No (RCN3 Fake Sword)

-- we also have intransitive verbs that form VPs directly, transitive verbs and ditransitive verbs

:t Laughed
:t VP1 Helped (NP1 Every Boy)
:t VP2 Gave (NP1 Every Boy) (NP1 A Sword)

-- we can form sentences with these NPs and VPs

:t Sent (NP1 No Girl) Laughed
:t Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
:t Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))

-- we have relative clauses in our fragment, both with a subject gap ...

:t NP2 Every (RCN1 Boy That Laughed)
:t Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled

-- ... and with a direct object gap

:t NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
:t Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled


-- SEMANTICS I: ENGLISH-to-FOL TRANSLATION

:l EF1sem

-- we define an indirect interpretation for our Eng. fragment, i.e., we define a (compositional) translation function from the sentences in our Eng. fragment into the first-order logic we defined previously
-- the Eng. sentences indirectly receive an interpretation: their meaning is the meaning of the FOL formulas they get translated into
-- our translation involves higher-order lambda terms exactly as it does in Montague's PTQ
-- however, those terms do not have an interpretation, the only model that we have is the FO model we had for FOL; we do this for convenience only, we could generalize our FOL model and assign interpretations to all the intermediate, higher-order lambda terms that are produced by our translation function


-- the translation function is compositionally defined based on the above syntax of the Eng. fragment
-- we have a translation function associated with every syntactic category, that is ...
-- for every syntactic tree whose mother node is of that syntactic category, its translation is a function of:
-- 1. the translations of its immediate daughters
-- 2. the way they are syntactically put together
-- (this is the textbook definition of compositionality)

-- our FOL syntax has 2 basic types of expressions: terms and formulas

:i Term
:i Formula

-- therefore, all the translations of Eng. expressions will be terms, formulas or higher-order functions over the domains of FOL terms and formulas
-- in extensional Montague semantics, we have two basic types e (for entities) and t (for truth values) based on which we define arbitrary higher-order functional types; Eng. expressions are compositionally translated into lambda terms of a basic or functional type
-- in our translation here, Term basically functions as type e and Formula is basically type t

-- let's take a CN, e.g., "boy": its Montagovian translation has type (et) (a function from entities to truth values)
-- in our implementation, its translation will be function from Terms to Formulas

:t lfCN Boy

-- and the translation function itself, i.e., lfCN, will be a function from CNs to Term->Formula functions

:t lfCN

-- incidentally, you can get info about where all these are defined like so

:i lfCN Boy

-- for example, the definition of lfCN in the EF1sem (English Fragment 1 Semantics) module is

{-
lfCN :: CN -> Term -> Formula
...
lfCN Boy      = \ t -> Atom "boy"      [t]
...
-}

-- the formulas on the rhs are FOL formulas and their interpretation is defined in the PredLsem module as follows:

{-
eval :: Eq a =>
    [a]              ->
    Interp a         ->
    Assignment a     ->
    Formula          -> Bool

eval domain i = eval' where
  eval' g (Atom str vs) = i str (map g vs)
  ...
-}

-- that is, we take the string "boy" and see what semantic is assigned to it by the interpretation i
-- we take the term t in the singleton list [t] that is the argument of "boy" and see what semantic value t is assigned by the variable assignment g
-- finally, we check whether the semantic value of t is in the semantic value of "boy"

-- we translate proper names as the Montagovian lifts of the corresponding FOL constant, i.e., their translation is of quantifier type

:t lfNP ALICE

{-
lfNP ALICE         = \ p -> p (Struct "Alice"      [])
-}

-- determiners and NP headed by determiners have translations of the expected Montague-style types

:t lfDET Every
:t lfNP $ NP1 Every Boy
:t lfNP $ NP1 A Sword

-- to avoid accidental binding of variables when we translate quantificational determiners, we are always careful to introduce a fresh variable -- we do this by defining three helper functions:
-- 1. bInLFs, which identifies the indices on the variables already present in the formulas we want to quantify over
-- 2. freshIndex, which produces a variable index that is different from any of the indices in an arbitrary list of indices
-- 3. fresh, which takes a list of predicates (functions from Terms to Formulas), and returns an fresh index, i.e., a variable index different from any of the current variable indices

-- the translations for VPs containing intrasitive, transitive and ditransive verbs have the expected Montagovian form

:t lfVP Laughed
:t lfVP $ VP1 Helped (NP1 Every Boy)
:t lfVP $ VP2 Gave (NP1 Every Boy) (NP1 A Sword)

-- we can now translate full sentences

:t lfSent $ Sent (NP1 No Girl) Laughed
:t lfSent $ Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
:t lfSent $ Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))

-- finally, restrictive relative clauses with a subject or object gap are translated in the expected Montagovian way

:t lfRCN $ RCN1 Boy That Laughed
:t lfNP $ NP2 Every (RCN1 Boy That Laughed)
:t lfSent $ Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled

:t lfRCN $ RCN2 Boy That (NP1 A Girl) Loved
:t lfNP $ NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
:t lfSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled


-- SEMANTICS II: EVALUATION IN A MODEL (MODEL CHECKING)

:l EF1sem

-- the set of boys in the model is {LittleMook,Atreyu}
-- the set of girls in the model is {SnowWhite,Alice,Dorothy,Goldilocks}
-- the set of love-pairs in the model is {(Atreyu,Ellie),(Bob,SnowWhite),(Remmy,SnowWhite),(SnowWhite,LittleMook)}
-- the set of smilers in the model is {Alice,Bob,Cyrus,Dorothy,Ellie,Fred,Goldilocks,LittleMook}
-- therefore, "Every boy that a girl loved smiled" is true b/c LittleMook is the only boy loved by a girl and LittleMook is in the set of smilers

eval entities int0 ass0 (lfSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled)

-- ... and "No boy that a girl loved smiled" is false

eval entities int0 ass0 (lfSent $ Sent (NP2 No (RCN2 Boy That (NP1 A Girl) Loved)) Smiled)

-- an example using name constants:

evl entities int0 fint0 ass0 (lfSent $ Sent SNOWWHITE (VP1 Loved LITTLEMOOK))
