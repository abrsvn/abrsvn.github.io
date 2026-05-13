-- THE SYNTAX OF OUR ENGLISH FRAGMENT

-- same as before, we had one determiner in there -- "most" -- for which we didn't have a semantics before

:l EF1syn
:i DET

-- SEMANTICS

:l EF2sem

-- we define an direct, compositional interpretation for our Eng. fragment
-- expressions of Eng. are interpreted as higher-order functions of various types; the two basic types are Entity (e in Montague semantics) and Bool (t in Montague semantics)

:i Entity
:i Bool

-- this is the interpretation of CNs

:t intCN Boy
:t intCN

:i intCN Boy

-- this is the interpretation of proper names

:t intNP ALICE

-- determiners and NP headed by determiners have translations of the expected Montague-style types

:t intDET Every
:t intDET Most
:t intNP $ NP1 Every Boy
:t intNP $ NP1 Most Sword

-- the translations for VPs containing intrasitive, transitive and ditransive verbs have the expected Montagovian form

:t intVP Laughed
:t intVP $ VP1 Helped (NP1 Every Boy)
:t intVP $ VP2 Gave (NP1 Every Boy) (NP1 A Sword)

-- we can now translate full sentences

:t intSent $ Sent (NP1 No Girl) Laughed
:t intSent $ Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
:t intSent $ Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))

-- finally, restrictive relative clauses with a subject or object gap are translated in the expected Montagovian way

:t intRCN $ RCN1 Boy That Laughed
:t intNP $ NP2 Every (RCN1 Boy That Laughed)
:t intSent $ Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled

:t intRCN $ RCN2 Boy That (NP1 A Girl) Loved
:t intNP $ NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
:t intSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled


-- the interpretation of full sentences yields truth values, for example ...
-- the set of boys in the model is {LittleMook,Atreyu}
-- the set of girls in the model is {SnowWhite,Alice,Dorothy,Goldilocks}
-- the set of love-pairs in the model is {(Atreyu,Ellie),(Bob,SnowWhite),(Remmy,SnowWhite),(SnowWhite,LittleMook)}
-- the set of smilers in the model is {Alice,Bob,Cyrus,Dorothy,Ellie,Fred,Goldilocks,LittleMook}
-- therefore, "Every boy that a girl loved smiled" is true b/c LittleMook is the only boy loved by a girl and LittleMook is in the set of smilers

intSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled

-- ... and "No boy that a girl loved smiled" is false

intSent $ Sent (NP2 No (RCN2 Boy That (NP1 A Girl) Loved)) Smiled

-- an example using name constants:

intSent $ Sent SNOWWHITE (VP1 Loved LITTLEMOOK)

-- more examples (from the "Comp. Sem." textbook)

intSent $ Sent (NP1 The Princess) Laughed
intSent $ Sent (NP1 The Giant) Shuddered
intSent $ Sent (NP1 A Dwarf) Cheered
intSent $ Sent (NP1 No Wizard) Laughed
intSent $ Sent (NP1 A Dwarf) (VP1 Defeated (NP1 A Giant))
