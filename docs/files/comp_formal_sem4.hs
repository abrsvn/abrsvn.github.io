{- ghci 1 -}
:l EF1syn
{- ghci 2 -}
:i DET
{- ghci 3 -}
:l EF2sem
{- ghci 4 -}
:i Entity
{- ghci 5 -}
:i Bool
{- ghci 6 -}
:t intCN Boy
{- ghci 7 -}
:t intCN
{- ghci 8 -}
:i intCN Boy
{- ghci 9 -}
:t intNP ALICE
{- ghci 10 -}
:t intDET Every
{- ghci 11 -}
:t intDET Most
{- ghci 12 -}
:t intNP $ NP1 Every Boy
{- ghci 13 -}
:t intNP $ NP1 Most Sword
{- ghci 14 -}
:t intVP Laughed
{- ghci 15 -}
:t intVP $ VP1 Helped (NP1 Every Boy)
{- ghci 16 -}
:t intVP $ VP2 Gave (NP1 Every Boy) (NP1 A Sword)
{- ghci 17 -}
:t intSent $ Sent (NP1 No Girl) Laughed
{- ghci 18 -}
:t intSent $ Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
{- ghci 19 -}
:t intSent $ Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))
{- ghci 20 -}
:t intRCN $ RCN1 Boy That Laughed
{- ghci 21 -}
:t intNP $ NP2 Every (RCN1 Boy That Laughed)
{- ghci 22 -}
:t intSent $ Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled
{- ghci 23 -}
:t intRCN $ RCN2 Boy That (NP1 A Girl) Loved
{- ghci 24 -}
:t intNP $ NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
{- ghci 25 -}
:t intSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled
{- ghci 26 -}
intSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled
{- ghci 27 -}
intSent $ Sent (NP2 No (RCN2 Boy That (NP1 A Girl) Loved)) Smiled
{- ghci 28 -}
intSent $ Sent SNOWWHITE (VP1 Loved LITTLEMOOK)
{- ghci 29 -}
intSent $ Sent (NP1 The Princess) Laughed
{- ghci 30 -}
intSent $ Sent (NP1 The Giant) Shuddered
{- ghci 31 -}
intSent $ Sent (NP1 A Dwarf) Cheered
{- ghci 32 -}
intSent $ Sent (NP1 No Wizard) Laughed
{- ghci 33 -}
intSent $ Sent (NP1 A Dwarf) (VP1 Defeated (NP1 A Giant))
