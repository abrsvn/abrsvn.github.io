{- ghci 1 -}
:l EF1syn
{- ghci 2 -}
:t ALICE
{- ghci 3 -}
:t Every
{- ghci 4 -}
:t Boy
{- ghci 5 -}
:t NP1 Every Boy
{- ghci 6 -}
:t NP1 A Sword
{- ghci 7 -}
:t Fake
{- ghci 8 -}
:t RCN3 Fake Sword
{- ghci 9 -}
:t NP2 No (RCN3 Fake Sword)
{- ghci 10 -}
:t Laughed
{- ghci 11 -}
:t VP1 Helped (NP1 Every Boy)
{- ghci 12 -}
:t VP2 Gave (NP1 Every Boy) (NP1 A Sword)
{- ghci 13 -}
:t Sent (NP1 No Girl) Laughed
{- ghci 14 -}
:t Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
{- ghci 15 -}
:t Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))
{- ghci 16 -}
:t NP2 Every (RCN1 Boy That Laughed)
{- ghci 17 -}
:t Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled
{- ghci 18 -}
:t NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
{- ghci 19 -}
:t Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled
{- ghci 20 -}
:l EF1sem
{- ghci 21 -}
:i Term
{- ghci 22 -}
:i Formula
{- ghci 23 -}
:t lfCN Boy
{- ghci 24 -}
:t lfCN
{- ghci 25 -}
:i lfCN Boy
{- ghci 26 -}
:t lfNP ALICE
{- ghci 27 -}
:t lfDET Every
{- ghci 28 -}
:t lfNP $ NP1 Every Boy
{- ghci 29 -}
:t lfNP $ NP1 A Sword
{- ghci 30 -}
:t lfVP Laughed
{- ghci 31 -}
:t lfVP $ VP1 Helped (NP1 Every Boy)
{- ghci 32 -}
:t lfVP $ VP2 Gave (NP1 Every Boy) (NP1 A Sword)
{- ghci 33 -}
:t lfSent $ Sent (NP1 No Girl) Laughed
{- ghci 34 -}
:t lfSent $ Sent (NP1 No Girl) (VP1 Helped (NP1 Every Boy))
{- ghci 35 -}
:t lfSent $ Sent (NP1 No Girl) (VP2 Gave (NP1 Every Boy) (NP1 A Sword))
{- ghci 36 -}
:t lfRCN $ RCN1 Boy That Laughed
{- ghci 37 -}
:t lfNP $ NP2 Every (RCN1 Boy That Laughed)
{- ghci 38 -}
:t lfSent $ Sent (NP2 Every (RCN1 Boy That Laughed)) Smiled
{- ghci 39 -}
:t lfRCN $ RCN2 Boy That (NP1 A Girl) Loved
{- ghci 40 -}
:t lfNP $ NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)
{- ghci 41 -}
:t lfSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled
{- ghci 42 -}
:l EF1sem
{- ghci 43 -}
eval entities int0 ass0 (lfSent $ Sent (NP2 Every (RCN2 Boy That (NP1 A Girl) Loved)) Smiled)
{- ghci 44 -}
eval entities int0 ass0 (lfSent $ Sent (NP2 No (RCN2 Boy That (NP1 A Girl) Loved)) Smiled)
{- ghci 45 -}
evl entities int0 fint0 ass0 (lfSent $ Sent SNOWWHITE (VP1 Loved LITTLEMOOK))
