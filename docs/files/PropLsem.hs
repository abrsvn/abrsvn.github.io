module PropLsem where

import Data.List
import PropLsyn

propNames :: Form -> [String]
propNames (P name) = [name]
propNames (Ng f)   = propNames f
propNames (Cnj fs) = sort . nub . concat $ map propNames fs
propNames (Dsj fs) = sort . nub . concat $ map propNames fs

genVals :: [String] -> [[(String,Bool)]]
genVals [] = [[]]
genVals (name:names) = map ((name,True) :) (genVals names)
                    ++ map ((name,False):) (genVals names)

allVals :: Form -> [[(String,Bool)]]
allVals = genVals . propNames

eval :: [(String,Bool)] -> Form -> Bool
eval [] (P name)    = error ("no info about " ++ show name)
eval ((atom,value):xs) (P name)
     | name == atom = value
     | otherwise    = eval xs (P name)
eval xs (Ng f)      = not (eval xs f)
eval xs (Cnj fs)    = all (eval xs) fs
eval xs (Dsj fs)    = any (eval xs) fs

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies f1 f2 = contradiction (Cnj [f1,Ng f2])

update :: [[(String,Bool)]] -> Form -> [[(String,Bool)]]
update vals f = [ v | v <- vals, eval v f ]
