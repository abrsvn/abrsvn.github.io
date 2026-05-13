module L0sem where

import Data.List
import L0syn

data Entity = Nixon | Chomsky | Mitchell | Ali deriving (Eq,Show,Enum,Bounded)

entities :: [Entity]
entities = [minBound..maxBound]

entityPairs :: [(Entity,Entity)]
entityPairs = [(e1,e2) | e1 <- entities, e2 <- entities]

data BasicExp = NameExp { nameExp :: Name } | Pred1Exp { pred1Exp :: Pred1 } | Pred2Exp { pred2Exp :: Pred2 }
data BasicInt = NameInt { nameInt :: Entity } | Pred1Int { pred1Int :: (Entity -> Bool) } | Pred2Int { pred2Int :: (Entity -> Entity -> Bool) }

list2Pred1Value :: [Entity] -> (Entity -> Bool)
list2Pred1Value xs = \ x -> elem x xs
list2Pred2Value :: [(Entity,Entity)] -> (Entity -> Entity -> Bool)
list2Pred2Value xs = \ y x -> elem (x,y) xs

basicIntFun :: [Entity] -> [Entity] -> [(Entity,Entity)] -> [(Entity,Entity)] -> BasicExp -> BasicInt
basicIntFun _ _ _ _ (NameExp Dick) = NameInt Nixon
basicIntFun _ _ _ _ (NameExp Noam) = NameInt Chomsky
basicIntFun _ _ _ _ (NameExp John) = NameInt Mitchell
basicIntFun _ _ _ _ (NameExp Muhammad) = NameInt Ali
basicIntFun xs _ _ _ (Pred1Exp HasMustache) = Pred1Int (list2Pred1Value xs)
basicIntFun _ xs _ _ (Pred1Exp IsBald) = Pred1Int (list2Pred1Value xs)
basicIntFun _ _ xs _ (Pred2Exp Knows) = Pred2Int (list2Pred2Value xs)
basicIntFun _ _ _ xs (Pred2Exp Loves) = Pred2Int (list2Pred2Value xs)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) p ++ p where p = powerset xs

allVals :: [(BasicExp -> BasicInt)]
allVals = [basicIntFun x1s x2s y1s y2s | x1s <- powerset entities, x2s <- powerset entities, y1s <- powerset entityPairs, y2s <- powerset entityPairs]

eval :: (BasicExp -> BasicInt) -> Form -> Bool
eval m (P1 pred1 name) = (pred1Int $ m (Pred1Exp pred1)) (nameInt $ m (NameExp name))
eval m (P2 pred2 name1 name2) = (pred2Int $ m (Pred2Exp pred2)) (nameInt $ m (NameExp name1)) (nameInt $ m (NameExp name2))
eval m (Ng f) = not (eval m f)
eval m (Cnj fs) = all (eval m) fs
eval m (Dsj fs) = any (eval m) fs

tautology :: Form -> Bool
tautology f = all (\ m -> eval m f) allVals

satisfiable :: Form -> Bool
satisfiable f = any (\ m -> eval m f) allVals

contradiction :: Form -> Bool
contradiction = not . satisfiable

implies :: Form -> Form -> Bool
implies f1 f2 = contradiction (Cnj [f1,Ng f2])

update :: [(BasicExp -> BasicInt)] -> Form -> [(BasicExp -> BasicInt)]
update vals f = [ m | m <- vals, eval m f ]
