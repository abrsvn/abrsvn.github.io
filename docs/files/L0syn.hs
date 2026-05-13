module L0syn where

import Data.List

data Name = Dick | Noam | John | Muhammad deriving (Eq,Show)
data Pred1 = HasMustache | IsBald deriving (Eq,Show)
data Pred2 = Knows | Loves deriving (Eq,Show)
data Form = P1 Pred1 Name | P2 Pred2 Name Name | Ng Form | Cnj [Form] | Dsj [Form] deriving Eq

instance Show Form where
    show (P1 pred1 name) = show pred1 ++ "(" ++ show name ++ ")"
    show (P2 pred2 name1 name2) = show pred2 ++ "(" ++ show name2 ++ "," ++ show name1 ++ ")"
    show (Ng  f)  = "~ " ++ show f
    show (Cnj fs) = "(" ++ intercalate " /\\ " (map show fs) ++ ")"
    show (Dsj fs) = "(" ++ intercalate " \\/ " (map show fs) ++ ")"
