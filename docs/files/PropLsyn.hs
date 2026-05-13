module PropLsyn where

import Data.List

data Form =  P String | Ng Form | Cnj [Form] | Dsj [Form]
            deriving Eq

instance Show Form where
    show (P name) = name
    show (Ng  f)  = "~ " ++ show f
    show (Cnj fs) = "(" ++ intercalate " /\\ " (map show fs) ++ ")"
    show (Dsj fs) = "(" ++ intercalate " \\/ " (map show fs) ++ ")"

-- example formulas
form1, form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]
