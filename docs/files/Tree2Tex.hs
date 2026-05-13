module Tree2Tex where

import Data.List
import BasicDef
import Lexicon

tree2tex :: (ParseTree Cat Cat) -> String
tree2tex (Leaf cat) = "\\textsc{" ++ (phon cat) ++ "}" ++ "\\textbf{::}" ++ (catLabel cat)
-- tree2tex (Leaf cat) = "\\textsc{" ++ (phon cat) ++ "}\\textbf{::}" ++ (catLabel cat) ++ show (fs cat)
tree2tex (Branch cat subtrees) = "[." ++ (catLabel cat) ++ " " ++ trees2tex subtrees ++ "]"

trees2tex :: [ParseTree Cat Cat] -> String
trees2tex []     = ""
trees2tex (x:xs) = tree2tex x ++ " " ++ trees2tex xs

-- add \usepackage{qtree} in the preamble of the lhs file

writeTree2Tex :: (ParseTree Cat Cat) -> String
writeTree2Tex ts = "\\Tree " ++ (tree2tex ts)
