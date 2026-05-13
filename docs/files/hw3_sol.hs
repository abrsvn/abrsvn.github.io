{- ghci 1 -}
let { split :: [a] -> [([a],[a])] ; split [] = [([],[])] ; split (x:xs) = ([],x:xs) : [(x:ys,zs) | (ys,zs) <- split xs] }
{- ghci 2 -}
split [1..5]
{- ghci 3 -}
let { perm :: [a] -> [[a]] ; perm [] = [[]] ; perm (x:xs) = [ys ++ [x] ++ zs | perm_xs <- perm xs, (ys,zs) <- split perm_xs] }
{- ghci 4 -}
perm [1..5]
{- ghci 5 -}
length (perm [1..5])
{- ghci 6 -}
let text = "Pierre Vinken , 61 years old , will join the board " ++ "as a nonexecutive director Nov. 29 .\nMr. Vinken " ++ "is chairman of Elsevier N.V. , the Dutch publishing group ."
{- ghci 7 -}
let ws_sent2 = words ((lines text) !! 1)
{- ghci 8 -}
take 10 (perm ws_sent2)
{- ghci 9 -}
take 5 (drop 500 (perm ws_sent2))
{- ghci 10 -}
take 5 (drop 20000 (perm ws_sent2))
