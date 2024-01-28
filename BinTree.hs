module BinTree where 
import Prelude
import Dir

type Path = [Dir]

{-# LANGUAGE GADTs #-}

data BT a where
    Tip :: a -> BT a
    Fork ::  BT a -> BT a -> BT a
    deriving (Eq , Show)

-- tips = leaves
tips :: BT a -> Int
tips (Tip _) = 1
tips (Fork l r) = tips l + tips r

-- nodes = forks
forks :: BT a -> Int
forks (Tip _) = 0
forks (Fork l r) = 1 + forks l + forks r

deapth :: BT a -> Int
deapth (Tip _) = 0
deapth (Fork l r) = 1 + (max (deapth l) (deapth r))

flat :: BT a -> [a]
flat (Tip x) = [x]
flat (Fork l r) = flat l ++ flat r

fetch :: Path -> BT a -> Maybe a
fetch (L : ds) (Fork l _) = fetch ds l
fetch (R : ds) (Fork _ r) = fetch ds r
fetch [] (Tip x) = Just x
fetch _ _ = Nothing


search :: Eq a => a -> BT a -> [Path]
search w (Tip x) = if (w == x) then [[]] else []
search w (Fork l r) = 
    let 
        (lpath, rpath) = (search w l, search w r)
    in
        (map (L:) lpath) ++ (map (R:) rpath)
    
