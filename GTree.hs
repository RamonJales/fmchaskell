module GenTree where 
import Prelude
import Dir

type Path = [Dir]

{-# LANGUAGE GADTs #-}

data GT a where
    Node ::  a -> [GT a] -> GT a
    deriving (Eq , Show)

-- gtmap :: (a -> a) -> (GT a -> GT a)
-- gtmap f (Node x []) = f x
-- gtmap f (Node x t:ts) = Node (f x) ((map f t):(gtmap f ts))
