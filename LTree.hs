module LBinTree where 
import Prelude
import Dir

-- Esse tipo de árvore é o que comumente vemos em estrutura de dados. A árvore carrega informação em cada nó. E uma folha é um nó sem filhos.

type Path = [Dir]

{-# LANGUAGE GADTs #-}

data LT a b where
    Tip :: a -> LT a b
    Fork ::  b -> LT a b -> LT a b -> LT a b
    deriving (Eq , Show)

