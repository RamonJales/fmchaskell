module Maybe where

import Prelude hiding(Maybe, Nothing, Just)

{-# LANGUAGE GADTs #-}

data Maybe a where
    Nothing :: Maybe a
    Just :: a -> Maybe a
    deriving ( Eq , Show )
