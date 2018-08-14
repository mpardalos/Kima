module CommonTypes where

import Data.String

newtype Name = Name String deriving (Eq, Ord)

instance Show Name where
    show (Name str) = "{" ++ str ++ "}"

instance IsString Name where
    fromString = Name
    

data BinOp = Add | Sub | Div | Mul | Mod
    deriving Show