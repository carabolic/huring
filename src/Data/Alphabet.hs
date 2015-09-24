module Alphabet where

data Alphabet a = Start | Blank | Symbol a

instance Show a => Show (Alphabet a) where
    show Start      = ">"
    show Blank      = "_"
    show (Symbol s) = show s
