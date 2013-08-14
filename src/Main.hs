-- | Main entry point to the application.

module Main where

import Tape
import TapeAlphabet

data Alphabet = Start | Blank | Symbol Int

-- | The main entry point.
main :: IO ()
main = do
    putStrLn (show (moveLeft (iterate (moveRight . write (Symbol 1)) empty !! 10)))


instance Show Alphabet where
    show Start        = ">"
    show Blank        = "[]"
    show (Symbol num) = show num

instance TapeAlphabet Alphabet where
    start = Start
    blank = Blank