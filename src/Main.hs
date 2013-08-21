module Main where

import Alphabet
import Movement
import State
import qualified Tape as T
import TuringMachine as TM

data Bin = Zero | One
         deriving (Eq, Ord, Enum)         

instance Show Bin where
    show Zero = "0"
    show One  = "1"

-- |Replace every symbol in the input tape with '1'
transitionFunction :: State Char -> Alphabet Bin -> (State Char, Alphabet Bin, Movement)
transitionFunction SStart sym        = (State 'a', sym,        R)
transitionFunction (State 'a') Blank = (Halt,      Blank,      S)
transitionFunction (State 'a') _     = (State 'a', Symbol One, R)
transitionFunction _ _ = error "Length does not match!"

content = [Start, Symbol Zero, Symbol Zero, Symbol Zero, Symbol Zero, Symbol Zero]

tape = iterate T.moveLeft (T.fromList content) !! 6

machine = TM.init tape transitionFunction

main :: IO ()
main = do
    putStrLn (show tape)
    putStrLn (show (getOutput (compute machine)))
    putStrLn "ready!"
