module Tape where

import Prelude hiding (read)

import Alphabet

-- Tape data type
-- each tape has a left list, a right list and the current head
data Tape a = Tape [Alphabet a] (Alphabet a) [Alphabet a]

empty :: Tape a
empty = Tape (repeat Blank) Blank (repeat Blank)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:left) head right) = Tape left l (head:right)

moveRight :: Tape a -> Tape a
moveRight (Tape left head (r:right)) = Tape (head:left) r right

isAtStart :: Tape a -> Bool
isAtStart (Tape _ Start _) = True
isAtStart _                = False

rewind :: Tape a -> Tape a
rewind = until isAtStart moveLeft

fastForward :: Tape a -> Tape a
fastForward = until isAtStart moveRight

read :: Tape a -> Alphabet a
read (Tape _ head _) = head

write :: Alphabet a -> Tape a -> Tape a
write elem (Tape left _ right) = Tape left elem right

fromList :: [Alphabet a] -> Tape a
fromList = foldl (\t x -> moveRight (write x t)) empty

showSnippet :: Show a => Int -> Tape a -> String
showSnippet num (Tape left head right) = showList' left' ++ "->" ++ show head ++ "<-" ++ showList' right'
    where left' = reverse . take num $ left
          right' = take num right
          showList' [] = ""
          showList' [x] = "|" ++ show x ++ "|"
          showList' (x:xs) = "|" ++ show x ++ showList' xs

instance Show a => Show (Tape a) where
    show = showSnippet 19
