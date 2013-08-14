module Tape where

import TapeAlphabet

-- Tape data type
-- each tape has a left list, a right list and the current head
data Tape a = Tape [a] a [a]

empty :: TapeAlphabet a => Tape a
empty = Tape (repeat blank) blank (repeat blank)

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:left) head right) = Tape left l (head:right)

moveRight :: Tape a -> Tape a
moveRight (Tape left head (r:right)) = Tape (head:left) r right

read :: Tape a -> a
read (Tape _ head _) = head

write :: a -> Tape a -> Tape a
write elem (Tape left _ right) = Tape left elem right

showSnippet :: Show a => Int -> Tape a -> String
showSnippet num (Tape left head right) = showList' left' ++ "->" ++ show head ++ "<-" ++ showList' right'
    where left' = reverse . take num $ left
          right' = take num right
          showList' [] = ""
          showList' [x] = "|" ++ show x ++ "|"
          showList' (x:xs) = "|" ++ show x ++ showList' xs

instance Show a => Show (Tape a) where
    show = showSnippet 19
