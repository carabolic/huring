module Util where

fstOf3 :: (a, b, c) -> a
fstOf3 (x, _, _) = x

sndOf3 :: (a, b, c) -> b
sndOf3 (_, x, _) = x

thdOf3 :: (a, b, c) -> c
thdOf3 (_, _, x) = x

