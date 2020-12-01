module Util where

mapSnd f (x, y) = (x, f y)


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

third3 :: (a, b, c) -> c
third3 (_, _, a) = a

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

third4 :: (a, b, c, d) -> c
third4 (_, _, a, _) = a

curry3 :: ((a, b, c) -> t) -> a -> b -> c -> t
curry3 f a b c = f (a, b, c)

uncurry3 f (a, b, c) = f a b c
