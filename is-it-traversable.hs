import Data.List
import System.IO

-- https://codegolf.stackexchange.com/questions/259083/is-it-traversable
isItTraversable :: (Integral a) => a -> [a] -> Bool
isItTraversable stamina _
    | stamina < 0 = False
isItTraversable stamina [] = True
isItTraversable stamina [p] = True
isItTraversable stamina (p:c:hs)
    | p == c = isItTraversable stamina (c:hs)
    | p > c = isItTraversable (if p-c > stamina then stamina - 1 else stamina) $ c:hs
    | p < c = ((c-p) <= stamina) && isItTraversable stamina (c:hs)
isItTraversable _ _ = False