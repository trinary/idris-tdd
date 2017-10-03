import Data.Vect
--
-- allLengths : List String -> List Nat
-- allLengths [] = []
-- allLengths (first :: words) = length first :: allLengths words

total allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y
