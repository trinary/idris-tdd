import Data.Vect

removeElem : DecEq a => (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem value (value :: ys) {prf = Here}  = ys
removeElem value {n = Z} (y :: []) {prf = There later} = absurd later
removeElem value {n = (S k)} (y :: ys) {prf = There later} = y :: removeElem value ys

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)
