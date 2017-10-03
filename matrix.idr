import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

createEmpties2 : Vect n (Vect 0 a)
createEmpties2 {n = Z} = []
createEmpties2 {n = (S k)} = [] :: createEmpties2


transposeHelper : (x : Vect n elem) -> (xsTrans : Vect n (Vect k elem)) -> Vect n (Vect (S k) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans

addMat : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMat [] [] = []
addMat (x :: xs) (y :: ys) = zipWith (+) x y :: addMat xs ys
