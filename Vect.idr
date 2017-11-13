
data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip [] ys = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

(Eq ty) => Eq (Vect Nat ty) where

-- vectTake : (i : Nat) -> Vect n a -> Maybe Vect i a
-- vectTake Z xs = Just []
-- vectTake {n} i (x :: xs) = case integerToFin i n of
--                   Nothing => Nothing
--                   (Just idx) => Just (x :: (vectTake (i - 1) xs))
