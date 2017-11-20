
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data Elem : a -> Vect k a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (contra : (value = x) -> Void) -> (contra1 : Elem value xs -> Void) -> Elem value (x :: xs) -> Void

isElem : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                        (Yes Refl) => Yes Here
                        (No contra) => case isElem value xs of
                                       (Yes prf) => Yes (There prf)
                                       (No contra) => No (notInTail contra contra1)
