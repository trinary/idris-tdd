
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

data List : Type -> Type where
  Nil : List a
  (::) : a -> List a -> List a

data Elem : a -> Vect k a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later

isElem : DecEq ty => (value : ty) -> (xs : Vect n ty) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                        (Yes Refl) => Yes Here
                        (No notHere) => case isElem value xs of
                                       (Yes prf) => Yes (There prf)
                                       (No notThere) => No (notInTail notThere notHere)

listElem : Eq ty => (value : ty) -> (xs : List ty) -> Bool
