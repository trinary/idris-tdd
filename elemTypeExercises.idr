
-- data List : (elem : Type) -> Type where
--   Nil : List elem
--   (::) : (x : elem) -> (xs : List elem) -> List elem

listElem : Eq ty => (value : ty) -> (xs : List ty) -> Bool
listElem value [] = False
listElem value (x :: xs) = case value == x of
                              False => listElem value xs
                              True => True
