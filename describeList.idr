import Data.List

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] form = ?describeHelper_rhs_1
describeHelper (x :: xs) form = ?describeHelper_rhs_2
