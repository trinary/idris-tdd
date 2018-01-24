import Data.List

data ListLast : List a -> Type where
  Empty : ListLast []
  NotEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

total describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NotEmpty xs x) =
  "Non-Empty, initial portion = " ++ show xs

total listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NotEmpty [] x
                          NotEmpty ys y => NotEmpty (x :: ys) y

describeListEnd : List Int -> String
describeListEnd xs = describeHelper xs (listLast xs)

describeListEnd2 : List Int -> String
describeListEnd2 input with (listLast input)
  describeListEnd2 [] | Empty = "Empty"
  describeListEnd2 (xs ++ [x]) | (NotEmpty xs x) = "NotEmpty, initial = " ++ show xs


myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NotEmpty xs x) = x :: myReverse xs

data SplitList : List a -> Type where
  SplitNil : SplitList []
  SplitOne : SplitList [x]
  SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

total
splitList : (input : List a) -> SplitList input
splitList input = splitListHelp input input where
  splitListHelp : List a -> (input : List a) -> SplitList input
  splitListHelp _ [] = SplitNil
  splitListHelp _ [x] = SplitOne
  splitListHelp (_ :: _ :: counter) (item :: items) =
    case splitListHelp counter items of
      SplitNil => SplitOne
      SplitOne {x} => SplitPair [item] [x]
      SplitPair lefts rights => SplitPair (item :: lefts) rights
  splitListHelp _ items = SplitPair [] items


mergeSort : Ord a => List a -> List a
mergeSort input with (splitList input)
  mergeSort [] | SplitNil = []
  mergeSort [x] | SplitOne = [x]
  mergeSort (lefts ++ rights) | (SplitPair lefts rights) =
    merge (mergeSort lefts) (mergeSort rights)


 -- ch10 exercises

data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z xs = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) with (takeN k xs)
  takeN (S k) (x :: xs) | Fewer = Fewer
  takeN (S k) (x :: (n_xs ++ rest)) | Exact _ = Exact (x :: n_xs)
