import Data.Vect

insert : Ord elem =>
         (x : elem) -> (xsSorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted

my_len : List a -> Nat
my_len [] = 0
my_len (x :: xs) = 1 + my_len xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = (my_reverse xs) ++ [x]

my_map : (f: a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: map f xs

my_mapVect : (f: a -> b) -> Vect len a -> Vect len b
my_mapVect f [] = []
my_mapVect f (x :: xs) = f x :: my_mapVect f xs
