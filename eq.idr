
occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                False => occurrences item xs
                                True => 1 + occurrences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _  = False

  (/=) a b = not (a == b)

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') = (left == left') && (e == e') && (right == right')
  (==) _ _ = False

record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

help : Album
help = MkAlbum "The Beatles" "Help" 1965

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, heroes]

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
       artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
    case compare artist artist' of
      EQ => case compare year year' of
              EQ => compare title title'
              diff_year => diff_year
      diff_artist => diff_artist
Show Album where
  show (MkAlbum artist title year) = title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"

-- exercises

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle x z) (Triangle x' z' ) = x == x' && z == z'
  (==) (Rectangle x z) (Rectangle x' z') = x == x' && z == z'
  (==) (Circle x) (Circle x') = x == x'
  (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y) where
    area : Shape -> Double
    area (Triangle x z) = x * z / 2
    area (Rectangle x z) = x * z
    area (Circle x) = Doubles.pi * x * x


testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
