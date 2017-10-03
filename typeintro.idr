data Direction = North
               | South
               | East
               | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise South = West
turnClockwise East = South
turnClockwise West = North

data Shape = ||| A triangle with base length and height
             Triangle Double Double
           | ||| A rectangle with width and height
             Rectangle Double Double
           | ||| A circle with radius
             Circle Double

area : Shape -> Double
area (Triangle x y) = x * y * 0.5
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
              (Translate 15 25 triangle))

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

Eq Biggest where
  (==) NoTriangle NoTriangle = True
  (==) NoTriangle (Size x) = False
  (==) (Size x) NoTriangle = False
  (==) (Size x) (Size y) = x == y

Ord Biggest where
  compare NoTriangle NoTriangle = EQ
  compare NoTriangle (Size x) = LT
  compare (Size x) NoTriangle = GT
  compare (Size x) (Size y) = if x > y then GT else if y > x then LT else EQ

findTriangle : (shape : Shape) -> Biggest
findTriangle (Triangle x y) = Size (area (Triangle x y))
findTriangle (Rectangle x y) = NoTriangle
findTriangle (Circle x) = NoTriangle

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive shape) = findTriangle shape
biggestTriangle (Combine pic1 pic2) = max (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
