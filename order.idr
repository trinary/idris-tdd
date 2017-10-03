module Main

Shape : Type
rotate : Shape -> Shape

twice : (a -> a) -> a -> a
twice f x = f (f x)

addTwo : (Num a) => a -> a
addTwo x = x + 2

longer : String -> String -> Nat
longer a b = let
             lenA = length a
             lenB = length b in
             if lenA > lenB then lenA else lenB

pythagoras : Double -> Double -> Double
pythagoras  x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x
