
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x) = "[" ++ show x ++ "]"

evaluate : (Neg num, Integral num, Eq num) => Expr num -> num
evaluate (Val y) = y
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y
evaluate (Div x y) = evaluate x `div` evaluate y
evaluate (Abs x) = abs (evaluate x)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = Just (max x y)


(Eq ty, Integral ty, Neg ty) => Eq (Expr ty) where
  (==) x y = (evaluate x) == (evaluate y)

(Eq from, Integral from, Neg from, Eq to, Integral to, Neg to) => Cast (Expr from) to where
  cast orig = ?bleah_notsure -- cast (evaluate orig)

Functor Expr where
  map func (Val x)   = (Val (func x))
  map func (Add x y) = (Add (map func x) (map func y))
  map func (Sub x y) = (Sub (map func x) (map func y))
  map func (Mul x y) = (Mul (map func x) (map func y))
  map func (Div x y) = (Div (map func x) (map func y))
  map func (Abs x)   = (Abs (map func x))
