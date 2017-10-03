
import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex {n} i xs = case integerToFin i n of
                      Nothing => Nothing
                      (Just idx) => Just (index idx xs)



sumEntries : Num a => Integer -> Vect n a -> Vect m a -> Maybe a
sumEntries {n} {m} k xs ys = let x = integerToFin k n in
                             let y = integerToFin k m in
                             case (x, y) of
                               (Just x, Just y) => Just (index x xs + index y ys)
                               _ => Nothing
