import Data.Vect

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' {n} {m} acc [] = rewrite sym plusZeroRightNeutral n in acc
        reverse' acc (x :: xs) = ?reverseProof_xs (reverse' (x::acc) xs)

myReverse2Proof : (x : elem) -> (xs : Vect len elem) ->
                  Vect (len + 1) elem -> Vect (S len) elem
myReverse2Proof {len} x xs result = rewrite plusCommutative 1 len in result

myReverse2 : Vect n elem -> Vect n elem
myReverse2 [] = []
myReverse2 (x :: xs) = myReverse2Proof x xs (myReverse2 xs ++ [x])




-- http://blorg.ericb.me/2016/09/proving-addition-is-commutative-in-idris/
myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = let inductiveHypothesis = myPlusCommutes k m in
                          rewrite inductiveHypothesis in
                          rewrite plusSuccRightSucc m k in
                          Refl
