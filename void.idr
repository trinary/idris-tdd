--import Data.Vect

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

twoPlusTwoIsNotFive : 2 + 2 = 5 -> Void
twoPlusTwoIsNotFive Refl impossible


zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z      Z    = Yes Refl
checkEqNat Z     (S k) = No zeroNotSuc
checkEqNat (S k) Z     = No sucNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              (Yes prf) => Yes (cong prf)
                              (No contra) => No (noRec contra)

headUnequal : DecEq a => { xs: Vect n a } -> { ys: Vect n a } ->
                         (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => { xs: Vect n a } -> { ys: Vect n a } ->
                         (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq xs ys = case headUnequal (xs = ys) of
                              val => ?foo
