
import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon k = Vect k Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

line : Polygon 2
line =[(?line_rhs1, ?line_rhs2), (?line_rhs3, ?line_rhs4)]

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety Four"
getStringOrInt True = 94

valToString : (isInt : Bool) -> (case isInt of
                                              False => String
                                              True => Int) -> String
valToString False y = trim y
valToString True y = cast y

AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
