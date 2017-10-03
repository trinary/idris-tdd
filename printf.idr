import Data.Vect

data Format = Number Format
            | Character Format
            | Dbl Format
            | Str Format
            | Lit String Format
            | End

PrintFType : Format -> Type
PrintFType (Number fmt) = (i : Int) -> PrintFType fmt
PrintFType (Character fmt) = (c : Char) -> PrintFType fmt
PrintFType (Dbl fmt) = (f : Double) -> PrintFType fmt
PrintFType (Str fmt) = (s : String) -> PrintFType fmt
PrintFType (Lit str fmt) = PrintFType fmt
PrintFType End = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintFType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Character fmt) acc = \c => printfFmt fmt (acc ++ (strCons c ""))
printfFmt (Dbl fmt) acc = \f => printfFmt fmt (acc ++ show f)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Character (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (fmt : String) -> PrintFType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""


Mat : Nat -> Nat -> Type
Mat k j = Vect k (Vect j Nat)

thisMat : Mat 2 3
thisMat = [[1,2,3], [2,3,4]]

TupleVect : Nat -> (A : Type) -> Type
TupleVect Z A = ()
TupleVect (S k) A = (A, (TupleVect k A))

tupleVectTest : TupleVect 4 Nat
tupleVectTest = (1,2,3,4,())
