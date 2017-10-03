import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do
  x <- getLine
  xs <- readVectLen k
  pure (x :: xs)

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if (x == "")
                then pure (MkVect _ [])
                else do MkVect _ xs <- readVect
                        pure (MkVect _ (x :: xs))

readVect2 : IO (len ** Vect len String)
readVect2 = do x <- getLine
               if (x == "")
                 then pure (_ ** [])
                 else do (_ ** xs) <- readVect2
                         pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "type stuff, then a blank line."
               (len1 ** vec1) <- readVect2
               putStrLn "type more stuff, then a blank line."
               (len2 ** vec2) <- readVect2
               case exactLength len1 vec2 of
                 Nothing => putStrLn "type the same number of things both times please"
                 Just vec2' => printLn (zip vec1 vec2')

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                   then pure []
                   else do xs <- readToBlank
                           pure (x :: xs)
