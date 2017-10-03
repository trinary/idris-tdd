
import System


printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                      putStrLn (show len)

printTwoThings : IO ()
printTwoThings = do
  putStrLn "ayyyyy"
  putStrLn "lmao"

printLonger : IO ()
printLonger = do
  putStr "First: "
  first <- getLine
  putStr "Second: "
  second <- getLine
  let longer = if (length first) > (length second) then length first else length second
  putStrLn (show longer)

printLongerBind : IO ()
printLongerBind =
  putStr "First: " >>= \_ =>
    getLine >>= \first =>
      putStr "Second: " >>= \_ =>
        getLine >>= \second =>
          let longer = if (length first) > (length second) then (length first) else (length second) in
            putStrLn (show longer)

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumPair : IO (Maybe (Nat, Nat))
readNumPair = do
  Just num1 <- readNumber | Nothing => pure Nothing
  Just num2 <- readNumber | Nothing => pure Nothing
  pure (Just (num1, num2))

guess : (target : Nat) -> IO ()
guess target = do
  putStr "Take a guess: "
  Just input <- readNumber
  | Nothing => do
    putStrLn "Enter a number please."
    guess target

  if (input == target) then pure ()
    else
      if (input > target) then do
        putStrLn "Too high!"
        guess target
      else do
        putStrLn "Too low!"
        guess target

main : IO ()
main = do
  random <- time
  randomTen = modNat (cast random) 10
  guess randomTen
