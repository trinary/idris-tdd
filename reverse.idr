module Main

import Average

showAverage : String -> String
showAverage str = "avg length: " ++ show (average str) ++ "\n"

main : IO ()
main = repl " > " showAverage


