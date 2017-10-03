module Main

import Average

showAverage : String -> String
showAverage str = "Avg length: " ++ show ( average str ) ++ "\n"

main : IO ()
main = repl "str: " showAverage
