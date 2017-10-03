module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store: DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newString = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newString]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" y = Just (Add y)
parseCommand "get" id = case all isDigit (unpack id) of
                          False => Nothing
                          True => Just (Get (cast id))
parseCommand "size" _ = Just Size
parseCommand "search" str = Just(Search str)
parseCommand "quit" _ = Just Quit
parseCommand _ _ = Nothing

parse : String -> Maybe Command
parse input = case span (/= ' ') input of
              (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
getEntry pos store input = let store_items = items store in
                               case integerToFin pos (size store) of
                                 Nothing => Just ("Out of range\n", store)
                                 Just id => Just (index id store_items ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str = let rest = searchString (idx + 1) xs str in
                                     if isInfixOf str x
                                       then show idx ++ " " ++ x ++ "\n" ++ rest
                                       else rest

processInput : DataStore -> String -> Maybe(String, DataStore)
processInput store input = case parse input of
                            Nothing => Just ("invalid command\n", store)
                            Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                            Just (Get pos) => getEntry pos store input
                            Just (Size) => Just("Size: " ++ show (size store) ++ "\n", store)
                            Just (Search str) => Just (searchString 0 (items store) str, store)
                            Just (Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
