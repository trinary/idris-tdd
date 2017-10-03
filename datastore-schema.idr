module Main

import Data.Vect

infixr 5 .+.
data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newItem
           = MkData schema _ (addToData store)
  where
    addToData : Vect oldSize (SchemaType schema) -> Vect (S oldSize) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Size : Command schema
     Search : String -> Command schema
     Quit : Command schema

parseCommand : (schema : Schema) -> String -> String -> Maybe Command schema
parseCommand "add" y = Just (Add (?parse y))
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

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                           Nothing => Just ("Out of range\n", store)
                           Just id => Just (?display (index id (items store)) ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str = let rest = searchString (idx + 1) xs str in
                                     if isInfixOf str x
                                       then show idx ++ " " ++ x ++ "\n" ++ rest
                                       else rest
processInput : DataStore -> String -> Maybe(String, DataStore)
processInput store input = case parse input of
                            Nothing => Just ("invalid command\n", store)
                            Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store (?convert item))
                            Just (Get pos) => getEntry pos store input
                            Just (Size) => Just("Size: " ++ show (size store) ++ "\n", store)
                            Just (Search str) => Just (searchString 0 (items store) str, store)
                            Just (Quit) => Nothing
--
-- main : IO ()
-- main = replWith (MkData _ []) "Command: " processInput
