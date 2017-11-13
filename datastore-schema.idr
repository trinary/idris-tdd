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
     SetSchema : (newSchema : Schema) -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Size : Command schema
     Search : String -> Command schema
     Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing

parsePrefix SInt input = case span isDigit input of
                          ("", rest) => Nothing
                          (num, rest) => Just(cast num, ltrim rest)
parsePrefix (left .+. right) input = case parsePrefix left input of
                                          Nothing => Nothing
                                          Just (lVal, input') => case parsePrefix right input' of
                                                                      Nothing => Nothing
                                                                      Just (rVal, input'') =>
                                                                        Just((lVal, rVal), input'')

parseSchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseSchema schema input = case parsePrefix schema input of
                                Just (res, "") => Just res
                                Just _ => Nothing
                                Nothing => Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" y = case parseSchema schema y of
                              Nothing => Nothing
                              Just restok => Just (Add restok)
parseCommand schema "get" id = case all isDigit (unpack id) of
                          False => Nothing
                          True => Just (Get (cast id))
parseCommand schema "size" _ = Just Size
parseCommand schema "search" str = Just(Search str)
parseCommand schema "quit" _ = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
              (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (a, b) = display a ++ ", " ++ display b

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                           Nothing => Just ("Out of range\n", store)
                           Just id => Just (display (index id (items store)) ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str : String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str = let rest = searchString (idx + 1) xs str in
                                     if isInfixOf str x
                                       then show idx ++ " " ++ x ++ "\n" ++ rest
                                       else rest
processInput : DataStore -> String -> Maybe(String, DataStore)
processInput store input = case parse (schema store) input of
                            Nothing => Just ("invalid command\n", store)
                            Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                            Just (Get pos) => getEntry pos store
                            Just (Size) => Just("Size: " ++ show (size store) ++ "\n", store)
                            -- Just (Search str) => Just (searchString 0 (items store) str, store)
                            Just (Quit) => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
