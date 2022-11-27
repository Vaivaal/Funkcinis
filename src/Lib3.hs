{-# OPTIONSGHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use <$>" #-}
module Lib3 (hint, gameStart, parseDocument, GameStart, Hint) where

import Data.Char
import Lib1 (State (..))
import Lib2 (findData, hintsGenerator, toInt, toIntList, toMap)
import Types (Coord (..), Document (..), FromDocument, fromDocument)

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str
  | take 4 str == "---\n" = parse (parseDocNull (drop 4 str)) (parseDocInt (drop 4 str)) (parseDocStr (drop 4 str)) (parseDocList (drop 4 str)) (parseDocMap (drop 4 str))
  | last str == '\n' = parse (parseDocNull (init str)) (parseDocInt (init str)) (parseDocStr (init str)) (parseDocList (init str)) (parseDocMap (init str))
  | take 4 str == "---\n" && last str == '\n' = parse (parseDocNull (init (drop 4 str))) (parseDocInt (init (drop 4 str))) (parseDocStr (init (drop 4 str))) (parseDocList (init (drop 4 str))) (parseDocMap (init (drop 4 str)))
  | otherwise = parse (parseDocNull str) (parseDocInt str) (parseDocStr str) (parseDocList str) (parseDocMap str)

parse :: Either String Document -> Either String Document -> Either String Document -> Either String Document -> Either String Document -> Either String Document
parse nullParser intParser strParser listParser mapParser = first [nullParser, intParser, strParser, listParser, mapParser]

first :: [Either String Document] -> Either String Document
first list = 
  if length list == 1
    then head list
  else
    case head list of
      Right doc -> Right doc
      Left _ -> first (drop 1 list)


parseDocNull :: String -> Either String Document
parseDocNull "null" = Right DNull
parseDocNull "Null" = Right DNull
parseDocNull _ = Left "Error on parseDocNull"

parseDocInt :: String -> Either String Document
parseDocInt str = do
  i <- parseInt str
  return $ DInteger i

parseInt :: String -> Either String Int
parseInt "" = Left "Integer can not be empty"
parseInt str
  | not (all isDigit (drop 1 str) && (isDigit (head str) || (head str == '-'))) = Left "Integer can not contain non digits"
  | head str == '-' =
      let number = takeWhile isDigit (drop 1 str)
       in case number of
            [] -> Left "No expected integer"
            _ -> Right $ (-1) * read number
  | otherwise =
      let number = takeWhile isDigit str
       in case number of
            [] -> Left "No expected integer"
            _ -> Right $ read number

parseDocStr :: String -> Either String Document
parseDocStr str = do
  s <- parseString str
  return $ DString s

parseString :: String -> Either String String
parseString "" = Left "String can not be empty"
parseString str
  | ':' `elem` str || '{' `elem` str || '-' `elem` str || '[' `elem` str = Left "Invalid characters in a string"
  | head str == '\"' = Right (takeWhile (/= '\"') (drop 1 str))
  | head str == '\'' = Right (takeWhile (/= '\'') (drop 1 str))
  | otherwise = Right (takeWhile (/= '\n') str)

parseDocList :: String -> Either String Document
parseDocList "" = Left "Invalid list"
parseDocList "[]" = Right (DList [])
parseDocList "[]\n" = Right (DList [])
parseDocList str = do
  l <- parseList str
  return $ DList l

parseList :: String -> Either String [Document]
parseList "" = Right []
parseList str =
  case parseListItem str of
    Right (x, xs) -> case parseList xs of
      Right list -> Right (x : list)
      Left e -> Left e
    Left e -> Left e

parseListItem :: String -> Either String (Document, String)
parseListItem str =
    case parseDash str of
      Right s -> case parse (parseDocNull (takeWhile (/= '\n') s)) (parseDocInt (takeWhile (/= '\n') s)) (parseDocStr (takeWhile (/= '\n') s)) (parseDocList (fst (parseTabs True s ""))) (parseDocMap (fst (mapFromList True s ""))) of
        Right DNull -> Right (DNull, drop 1 (dropWhile (/= '\n') str))
        Right (DInteger num) -> Right (DInteger num, drop 1 (dropWhile (/= '\n') str))
        Right (DString st) -> Right (DString st, drop 1 (dropWhile (/= '\n') str))
        Right (DList list) -> Right (DList list, snd (parseTabs True s ""))
        Right (DMap mp) -> Right (DMap mp, snd (mapFromList True s ""))
        Left e -> Left e
      Left e -> Left e

mapFromList :: Bool -> String -> String -> (String, String)
mapFromList _ "" acc = (acc, "")
mapFromList isFirst r acc
  | isFirst && head r /= '-' = mapFromList False (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | not isFirst && head r /= '-' = mapFromList False (drop 1 (dropWhile (/= '\n') r)) (acc ++ drop 2 (takeWhile (/= '\n') r) ++ "\n")
  | otherwise = (acc, r)

parseTabs :: Bool -> String -> String -> (String, String)
parseTabs _ "" acc = (acc, "")
parseTabs isFirst r acc
  | isFirst || take 2 r == "{}" = parseTabs False (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | take 2 r == "  " && not isFirst = parseTabs False (drop 1 (dropWhile (/= '\n') r)) (acc ++ drop 2 (takeWhile (/= '\n') r) ++ "\n")
  | otherwise = (acc, r)

parseDash :: String -> Either String String
parseDash "" = Left "No dash before list item"
parseDash str
  | take 2 str == "- " = Right (drop 2 str)
  | otherwise = Left "No dash before list item"

parseDocMap :: String -> Either String Document
parseDocMap "{}" = Right (DMap[])
parseDocMap "{}\n" = Right (DMap[])
parseDocMap str = do
  m <- parseMap str
  return $ DMap m

parseMap :: String -> Either String [(String, Document)]
parseMap "" = Right []
parseMap str =
  case parseMapItem str of
    Right (x, xs) -> case parseMap xs of
      Right mp -> Right (x : mp)
      Left e -> Left e
    Left e -> Left e

parseMapItem :: String -> Either String ((String, Document), String)
parseMapItem str =
  case parseMapName str of
    Right (name, r) -> case parse (parseDocNull (takeWhile (/= '\n') r)) (parseDocInt (takeWhile (/= '\n') r)) (parseDocStr (takeWhile (/= '\n') r)) (parseDocList (fst (listFromMap r ""))) (parseDocMap (fst (parseTabs False r ""))) of
      Right DNull -> Right ((name, DNull), drop 1 (dropWhile (/= '\n') str))
      Right (DInteger num) -> Right ((name, DInteger num), drop 1 (dropWhile (/= '\n') str))
      Right (DString st) -> Right ((name, DString st), drop 1 (dropWhile (/= '\n') str))
      Right (DList list) -> Right ((name, DList list), snd (listFromMap r ""))
      Right (DMap mp) -> Right ((name, DMap mp), snd (parseTabs False r ""))
      Left e -> Left e
    Left e -> Left e

listFromMap :: String -> String -> (String, String)
listFromMap "" acc = (acc, "")
listFromMap r acc
  | head r == '-' || take 2 r == "[]" || take 2 r == "  " = listFromMap (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | otherwise = (acc, r)

parseMapName :: String -> Either String (String, String)
parseMapName "" = Left "Empty map name"
parseMapName str
  | ':' `notElem` str = Left "No ':' after map name"
  | '\'' `elem` takeWhile (/= ':') str || '\"' `elem` takeWhile (/= ':') str = Right (init (drop 1 (takeWhile (/= ':') str)), drop 2 (dropWhile (/= ':') str))
  | otherwise = Right (takeWhile (/= ':') str, drop 2 (dropWhile (/= ':') str))

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart
  { occupiedCols :: [Int],
    occupiedRows :: [Int],
    noOfHints :: Int
  }
  deriving (Show)

instance FromDocument GameStart where
  fromDocument doc = do
    c <- toIntList (findData "occupied_cols" $ toMap doc)
    r <- toIntList (findData "occupied_rows" $ toMap doc)
    h <- toInt (findData "number_of_hints" $ toMap doc)
    return (GameStart c r h)

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State a b _ _ _) (GameStart c d e) = State a b c d e

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint
  { hintedCells :: [Coord]
  }
  deriving (Show)

instance FromDocument Hint where
  fromDocument doc =
    case toMap doc of
      Right newMap -> case hintsGenerator (toMap (snd (head newMap))) of
        Right hints -> Right $ Hint hints
        Left e -> Left e
      Left err -> Left err

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
hint (State _ a b c d) (Hint h) = State h a b c d