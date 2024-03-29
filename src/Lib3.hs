{-# OPTIONSGHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use <$>" #-}
module Lib3 (hint, gameStart, parseDocument, GameStart, Hint) where

import Data.Char
import Lib1 (State (..))
import Lib2 (findData, hintsGenerator, toInt, toIntList, toMap)
import Types (Coord (..), Document (..), FromDocument, fromDocument, Check(..))

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
    else case head list of
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
  case parseDash str of
    Right s -> case parseItem s parseTabs mapFromList True of
      Right (x, xs) -> case parseList xs of
        Right list -> Right (x : list)
        Left e -> Left e
      Left e -> Left e
    Left e -> Left e

parseItem :: String -> (Bool -> String -> String -> (String, String)) -> (Bool -> String -> String -> (String, String)) -> Bool -> Either String (Document, String)
parseItem str f1 f2 b =
  case parse (parseDocNull (takeWhile (/= '\n') str)) (parseDocInt (takeWhile (/= '\n') str)) (parseDocStr (takeWhile (/= '\n') str)) (parseDocList (fst (f1 True str ""))) (parseDocMap (fst (f2 b str ""))) of
    Right DNull -> Right (DNull, drop 1 (dropWhile (/= '\n') str))
    Right (DInteger num) -> Right (DInteger num, drop 1 (dropWhile (/= '\n') str))
    Right (DString st) -> Right (DString st, drop 1 (dropWhile (/= '\n') str))
    Right (DList list) -> Right (DList list, snd (f1 True str ""))
    Right (DMap mp) -> Right (DMap mp, snd (f2 True str ""))
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
  | not isFirst && take 2 r == "  " = parseTabs False (drop 1 (dropWhile (/= '\n') r)) (acc ++ drop 2 (takeWhile (/= '\n') r) ++ "\n")
  | otherwise = (acc, r)

parseDash :: String -> Either String String
parseDash "" = Left "No dash before list item"
parseDash str
  | take 2 str == "- " = Right (drop 2 str)
  | otherwise = Left "No dash before list item"

parseDocMap :: String -> Either String Document
parseDocMap "{}" = Right (DMap [])
parseDocMap "{}\n" = Right (DMap [])
parseDocMap str = do
  m <- parseMap str
  return $ DMap m

parseMap :: String -> Either String [(String, Document)]
parseMap "" = Right []
parseMap str =
  case parseMapName str of
    Right (name, s) -> case parseItem s listFromMap parseTabs False of
      Right (x, xs) -> case parseMap xs of
        Right mp -> Right ((name, x) : mp)
        Left e -> Left e
      Left e -> Left e
    Left e -> Left e

listFromMap :: Bool -> String -> String -> (String, String)
listFromMap _ "" acc = (acc, "")
listFromMap _ r acc
  | head r == '-' || take 2 r == "[]" || take 2 r == "  " = listFromMap True (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
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
    noOfHints :: Int,
    toggles :: [Coord]
  }
  deriving (Show)

instance FromDocument GameStart where
  fromDocument doc = do
    c <- toIntList (findData "occupied_cols" $ toMap doc)
    r <- toIntList (findData "occupied_rows" $ toMap doc)
    h <- toInt (findData "number_of_hints" $ toMap doc)
    t <- findData "toggles" $ toMap doc
    tog <- toCoord t
    return (GameStart c r h tog)

toCoord :: Document -> Either String [Coord]
toCoord (DMap [("coords", DList [])]) = Right []
toCoord (DMap [("coords", DList a)]) = do toList' a
toCoord _  = Left "Wrong document on toCoord"
--toCoord a = Left $ show a

toList' :: [Document] -> Either String [Coord]
toList' [] = Right []
toList' (x:xs) = case mapToCoord x of 
  Right m -> do
    l <- toList' xs
    Right $ m : l
  Left err -> Left err
    
mapToCoord :: Document -> Either String Coord 
mapToCoord (DMap [("col", DInteger c),("row", DInteger r)]) = Right $ Coord {col = c, row = r}
mapToCoord _ = Left "err"
-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
gameStart (State a b _ _ _) (GameStart c d e f) = State a f c d e

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