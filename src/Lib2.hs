{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Lib2(renderDocument, hint, gameStart) where

import Types ( ToDocument(..), Document(..), Check(..), Coord(..) )
import Lib1 (State(..))
import Data.List (delete)

instance Eq State where
    (==) (State a1 b1 c1 d1 e1) (State a2 b2 c2 d2 e2) = a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 && e1 == e2
-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
    toDocument Check {coords = []} = DMap [("coords", DList [])] 
    toDocument Check {coords = a} = DMap [("coords", DList (toList' a))]
        where
            toList' :: [Coord] -> [Document]
            toList' [] = []
            toList' (x:xs) = coordToMap x : toList' xs
                where
                    coordToMap :: Coord -> Document
                    coordToMap Coord {col = c, row = r} = DMap [("col", DInteger c),("row", DInteger r)]

-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument DNull = "null"
renderDocument (DInteger num) = show num
renderDocument (DString str) = str
renderDocument (DList list) = renderList list 0 False
renderDocument (DMap lot) = renderMap lot 0 False

renderList :: [Document] -> Int -> Bool -> String
renderList [] _ _ = ""
renderList (DNull:xs) a b = renderTab a b ++ "- " ++ "null" ++ "\n" ++ renderList xs a True
renderList ((DInteger num):xs) a b = renderTab a b ++ "- " ++ show num ++ "\n" ++ renderList xs a True
renderList ((DString str):xs) a b = renderTab a b ++ "- " ++ str ++ "\n" ++ renderList xs a True
renderList ((DList []):xs) a b = renderTab a b ++ "- \n" ++ renderList xs a True
renderList ((DList list):xs) a b = renderTab a b ++ "- " ++ renderList list (a + 1) False ++ renderList xs a True
renderList ((DMap lot):xs) a b = renderTab a b ++ "- " ++ renderMap lot (a + 1) b ++ renderList xs a b

renderMap :: [(String, Document)] -> Int -> Bool -> String
renderMap [] _ _ = ""
renderMap ((s, DNull):xs) a b = renderTab a b ++ s ++ ": " ++ "null" ++ "\n" ++ renderMap xs a True
renderMap ((s, DInteger num):xs) a b = renderTab a b ++ s ++ ": " ++ show num ++ "\n" ++ renderMap xs a True
renderMap ((s, DString str):xs) a b = renderTab a b ++ s ++ ": " ++ str ++ "\n" ++ renderMap xs a True
renderMap ((s, DList list):xs) a b = renderTab a b ++ s ++ ":\n" ++ renderList list a b ++ renderMap xs a True
renderMap ((s, DMap lot):xs) a b = renderTab a b ++ s ++ ":\n" ++ renderMap lot (a + 1) True ++ renderMap xs a True

renderTab :: Int -> Bool -> String
renderTab a b
    | not b = ""
    | a == 0 = ""
    | otherwise = "  " ++ renderTab (a - 1) b

-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart _ doc = do
    c <- toIntList(findData "occupied_cols" $ toMap doc)
    r <- toIntList(findData "occupied_rows" $ toMap doc)
    h <- toInt(findData "number_of_hints" $ toMap doc)
    return (State [] [] c r h)
--gameStart _ doc = Left (show doc)

toMap :: Document -> Either String [(String, Document)]
toMap (DMap x) = Right x
toMap DNull = Right [("", DNull)]
toMap e = Left ("invalid " ++ show e ++ " argument, expected DMap")

toInt :: Either String Document -> Either String Int
toInt (Right (DInteger x)) = Right x
--toInt (Right DNull) = Left "invalid argument, expected DInteger"
toInt (Right e) = Left ("invalid " ++ show e ++ " argument, expected DInteger")
toInt (Left e) = Left e

toIntList :: Either String Document -> Either String [Int] 
toIntList (Right (DList [])) = Right []
toIntList (Right (DList (DInteger x:xs))) = 
    case  toIntList (Right (DList xs)) of
        Right num -> Right (x : num)
        Left e -> Left e
toIntList (Right e) = Left ("invalid " ++ show e ++ " argument, expected DList [DInteger...]")
toIntList (Left e) = Left e
    

findData :: String -> Either String [(String, Document)] -> Either String Document
findData category (Right []) = Left ("could not find " ++ show category)
findData category (Right gameMap)
    | category == fst (head gameMap) = Right (snd (head gameMap))
    | otherwise = findData category (Right (tail gameMap))
findData _ (Left e) = Left e

-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 

hint :: State -> Document -> Either String State
hint (State _ toggled cols rows hintsNo) doc = 
    case toMap doc of
        Right newMap -> case hintsGenerator (toMap(snd (head newMap))) of
            Right hints -> Right (noDuplicates (State hints toggled cols rows hintsNo))
            Left e -> Left e
        Left err -> Left err

hintsGenerator :: Either String [(String, Document)] -> Either String [Coord]
hintsGenerator (Right [("", DNull)]) = Right []
hintsGenerator (Right x) = 
    case hintGenerator (toMap(snd (head x))) of
        Right listHead -> case hintsGenerator (toMap (snd (last x))) of
            Right listTail -> if snd (head x) == DNull then Right [] else Right (listHead : listTail)
            Left e -> Left e
        Left e -> Left e
hintsGenerator (Left e) = Left e

hintGenerator :: Either String [(String, Document)] -> Either String Coord
hintGenerator (Right x) = 
    case toInt (Right (snd (head x))) of
        Right c -> 
            case toInt (Right (snd (last x))) of 
                Right r -> Right (Coord {col = c, row = r})
                Left e -> Left e
        Left err -> Left err  
hintGenerator (Left e) = Left e

noDuplicates :: State -> State
noDuplicates (State hinted toggled cols rows hintsNo) = State hinted (elimination hinted toggled) cols rows hintsNo

elimination :: [Coord] -> [Coord] -> [Coord]
elimination [] toggled = toggled
elimination (x:xs) toggled = elimination xs (delete x toggled)
