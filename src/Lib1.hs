{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types
import Data.List (delete)

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State = State{
    hintedCells :: [Coord],
    toggledCells :: [Coord],
    occupiedCols :: [Int],
    occupiedRows :: [Int],
    noOfHints :: Int
}deriving Show

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State [] [] [] [] 0

toMap :: Document -> [(String, Document)]
toMap (DMap x) = x
toMap DNull = [("", DNull)]
toMap _ = error "Argument not provided"

toIntList :: Document -> [Int]
toIntList (DList x) = map toInt x
toIntList _ = error "Argument not provided"

toInt :: Document -> Int
toInt (DInteger x) = x
toInt _ = error "Argument not provided"

findData :: String -> [(String, Document)] -> Document
findData category gameMap
    | category == fst (head gameMap) = snd (head gameMap)
    | otherwise = findData category (tail gameMap)

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart _ doc = State [] []
    (toIntList(findData "occupied_cols" $ toMap doc))
    (toIntList(findData "occupied_rows" $ toMap doc))
    (toInt(findData "number_of_hints" $ toMap doc))

-- IMPLEMENT
-- renders your game board
render :: State -> String
render (State hinted toggled cols rows hintsNo) = "   " ++ printCols cols ++ "\n   --------------------\n" ++ printRows 0 (State hinted toggled cols rows hintsNo) rows

printCols :: [Int] -> String
printCols [] = ""
printCols cols = " " ++ show (head cols) ++ printCols (tail cols)

printRows :: Int -> State -> [Int] -> String
printRows _ _ [] = ""
printRows noOfRow (State hinted toggled cols rows hintsNo) rowsPrnt = show (head rowsPrnt) ++ " |" ++ checkIfMarked noOfRow 0 (State hinted toggled cols rows hintsNo) "" ++ "\n" ++ printRows (noOfRow + 1) (State hinted toggled cols rows hintsNo) (tail rowsPrnt)

checkIfMarked :: Int -> Int -> State -> String -> String
checkIfMarked noOfRow noOfCol (State hinted toggled cols rows hintsNo) marks
    | noOfCol == 10 = marks
    | Coord noOfCol noOfRow `elem` (hinted ++ toggled) = checkIfMarked noOfRow (noOfCol + 1) (State hinted toggled cols rows hintsNo) (marks ++ " #")
    | otherwise = checkIfMarked noOfRow (noOfCol + 1) (State hinted toggled cols rows hintsNo) (marks ++ " -")

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State hinted toggled _ _ _) = Check (hinted ++ toggled)
-- Answer: Coord 6 1, Coord 6 2, Coord 6 3, Coord 6 4, Coord 2 2, Coord 3 2, Coord 4 2, Coord 9 3, Coord 9 4, Coord 9 5,Coord 8 1,Coord 0 4, Coord 0 7,Coord 4 4,Coord 2 6, Coord 3 6,Coord 6 6, Coord 6 7,Coord 8 8, Coord 8 9

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State hinted toggled cols rows hintsNo) [] = State hinted toggled cols rows hintsNo
toggle (State hinted toggled cols rows hintsNo) [_] = State hinted toggled cols rows hintsNo
toggle (State hinted toggled cols rows hintsNo) input =
    toggle (toggleTry (State hinted toggled cols rows hintsNo) (Coord {col = read (head input), row = read (head (tail input))})) (drop 2 input)

toggleTry :: State -> Coord -> State
toggleTry (State hinted toggled cols rows hintsNo) coord
    | coord `notElem` (hinted ++ toggled) = State hinted (toggled ++ [coord]) cols rows hintsNo
    | otherwise = untoggle (State hinted toggled cols rows hintsNo) coord

untoggle :: State -> Coord -> State
untoggle (State hinted toggled cols rows hintsNo) coord = State hinted (delete coord toggled) cols rows hintsNo

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint (State _ toggled cols rows hintsNo) doc = noDuplicates (State (hintsGenerator (toMap(snd (head (toMap doc))))) toggled cols rows hintsNo)

hintsGenerator :: [(String, Document)] -> [Coord]
hintsGenerator x | snd (head x) == DNull = []
                | otherwise = hintGenerator (toMap(snd (head x))) : hintsGenerator (toMap (snd (last x)))

hintGenerator :: [(String, Document)] -> Coord
hintGenerator x = Coord {col = toInt (snd (head x)), row = toInt (snd (last x))}

noDuplicates :: State -> State
noDuplicates (State hinted toggled cols rows hintsNo) = State hinted (elimination hinted toggled) cols rows hintsNo

elimination :: [Coord] -> [Coord] -> [Coord]
elimination [] toggled = toggled
elimination (x:xs) toggled = elimination xs (delete x toggled)