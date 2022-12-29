{-# language OverloadedStrings #-}

import qualified Web.Scotty as S
import Data.Text.Lazy as T
import qualified Data.Map as M
import Types
import Lib4(parse, parseDoc)
--import Lib3(parseDocument)
import Lib2(renderDocument)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions
import qualified Data.List as DL (sort)

main :: IO ()
main = do
  mystateVar <- STM.newTVarIO initState
  S.scotty 3000 $ myApp mystateVar

type Toggles = M.Map Integer Check

data State = State{
  token :: Integer,
  toggles :: Toggles
}deriving Show


initState :: State
initState = State 0 $ M.singleton 0 Check {coords = []}

newState :: [Coord] -> STM.TVar State -> IO Integer
newState _ state = do
  STM.atomically $ do
    current <- STM.readTVar state
    STM.writeTVar 
      state (current {token = token current + 1, toggles = M.insert (token current) Check {coords = []} (toggles current)})
    pure $ token current

myApp :: STM.TVar State -> S.ScottyM ()
myApp state = do
  S.post "/" $ do 
    newID <- liftIO $ newState [] state
    S.text (T.pack (show newID))

  S.post "/:id/check" $ do
    user <- S.param "id"
    toggl <- liftIO $ toggles <$> STM.readTVarIO state
    case M.lookup user toggl of
      Just t -> S.text $ T.pack $ check $ toDocument t
      Nothing -> do
        S.text "Game doesn't exist"
  
  S.post "/:id/toggle" $ do
    par <- S.body
    user <- S.param "id"
    tog <- liftIO $ toggles <$> STM.readTVarIO state
    case M.lookup user tog of
      Just _ -> do 
        exists <- liftIO $ STM.atomically $ do
          curr <- STM.readTVar state
          case M.lookup user (toggles curr) of
            Just _ -> do
              case fst (Lib4.parse Lib4.parseDoc (cs par)) of 
                Right d -> 
                  case toCoord d of
                    Right coord -> 
                      STM.writeTVar
                        state
                        ( curr
                          { toggles = M.insert user coord (toggles curr)}
                        )
                    Left _ -> return ()
                Left _ -> return ()
              pure True
            Nothing -> pure False
        S.text $ T.pack $ show exists
      Nothing -> do
            S.text "Game doesn't exist"

  S.post "/:id" $ do
    par <- S.param "id"
    toggl <- liftIO $ toggles <$> STM.readTVarIO state
    case M.lookup par toggl of
      Just t -> S.text $ T.pack $ renderDocument $ board par toggl
      Nothing -> do
        S.text "Game doesn't exist"


check :: Document -> String
check tog
  | mySort tog == answer = "Puzzle is solved!"
  | otherwise = "Try again :("

mySort :: Document -> Document
mySort (DMap [("coords", DList l)]) = DMap [("coords", DList (DL.sort l))]
mySort _ = DMap []

toCoord :: Document -> Either String Check
toCoord (DMap [("coords", DList [])]) = Right Check {coords = []}
toCoord (DMap [("coords", DList a)]) = case toList' a of 
  Right l -> Right Check {coords = l}
  Left er -> Left er
toCoord _  = Left "Wrong document on toCoord"

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

getToggles :: Integer -> Toggles -> Document
getToggles i st = do
  case M.lookup i st of
    Just t -> toDocument t
    Nothing -> DNull

board :: Integer -> Toggles -> Document
board i st = 
  let t = getToggles i st in
  DMap [
  ("number_of_hints",DInteger 10),
  ("occupied_cols",DList [DInteger 2,DInteger 0,DInteger 2,DInteger 2,DInteger 2,DInteger 0,DInteger 6,DInteger 0,DInteger 3,DInteger 3]),
  ("occupied_rows",DList [DInteger 0,DInteger 2,DInteger 4,DInteger 2,DInteger 4,DInteger 1,DInteger 3,DInteger 2,DInteger 1,DInteger 1]),
  ("toggles", t)
  ]

answer :: Document
answer = DMap [("coords", DList [
  DMap [("col",DInteger 0),("row",DInteger 4)],
  DMap [("col",DInteger 0),("row",DInteger 7)],
  DMap [("col",DInteger 2),("row",DInteger 2)],
  DMap [("col",DInteger 2),("row",DInteger 6)],
  DMap [("col",DInteger 3),("row",DInteger 2)],
  DMap [("col",DInteger 3),("row",DInteger 6)],
  DMap [("col",DInteger 4),("row",DInteger 2)],
  DMap [("col",DInteger 4),("row",DInteger 4)],
  DMap [("col",DInteger 6),("row",DInteger 1)],
  DMap [("col",DInteger 6),("row",DInteger 2)],
  DMap [("col",DInteger 6),("row",DInteger 3)],
  DMap [("col",DInteger 6),("row",DInteger 4)],
  DMap [("col",DInteger 6),("row",DInteger 6)],
  DMap [("col",DInteger 6),("row",DInteger 7)],
  DMap [("col",DInteger 8),("row",DInteger 1)],
  DMap [("col",DInteger 8),("row",DInteger 8)],
  DMap [("col",DInteger 8),("row",DInteger 9)],
  DMap [("col",DInteger 9),("row",DInteger 3)],
  DMap [("col",DInteger 9),("row",DInteger 4)],
  DMap [("col",DInteger 9),("row",DInteger 5)]
  ])]
  -- toggle 6 1 6 2 6 3 6 4 2 2 3 2 4 2 9 3 9 4 9 5 8 1 0 4 0 7 4 4 2 6 3 6 6 6 6 7 8 8 8 9