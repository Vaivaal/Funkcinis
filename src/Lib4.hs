{-# OPTIONSGHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib4 (parse, parseDoc, Parser) where

import Types (Document (..))
import Data.Char
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Applicative((<|>), some)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

parseDoc :: Parser Document   
parseDoc = do
    _ <- initialEdit
    parseDocNull <|> parseDocInt <|> parseDocString <|> parseDocList <|> parseDocMap

initialEdit :: Parser String
initialEdit = do
    s <- lift get
    case s of
        str | take 4 str == "---\n" && last str == '\n' -> lift (put (init (drop 4 str))) >> return str
            | take 4 str == "---\n" -> lift (put (drop 4 str)) >> return str
            | last str == '\n' -> lift (put (init str)) >> return str
            | otherwise -> lift (put str) >> return str

parseDocNull :: Parser Document
parseDocNull = do
    str <- lift get
    case str of
        "Null" -> return DNull
        "null" -> return DNull
        _ -> throwE "Document is not a DNull\n"
    

parseDocInt :: Parser Document
parseDocInt = do
    str <- lift get
    case str of
        "" -> throwE "Document is not a DInteger\n"
        _ | not (all isDigit (drop 1 str) && (isDigit (head str) || (head str == '-'))) -> throwE "Integer can not contain non digits\n"
          | head str == '-' -> do
              let number = takeWhile isDigit (drop 1 str)
               in case number of
                    [] -> throwE "No expected integer\n"
                    _ -> return $ DInteger $ (-1) * read number
          | otherwise -> do
              let number = takeWhile isDigit str
               in case number of
                    [] -> throwE "No expected integer\n"
                    _ -> return $ DInteger $ read number

parseDocString :: Parser Document
parseDocString = do
    str <- lift get
    case str of
        "" -> throwE "Document is not a DString\n"
        _ | ':' `elem` str || '{' `elem` str || '-' `elem` str || '[' `elem` str -> throwE "Invalid characters in a string\n"
          | head str == '\"' -> return $ DString $ takeWhile (/= '\"') (drop 1 str)
          | head str == '\'' -> return $ DString $ takeWhile (/= '\'') (drop 1 str)
          | otherwise ->  return $ DString $ takeWhile (/= '\n') str

parseDocList :: Parser Document
parseDocList = do
    str <- lift get
    case str of
        "" -> throwE "Document is not a DList\n"
        "[]" -> return (DList [])
        "[]\n" -> return (DList [])
        _ -> do DList <$> parseList

parseList :: Parser [Document]
parseList = do
    some $ parseDash >> parseItem parseTabs mapFromList True

parseDocMap :: Parser Document
parseDocMap = do
    str <- lift get
    case str of
        "" -> throwE "Document is not a DMap\n"
        "{}" -> return (DMap [])
        "{}\n" -> return (DMap [])
        _ -> do DMap <$> parseMap

parseMap :: Parser [(String, Document)]
parseMap = do
    some $ toTouple parseMapName (parseItem listFromMap parseTabs False)

toTouple :: Parser String -> Parser Document -> Parser (String, Document)
toTouple pstr parser = do
    str <- pstr
    doc <- parser
    return (str, doc)
            
parseItem :: (Bool -> String -> String -> (String, String)) -> (Bool -> String -> String -> (String, String)) -> Bool -> Parser Document
parseItem f1 f2 b = do
    str <- lift get
    item <- (lift (put (takeWhile (/= '\n') str)) >> parseDocNull) <|> (lift (put (takeWhile (/= '\n') str)) >> parseDocInt) <|> (lift (put (takeWhile (/= '\n') str)) >> parseDocString) <|> (lift (put (fst (f1 True str ""))) >> parseDocList) <|> (lift (put (fst (f2 b str ""))) >> parseDocMap)
    case item of
        DNull -> lift (put (drop 1 (dropWhile (/= '\n') str))) >> return item
        DInteger _ -> lift (put (drop 1 (dropWhile (/= '\n') str))) >> return item
        DString _ -> lift (put (drop 1 (dropWhile (/= '\n') str))) >> return item
        DList _ -> lift (put (snd (f1 True str ""))) >> return item
        DMap _ -> lift (put (snd (f2 True str ""))) >> return item

parseTabs :: Bool -> String -> String -> (String, String)
parseTabs _ "" acc = (acc, "")
parseTabs isFirst r acc
  | isFirst || take 2 r == "{}" = parseTabs False (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | not isFirst && take 2 r == "  " = parseTabs False (drop 1 (dropWhile (/= '\n') r)) (acc ++ drop 2 (takeWhile (/= '\n') r) ++ "\n")
  | otherwise = (acc, r)

mapFromList :: Bool -> String -> String -> (String, String)
mapFromList _ "" acc = (acc, "")
mapFromList isFirst r acc
  | isFirst && head r /= '-' = mapFromList False (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | not isFirst && head r /= '-' = mapFromList False (drop 1 (dropWhile (/= '\n') r)) (acc ++ drop 2 (takeWhile (/= '\n') r) ++ "\n")
  | otherwise = (acc, r)

parseDash :: Parser Document
parseDash = do
    str <- lift get
    case str of
        [] -> throwE "No dash before a list item\n"
        _ | take 2 str == "- " -> do
            lift $ put $ drop 2 str
            return DNull
          | otherwise -> throwE "No dash before a list item\n"

listFromMap :: Bool -> String -> String -> (String, String)
listFromMap _ "" acc = (acc, "")
listFromMap _ r acc
  | head r == '-' || take 2 r == "[]" || take 2 r == "  " = listFromMap True (drop 1 (dropWhile (/= '\n') r)) (acc ++ takeWhile (/= '\n') r ++ "\n")
  | otherwise = (acc, r)

parseMapName :: Parser String
parseMapName = do
    str <- lift get
    case str of
        [] -> throwE "Empty map name"
        _ |':' `notElem` str -> throwE "No ':' after map name"
          | '\'' `elem` takeWhile (/= ':') str || '\"' `elem` takeWhile (/= ':') str -> do
            lift $ put $ (drop 2 (dropWhile (/= ':') str))
            return (init (drop 1 (takeWhile (/= ':') str)))
          | otherwise -> do
            lift $ put $ (drop 2 (dropWhile (/= ':') str))
            return (takeWhile (/= ':') str)