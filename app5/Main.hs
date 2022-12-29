{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT )
import Data.ByteString as B ( empty, ByteString )
import Data.Either as E (fromRight)
import qualified Data.List as L
import Data.Text as T ( concat, drop, pack, unpack, Text )
import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
import Lib1 ( emptyState, mkCheck, render, toggle, State )
import Lib2 ( renderDocument )
import Lib3 ( gameStart, GameStart)
import Lib4 (parse, parseDoc)
import Types(Check, toDocument, fromDocument)
import Network.Wreq
    ( post, postWith, defaults, header, responseBody )
import qualified Network.Wreq as Wreq

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions

type Repl a = HaskelineT (StateT (String, Lib1.State) IO) a

commandShow :: String
commandShow = "show"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

--Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = lift get >>= liftIO . Prelude.putStrLn . Lib1.render . snd
  | trim c == commandCheck = lift get >>= check . (Lib1.mkCheck . snd) >>= liftIO . Prelude.putStrLn
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at leas one argument"
      t -> do
        (url, _) <- lift get
        lift $ modify (\(u, s) -> (u, Lib1.toggle s (L.drop 1 t)))
        (_, newState) <- lift get
        let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
        let body = cs $ renderDocument $ toDocument (Lib1.mkCheck newState) :: B.ByteString
        resp <- liftIO $ postWith opts (url ++ "/toggle") body
        return ()
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

check :: Check -> Repl String
check c = do
  (url, _) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ toDocument c :: B.ByteString
  resp <- liftIO $ postWith opts (url ++ "/check") body
  pure $ cs $ resp ^. responseBody

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandCheck, commandToggle]
  return $ Prelude.filter (L.isPrefixOf n) names


ini :: Repl ()
ini = do
  (url, s) <- lift get
  r <- liftIO $ post url B.empty
  let gs = fst (Lib4.parse Lib4.parseDoc (cs (r ^. responseBody))) >>= fromDocument
  case (gs :: Either String Lib3.GameStart) of
    Left msg -> liftIO $ fatal $ cs msg
    Right d -> do
      lift $ put (url, Lib3.gameStart s d)
      liftIO $ TIO.putStrLn "Welcome to Bimaru v4. Press [TAB] for available commands list"

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run' $ T.pack token
    _ -> run 

run :: IO ()
run = do
  let fullUrl = T.unpack "http://localhost:3000/"
  r <- liftIO $ post fullUrl B.empty
  run' (cs (r ^. responseBody))
  Prelude.putStrLn $ "id: " ++ cs (r ^. responseBody)

run' :: T.Text -> IO ()
run' token = do
  let fullUrl = T.unpack (T.concat ["http://", "localhost:3000/", token])
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) (fullUrl, Lib1.emptyState)