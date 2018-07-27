{-# LANGUAGE LambdaCase #-}

module Lisp.Repl where

import Data.Maybe (isJust)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Functor.Identity

import Text.Parsec
import Text.ParserCombinators.Parsec

import MonadStuff
import Lisp.Types
import Lisp.Eval
import Lisp.Parser

{-
For parsing commands are organized into directives with =:= prefix and bare lisp expressions.
TODO maybe the datatype should reflect that
-}
data Command = Eval Expr
             | Exit
             | RunFile String

parseExit :: Parser Command
parseExit = const Exit <$> string "exit"

parseFile :: Parser Command
parseFile = do
  _ <- string "runFile"
  _ <- many space
  str <- many letter -- TODO allow more symbols (can't really enter many paths right now)
  pure $ RunFile str

parseSpecial :: Parser Command
parseSpecial = do
  _ <- char ':'
  _ <- many space
  parseExit <|> parseFile

parseRepl :: Parser Command
parseRepl = do
  _ <- many space
  cmd <- parseSpecial <|> (Eval <$> parseExpr)
  _ <- many space
  pure cmd

printLeft :: (MonadIO m, Show e) => (a -> m ()) -> Either e a -> m ()
printLeft = either (\err -> liftIO . putStrLn $ show err)

replEval :: Expr -> MaybeT (ReaderT Env IO) ()
replEval e = do
  res  <- MaybeT . fmap Just $ readerT . runExceptT $ withExceptT Runtime $ eval e
  printLeft
    (\value -> liftIO . putStrLn $ show value)
    (res)

-- Reader a = ReaderT Identity a
repl :: (ReaderT Env IO) ()
repl = do
  str <- liftIO getLine
  maybeRun <- runMaybeT $ printLeft
    (\case
        RunFile path -> return () -- TODO
        Exit -> MaybeT . pure $ Nothing
        Eval expr -> replEval expr)
    (parse parseRepl "repl" str)
  when (isJust maybeRun) repl


