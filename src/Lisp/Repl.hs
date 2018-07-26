{-# LANGUAGE LambdaCase #-}

module Lisp.Repl where

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

foo :: (MonadIO m, Show e) => (a -> m ()) -> Either e a -> m ()
foo = either (\err -> liftIO . putStrLn $ show err)

-- Reader a = ReaderT Identity a
repl :: (ReaderT Env IO) ()
repl = do
  str <- liftIO getLine
  foo
    (\case
        Exit -> error "actually not an error" -- TODO
        RunFile path -> return () -- TODO
        Eval expr -> do
          res <- readerT . runExceptT $ withExceptT Runtime $ eval expr
          foo
            (\value -> liftIO . putStrLn $ show value)
            (res))
    (parse parseRepl "repl" str)
  repl

  
