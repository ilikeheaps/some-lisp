module Lisp.Repl where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Functor.Identity

import MonadStuff
import Lisp.Types
import Lisp.Eval
import Lisp.Parser

-- Reader a = ReaderT Identity a
repl :: (ReaderT Env IO) ()
repl = do
  str <- liftIO getLine
  case str of
    -- TODO better handling of repl commands
    "exit" -> return ()
    _ -> evalStr str >> repl
    where evalStr :: String -> ReaderT Env IO ()
          evalStr str = do
            res <- readerT . runExceptT $ do
              expr <- withExceptT Parsing . ExceptT . pure $ tryParse str
              withExceptT Runtime $ eval expr
            either
              (\err -> liftIO . putStrLn $ show err)
              (\value -> liftIO . putStrLn $ show value)
              (res)

  
