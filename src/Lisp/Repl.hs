module Lisp.Repl where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

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
            either -- mapMaybeT lift readExpr
              (\e -> liftIO . putStrLn $ show e)
              (\expr -> do
                  mval <- readerT . runMaybeT $ eval expr
                  maybe
                    (liftIO $ putStrLn "Runtime error")
                    (\val -> liftIO $ putStrLn $ show val)
                    mval)
              (tryParse str)
            
    {-
    _ -> do case parse str of
              Nothing -> liftIO $ putStrLn "Parse failed :c"
              Just expr -> do
                val <- mapMaybeT readerT $ eval expr
                liftIO $ putStrLn $ show val
            repl
-}

  
