{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Lisp.Environment where

import Control.Monad.Trans.Except

import MonadStuff
import Lisp.Eval
import Lisp.Environment.Basic
import Lisp.Types

bindName :: String -> Expr -> Env -> Env
bindName s = bindVar s

bindFun :: String -> (Expr -> EvalM Expr) -> Env -> Env
bindFun name body = bindName name (EHaskellFun body)

bindPureFun :: String -> (Expr -> Expr) -> Env -> Env
bindPureFun name fun = bindName name (EHaskellFun action)
  where action args = do
          values <- evalList args
          pure $ fun values

bindFailFun :: String -> String -> (Expr -> Maybe Expr) -> Env -> Env
bindFailFun name errMsg fun = bindName name (EHaskellFun action)
  where action args = do
          values <- evalList args
          maybeFail (EvalExc errMsg) $ fun values

bindExceptFun :: String -> (forall m . Monad m => Expr -> ExceptT EvalError m Expr) -> Env -> Env
bindExceptFun name fun = bindName name (EHaskellFun action)
  where action :: Expr -> EvalM Expr
        action args = do
          values <- evalList args
          fun values

bindPureForm :: String -> (Expr -> Expr) -> Env -> Env
bindPureForm name fun = bindName name (EHaskellFun (pure . fun))

emptyEnv :: Env
emptyEnv = []
