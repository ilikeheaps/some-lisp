{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Lisp.Environment (bindName,
                         bindPureFun,
                         bindFailFun,
                         bindExceptFun,
                         bindPureForm,
                         bindFailForm,
                         bindExceptForm,
                         emptyEnv) where

import MonadStuff
import Lisp.Eval
import Lisp.Environment.Basic
import Lisp.Types

bindName :: String -> Expr -> Env -> Env
bindName s = bindVar s

bindFun :: String -> (Expr -> EvalM Expr) -> Env -> Env
bindFun name body = bindName name (EHaskellFun body)

bindPureFun :: String -> (Expr -> Expr) -> Env -> Env
bindPureFun name fun =
  bindFun name (withEvalArgs (pure . fun))

bindFailFun :: String -> String -> (Expr -> Maybe Expr) -> Env -> Env
bindFailFun name errMsg fun =
  bindFun name (withEvalArgs (maybeFail (EvalExc errMsg) . fun))

bindExceptFun :: String -> (Expr -> EvalM Expr) -> Env -> Env
bindExceptFun name fun =
  bindFun name (withEvalArgs fun)

bindPureForm :: String -> (Expr -> Expr) -> Env -> Env
bindPureForm name fun =
  bindFun name (pure . fun)

bindFailForm :: String -> String -> (Expr -> Maybe Expr) -> Env -> Env
bindFailForm name errMsg fun =
  bindFun name (maybeFail (EvalExc errMsg) . fun)

bindExceptForm :: String -> (Expr -> EvalM Expr) -> Env -> Env
bindExceptForm name fun =
  bindFun name fun
