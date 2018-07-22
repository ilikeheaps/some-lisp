module Lisp.Environment where

import Lisp.Eval
import Lisp.Environment.Basic
import Lisp.Types

bindName :: String -> Expr -> Env -> Env
bindName s = bindVar s

bindFun :: String -> (Expr -> EvalM Expr) -> Env -> Env
bindFun name body = bindName name (EHaskellFun body)

-- TODO rename to bindStrictFun?
-- the bound function will be strict
bindPureFun :: String -> (Expr -> Expr) -> Env -> Env
bindPureFun name fun = bindName name (EHaskellFun action)
  where action args = do
          values <- evalList args
          pure $ fun values

bindLazyFun :: String -> (Expr -> Expr) -> Env -> Env
bindLazyFun name fun = bindName name (EHaskellFun (pure . fun))

emptyEnv :: Env
emptyEnv = []
