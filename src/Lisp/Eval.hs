module Lisp.Eval ( eval
                 , evalList) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad

import Lisp.Environment.Basic
import Lisp.Types
import MonadStuff

evalList :: Expr -> EvalM Expr
evalList expr = mapConsM eval expr

eval :: Expr -> EvalM Expr
-- TODO runtime check if lambda syntax is correct
eval f@(EVar "lambda" :.: _args :.: _body :.: ENil) = pure f
eval (function :.: argList) = do
  fun <- eval function
  case fun of
    EHaskellFun f -> f argList
    (EVar "lambda" :.: argNames :.: expr :.: ENil) -> do
      argVals <- evalList argList
      ExceptT . (local $ bindVars argNames argVals) . runExceptT
        $ eval expr
    _ -> throwE $ EvalExc "invalid function"
eval (EVar v) = do
  env <- lift ask
  maybeFail
    (EvalExc $ "variable not found: "++show v)
    (lookup v env)
eval (EInt n) = pure $ EInt n
eval (EStr s) = pure $ EStr s
eval (EBool b) = pure $ EBool b
eval ENil = pure $ ENil
eval (EHaskellFun _) = throwE $ EvalExc "shit went wrong" -- impossible syntax (there should be no way to write predefined functions)
