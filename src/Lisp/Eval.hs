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

keywords :: [Var]
keywords = ["lambda"]

eval :: Expr -> EvalM Expr
-- TODO runtime check if lambda syntax is correct
eval (EVar "lambda" :.: args :.: body :.: ENil) = do
  -- this is not enough:
  -- consider (lambda (x) (lambda (t) expr))
  -- !!! t is not considered a locally bound variable in expr
  let foo :: [Var] -> Expr -> EvalM Expr
      foo exc v@(EVar x) = if x `elem` exc then pure v
                           else eval v
      foo _ constant = pure constant
  argLs <- maybeFail (EvalExc "lambda: bad syntax") $ consToList args
  argNames <- mapM (\e -> case e of
                       EVar x -> pure x
                       _ -> throwE $ EvalExc "lambda: bad syntax") argLs
  bodyBound <- mapTreeM (foo $ keywords++argNames) body
  pure $ EVar "lambda" :.: args :.: bodyBound :.: ENil
eval (EVar "lambda" :.: _) =
  throwE $ EvalExc "lambda: bad syntax, expected: (lambda (<var>...) <expr>)"
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
eval f@(EHaskellFun _) = pure f
  -- throwE $ EvalExc "shit went wrong" -- impossible syntax (there should be no way to write predefined functions)
