{-# LANGUAGE ScopedTypeVariables #-}

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

extractVars :: Expr -> Maybe [String]
extractVars e = do
  argLs <- consToList e
  mapM (\(EVar x) -> Just x) argLs

mapFreeVars :: (Expr -> Expr) -> Expr -> Expr
mapFreeVars f e = go [] e
  where
    go :: [String] -> Expr -> Expr
    go bound (EVar "lambda" :.: args :.: body :.: ENil) =
      let (Just argLs) = extractVars args
      in EVar "lambda" :.: args :.: go (bound ++ argLs) body :.: ENil
    go bound v@(EVar x) = if x `elem` bound then v else f v
    go bound c@(_ :.: _) = mapCons (go bound) c
    go _ s@(EStr _) = s
    go _ n@(EInt _) = n
    go _ b@(EBool _) = b
    go _ h@(EHaskellFun _) = h
    go _ ENil = ENil

-- couldn't the restraint be just =Applicative m= ?
mapFreeVarsM :: forall m . Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapFreeVarsM f e = go [] e
  where
    go :: [String] -> Expr -> m Expr
    go bound (EVar "lambda" :.: args :.: body :.: ENil) = do
      let (Just argLs) = extractVars args
      body' <- go (bound ++ argLs) body
      pure $ EVar "lambda" :.: args :.: body' :.: ENil
    go bound v@(EVar x) = if x `elem` bound then pure v else f v
    go bound c@(_ :.: _) = mapConsM (go bound) c
    go _ s@(EStr _) = pure s
    go _ n@(EInt _) = pure n
    go _ b@(EBool _) = pure b
    go _ h@(EHaskellFun _) = pure h
    go _ ENil = pure ENil

eval :: Expr -> EvalM Expr
-- TODO runtime check if lambda syntax is correct
eval l@(EVar "lambda" :.: _args :.: _body :.: ENil) =
  mapFreeVarsM eval l
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
