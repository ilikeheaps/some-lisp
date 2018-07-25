module Lisp.Environment.Basic where

import Lisp.Types

bindVar :: Var -> Expr -> Env -> Env
bindVar var val env = (var, val) : env

-- expressions are lists of stuff
bindVars :: Expr -> Expr -> Env -> Env
bindVars (EVar var :.: restVars) (value :.: restValues) =
  bindVar var value . bindVars restVars restValues
bindVars ENil ENil = id
bindVars _ _ = error "bindVars got wrong stuff :/"

emptyEnv :: Env
emptyEnv = []
