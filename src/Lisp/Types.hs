-- common types and functions
module Lisp.Types where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Text.Parsec

type Var = String
  -- deriving (Eq, Show)

infixr 9 :.:

-- also values at the same time
data Expr = (:.:) Expr Expr -- ECons
          --  | ELambda Var Expr
          --  | EApply Expr Expr
          | EStr String
          | EVar Var -- ~atom
          | EInt Int
          | EBool Bool
          | ENil
          -- predefined functions or special forms
          -- gets a (cons) list of unevaluated arguments
          | EHaskellFun (Expr -> EvalM Expr)

data EvalError = EvalExc String
data LispError = Runtime EvalError | Parsing ParseError
-- type LispError = EvalError :+: ParseError

instance Show LispError where
  show (Runtime (EvalExc s)) = "Runtime error: "++s
  show (Parsing pe) = "Parse error: "++show pe

showOpenList :: Expr -> String
showOpenList (a :.: ENil) = show a ++ ")"
showOpenList (a :.: as) = show a ++" "++ showOpenList as
showOpenList _ = error "showOpenList got wrong stuff :/"

instance Show Expr where
  show ls@(_ :.: _) = "(" ++ showOpenList ls
  show (EStr s) = show s
  show (EVar v) = v
  show (EInt n) = show n
  -- TODO might need changes
  show (ENil) = "()"
  show (EBool b) = show b
  show (EHaskellFun _) = "<haskell function>"

type Env = [(Var, Expr)]

type LispM e a = ExceptT e (Reader Env) a
type EvalM a = LispM EvalError a

-- this will map over any list, also kinda-list (e.g. (1 2 . 3) )
mapConsM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapConsM f (e1 :.: e2) = do
  v1 <- f e1
  vs <- mapConsM f e2
  pure $ v1 :.: vs
mapConsM f expr = f expr

-- this will map over any list, also kinda-list (e.g. (1 2 . 3) )
-- oh god this type
mapCons :: (Expr -> Expr) -> Expr -> Expr
mapCons f (e1 :.: e2) = f e1 :.: mapCons f e2
mapCons f expr = f expr

mapTree :: (Expr -> Expr) -> Expr -> Expr
--mapTree f expr = mapCons (mapTree f) expr

mapTree f (e1 :.: e2) = mapTree f e1 :.: mapTree f e2
mapTree f expr = f expr

mapTreeM :: Monad m => (Expr -> m Expr) -> Expr -> m Expr
mapTreeM f (e1 :.: e2) =
  (:.:) <$> mapTreeM f e1 <*> mapTreeM f e2
mapTreeM f expr = f expr

consToList :: Expr -> Maybe [Expr]
consToList (e1 :.: e2) = (:) <$> Just e1 <*> consToList e2
consToList ENil = Just []
consToList _ = Nothing
