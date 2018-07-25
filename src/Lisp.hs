{-# LANGUAGE LambdaCase #-}
import Control.Monad.Trans.Reader
import Control.Monad (when)

import Lisp.Types
import Lisp.Environment
import Lisp.Repl
import Lisp.Eval

defaultEnv :: Env
defaultEnv = bindPureFun "+" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a + b)
             . bindPureFun "*" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a * b)
             . bindPureFun "-" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a - b)
             . bindExceptFun "div"
               (\case (EInt a :.: EInt b :.: ENil) -> do
                        when (b == 0) $ lispError "div by 0"
                        pure $ EInt $ a `div` b
                      _ -> lispError "Wrong arguments for div")
             -- TODO this =if= is strict!
             . bindPureFun "if" (\(EBool p :.: v1 :.: v2 :.: ENil) ->  if p then v1 else v2)
             . bindPureFun "<" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a < b)
             . bindPureFun "=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a == b)
             . bindPureFun "<=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a <= b)
             . bindPureFun "=:=" (\(e1 :.: e2 :.: ENil) -> EBool $ e1 == e2)
             -- could we replace =quote= with =.=?
             . bindPureForm "quote" (\(e :.: ENil) -> e)
             . bindPureFun "." (\(e1 :.: e2 :.: ENil) -> e1 :.: e2)
{- TODO how to best express this?
(eval expr 0) = expr
(eval expr n) = (eval (eval expr) (n-1))
-}
             . bindExceptForm "eval"
               (\case (expr :.: EInt n :.: ENil) -> iterateM eval (pure expr) !! n
                      _ -> lispError "nope")
             $ emptyEnv

iterateM :: Monad m => (a -> m a) -> m a -> [m a]
iterateM f x = [x] ++ iterateM f (x >>= f)

main :: IO ()
main =
  runReaderT repl defaultEnv
