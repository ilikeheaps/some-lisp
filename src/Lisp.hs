import Control.Monad.Trans.Reader
import Control.Monad (when)

import Lisp.Types
import Lisp.Environment
import Lisp.Repl

defaultEnv :: Env
defaultEnv = bindPureFun "+" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a + b)
             . bindPureFun "*" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a * b)
             . bindPureFun "-" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a - b)
             . bindExceptFun "div"
               (\expr -> case expr of
                           (EInt a :.: EInt b :.: ENil) -> do
                             when (b == 0) $ lispError "div by 0"
                             pure $ EInt $ a `div` b
                           _ -> lispError "Wrong arguments for div")
             -- TODO this =if= is strict!
             . bindPureFun "if" (\(EBool p :.: v1 :.: v2 :.: ENil) ->  if p then v1 else v2)
             . bindPureFun "<" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a < b)
             . bindPureFun "=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a == b)
             . bindPureFun "<=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a <= b)
             . bindPureFun "=:=" (\(e1 :.: e2 :.: ENil) -> EBool $ e1 == e2)
             . bindPureForm "quote" (\(e :.: ENil) -> e)
             $ emptyEnv

main :: IO ()
main =
  runReaderT repl defaultEnv
