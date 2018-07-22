import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import System.IO.Unsafe

import Lisp.Types
import Lisp.Environment
import Lisp.Repl

defaultEnv :: Env
defaultEnv = bindPureFun "+" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a + b)
             . bindPureFun "*" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a * b)
             . bindPureFun "-" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a - b)
             . bindPureFun "div" (\(EInt a :.: EInt b :.: ENil) ->  EInt $ a `div` b)
             . bindPureFun "if" (\(EBool p :.: v1 :.: v2 :.: ENil) ->  if p then v1 else v2)
             . bindPureFun "<" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a < b)
             . bindPureFun "=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a == b)
             . bindPureFun "<=" (\(EInt a :.: EInt b :.: ENil) ->  EBool $ a <= b)
             . bindPureFun "=:=" (\(e1 :.: e2 :.: ENil) -> EBool $ e1 == e2)
             $ emptyEnv

main :: IO ()
main =
  runReaderT repl defaultEnv
