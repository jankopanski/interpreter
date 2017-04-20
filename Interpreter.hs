module Interpreter where


import LexMacchiato
import ParMacchiato
import SkelMacchiato
import PrintMacchiato
import AbsMacchiato

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Array as Array


type Name = String
type FName = String
type Loc = Integer

data Value =
    VInt Int
  | VBool Bool
  | VString String
  -- | VVoid
  | VTup [Value]
  | VArr (Array.Array Int Value)
  | VMap (Map.Map Value Value)
-- data ArrValue a = VArr (Array.Array Int a)
-- type ArrValue a = Array.Array Int a
-- data MapValue k v = VMap (Map.Map Value v)

data Func = Func FName Type [Arg] Stmt Scope
data Scope = Scope Env FEnv Store

type Env = Map.Map Name Loc
type FEnv = Map.Map FName Func
type Store = Map.Map Loc Value

type Interpreter = StateT Scope IO ()

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty

interpret :: Program-> IO ()
interpret (Program topdefs) = print topdefs

todo :: Interpreter
todo = return ()

evalProgram :: Program -> Interpreter
evalProgram (Program topdefs) = do
  let addTopDef sc@(Scope env fenv store) (FnDef ret (Ident name) args block) =
        if Map.member name fenv then error "TopDef function definition duplication" else
        Scope env (Map.insert name (Func name ret args (BStmt block) sc) fenv) store
  let scope@(Scope _ fenv _) = foldl addTopDef emptyScope topdefs
  unless (Map.member "main" fenv) $ error "Undefined reference to 'main'"
  put scope
  case Map.lookup "main" fenv of
    Just main@(Func "main" Int [] _ _) -> runFunc main
    _ -> error "Invalid 'main' declaration"

runFunc :: Func -> Interpreter
runFunc func = todo
