module DataStructures where


import Control.Monad.State
import Data.Char(toLower)
import qualified Data.Map as Map
import qualified Data.Array as Array

import AbsMacchiato


type Name = String
type FName = String
type Loc = Int

data Value =
    VInt Integer
  | VBool Bool
  | VString String
  | VVoid
  | VTup [Value]
  | VArr (Array.Array Int Value)
  | VMap (Map.Map Value Value)
  deriving (Eq, Ord)
instance Show Value where
  show (VInt n) = show n
  show (VBool b) = let s = show b in toLower (head s) : tail s
  show (VString s) = s
  show (VTup t) = show t
  show (VArr a) = show a
  show (VMap m) = show m
  show VVoid = error "Void type is not printable"

data Func = Func FName [Name] Stmt Scope | Print | IntToStr | StrToInt
instance Show Func where
  show (Func name args _ scope) =
    "Func " ++ show name ++ " " ++ show args ++ " " ++ show scope
  show Print = "print"
  show IntToStr = "intToStr"
  show StrToInt = "strToInt"

data Scope = Scope { innerEnv :: Env
                   , outerEnv :: Env
                   , innerFEnv :: FEnv
                   , outerFEnv :: FEnv
                   , allStore :: Store
                   , returnValue :: Maybe Value
                   } deriving (Show)

type Env = Map.Map Name Loc
type FEnv = Map.Map FName Func
type Store = (Map.Map Loc Value, Loc)

type InterpreterT = StateT Scope IO
type Interpreter = InterpreterT ()
-- type Interpreter = StateT Scope IO ()

emptyEnv :: Env
emptyEnv = Map.empty

emptyFEnv :: FEnv
emptyFEnv = Map.empty

emptyStore :: Store
emptyStore = (Map.empty, 0)

emptyScope :: Scope
emptyScope = Scope emptyEnv emptyEnv emptyFEnv outerFEnv emptyStore Nothing
  where
    outerFEnv = foldl (\m f -> Map.insert (show f) f m) Map.empty [Print, IntToStr, StrToInt]
    -- outerFEnv = Map.singleton (show Print) Print

undefLoc :: Loc
undefLoc = 0

-- newloc :: Store -> Loc
-- newloc store = Map.size store + 1

-- newloc :: Store -> Loc
-- newloc (_, n) ->
