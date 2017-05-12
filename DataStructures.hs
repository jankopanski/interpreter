module DataStructures where


import Control.Monad.State
import Data.Char(toLower)
import qualified Data.Map as Map
import qualified Data.Vector as Vector

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
  | VArr (Vector.Vector Value)
  | VMap (Map.Map Value Value)
  deriving (Eq, Ord)
instance Show Value where
  show (VInt n) = show n
  show (VBool b) = let s = show b in toLower (head s) : tail s
  show (VString s) = s
  show (VTup t) =
    if null t
      then "<()>"
      else "<(" ++ show (head t) ++ foldr (\e s -> (',' : show e) ++ s) "" (tail t) ++ ")>"
  show (VArr a) = show a
  show (VMap m) = show m
  show VVoid = error "Void type is not printable"

data Func = Func FName [Name] Stmt Scope | Print | IntToStr | StrToInt | ConcatStr
instance Show Func where
  show (Func name args _ scope) =
    "Func " ++ show name ++ " " ++ show args ++ " " ++ show scope
  show Print = "print"
  show IntToStr = "intToStr"
  show StrToInt = "strToInt"
  show ConcatStr = "concatStr"

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

emptyEnv :: Env
emptyEnv = Map.empty

emptyFEnv :: FEnv
emptyFEnv = Map.empty

emptyStore :: Store
emptyStore = (Map.empty, 0)

emptyScope :: Scope
emptyScope = Scope emptyEnv emptyEnv emptyFEnv outerFEnv emptyStore Nothing
  where
    outerFEnv = foldl (\m f -> Map.insert (show f) f m)
      Map.empty [Print, IntToStr, StrToInt, ConcatStr]

undefLoc :: Loc
undefLoc = 0
