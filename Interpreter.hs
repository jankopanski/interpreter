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
type Loc = Int

data Value =
    VInt Int
  | VBool Bool
  | VString String
  -- | VVoid
  | VTup [Value]
  | VArr (Array.Array Int Value)
  | VMap (Map.Map Value Value)
  deriving (Show)

data Func = Func FName Type [Arg] Stmt Scope --deriving (Show)
instance Show Func where
  show (Func name t args _ scope) = "Func " ++ show name ++ " " ++ show t ++ " "
    ++ show args ++ " " ++ show scope

data Scope = Scope { innerEnv :: Env
                   , outerEnv :: Env
                   , innerFEnv :: FEnv
                   , outerFEnv :: FEnv
                   , allStore :: Store
                   } deriving (Show)

type Env = Map.Map Name Loc
type FEnv = Map.Map FName Func
type Store = Map.Map Loc Value

type Interpreter = StateT Scope IO ()

emptyEnv :: Env
emptyEnv = Map.empty

emptyFEnv :: FEnv
emptyFEnv = Map.empty

emptyStore :: Store
emptyStore = Map.empty

emptyScope :: Scope
emptyScope = Scope emptyEnv emptyEnv emptyFEnv emptyFEnv emptyStore

interpret :: Program-> IO ()
interpret p = evalStateT (evalProgram p) emptyScope

todo :: Interpreter
todo = return ()

evalProgram :: Program -> Interpreter
evalProgram (Program topdefs) = do
  let addTopDef sc@(Scope inenv outenv infenv outfenv store) (FnDef ret (Ident name) args block) =
        if Map.member name outfenv then error "TopDef function definition duplication" else
        Scope inenv outenv infenv (Map.insert name (Func name ret args (BStmt block) sc) outfenv) store
  let scope@(Scope _ _ _ outfenv _) = foldl addTopDef emptyScope topdefs
  unless (Map.member "main" outfenv) $ error "Undefined reference to 'main'"
  put scope
  case Map.lookup "main" outfenv of
    Just main@(Func "main" Int [] _ _) -> runMain main
    _ -> error "Invalid 'main' declaration"

modifyStore :: (Store -> Store) -> Interpreter
modifyStore f = modify (\(Scope inenv outenv infenv outfenv store) -> (Scope inenv outenv infenv outfenv (f store)))

execBlock :: Block -> Interpreter
execBlock (Block []) = return ()
execBlock (Block (VRet:_)) = return () -- TODO dodać typ void
execBlock (Block (Ret expr:_)) = get >>= \scope -> void $ modifyStore (Map.insert undefLoc (evalExpr expr scope))
-- execBlock (Block ((Ret expr):_)) = get >>= \scope -> modifyStore (\store -> Map.insert 0 (evalExpr expr scope) store)
-- Dodaje wartość expr na lokacje 0
execBlock (Block (stmt:stmts)) = execStmt stmt >> execBlock (Block stmts)

-- Main to wyjątek, który nie potrzebuje podmiany scopa
runMain :: Func -> Interpreter
-- runMain (Func _ _ _ (BStmt block) _) = execBlock block
runMain (Func _ _ _ (BStmt block) _) = get >>= lift . print >> execBlock block
-- TODO Main discards return
  -- dodać argsy
  -- sprawdzić ret

execStmt :: Stmt -> Interpreter

execStmt Empty = return ()

execStmt (BStmt block) = do
  Scope inenv outenv infenv outfenv store <- get
  let newoutenv = Map.union inenv outenv
      newoutfenv = Map.union infenv outfenv
  put (Scope emptyEnv newoutenv emptyFEnv newoutfenv store)
  execBlock block
  Scope _ _ _ _ store' <- get
  put (Scope inenv outenv infenv outfenv store')

execStmt (Decl _ []) = return ()
execStmt (Decl t (item:items)) = declVar t item >> execStmt (Decl t items)

undefLoc :: Loc
undefLoc = 0

newloc :: Store -> Loc
newloc store = Map.size store + 1

-- TODO kontrola typów
declVar :: Type -> Item -> Interpreter
declVar t (NoInit (Ident name)) = do
  Scope inenv outenv infenv outfenv store <- get
  when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
  let inenv' = Map.insert name undefLoc inenv
  put (Scope inenv' outenv infenv outfenv store)

declVar t (Init (Ident name) expr) = do
  scope@(Scope inenv outenv infenv outfenv store) <- get
  when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
  -- TODO kontrola typów
  let val = evalExpr expr scope
      loc = newloc store
      inenv' = Map.insert name loc inenv
      store' = Map.insert loc val store
  put (Scope inenv' outenv infenv outfenv store')

getFunc :: FName -> Scope -> Func
getFunc name (Scope _ _ infenv outfenv _) = case Map.lookup name infenv of
  Just func -> func
  Nothing -> case Map.lookup name outfenv of
    Just func -> func
    Nothing -> error "Function not defined"

-- getVar :: Name -> Scope -> Value
-- getVar name (Scope inenv outenv _ _ _) = case Map.lookup name inenv of
--   Just var -> var
--   Nothing -> case Map.lookup name outenv of
--     Just var -> var
--     Nothing -> error "Variable not defined"

evalExpr :: Expr -> Scope -> Value

-- evalExpr (EApp (Ident name) exprs) = do
--   scope@(Scope inenv outenv infenv outfenv store) <- get
--   -- when (Map.member infenv || Map.member outenv) $ error ("Invalid number of arguments")
--   --sprawdzanie liczby argumentów przy typach
--   let func@(Func _ _ args stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore)) = getFunc name infenv
--   let outenv' = Map.union inenv outenv
--       outfenv' = Map.insert name func (Map.union infenv outfenv)
--       paramZip = zip (map (\(Arg _ (Ident argname)) -> argname) args) (map evalExpr exprs)
--       inenv' = foldl (\env (paramName, paramVal) -> Map.insert paramName paramVal) emptyEnv paramZip
--       infenv' = emptyFEnv
--   execStmt stmt
--   let ret =
--   return ()
--Jeżeli call przekazuje return przez scope
evalExpr _ _ = VInt 0 -- TODO

{-
get scope
dodać argsy do inenv
wywołać blok
zebrać return z 0
przywrócić parametry
dodać funckję do fenv
-}

inbuildPrint :: [Value] -> IO ()
inbuildPrint = mapM_ (\x -> print (show x ++ "  "))
