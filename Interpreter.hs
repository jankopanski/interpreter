module Interpreter where


-- import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
-- import qualified Data.Array as Array

-- import LexMacchiato
-- import ParMacchiato
-- import SkelMacchiato
-- import PrintMacchiato
import AbsMacchiato

import DataStructures
-- import Statements
import InbuildFunctions
import Utils


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

-- Main to wyjątek, który nie potrzebuje podmiany scopa
runMain :: Func -> Interpreter
-- runMain (Func _ _ _ (BStmt block) _) = execBlock block
runMain (Func _ _ _ (BStmt block) _) = get >>= lift . print >> execBlock block
-- TODO Main discards return
  -- dodać argsy
  -- sprawdzić ret

-- TODO quickfix modifyStore
execBlock :: Block -> Interpreter
execBlock (Block []) = return ()
execBlock (Block (VRet:_)) = return () -- TODO dodać typ void
execBlock (Block (Ret expr:_)) = get >>= \scope -> void $ modifyStore (\(s, n) -> (Map.insert undefLoc (evalExpr expr scope) s, n))
-- execBlock (Block (Ret expr:_)) = get >>= \scope -> void $ modifyStore (\store -> Map.insert undefLoc (evalExpr expr scope) store)
-- execBlock (Block ((Ret expr):_)) = get >>= \scope -> modifyStore (\store -> Map.insert 0 (evalExpr expr scope) store)
-- Dodaje wartość expr na lokacje 0
execBlock (Block (stmt:stmts)) = execStmt stmt >> execBlock (Block stmts)

-- Statements --

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
      store'@(_, loc) = insertStore val store
      -- loc = newloc store
      inenv' = Map.insert name loc inenv
      -- store' = Map.insert loc val store
  put (Scope inenv' outenv infenv outfenv store')

-- Expressions --

evalExpr :: Expr -> Scope -> Value

evalExpr (EApp (Ident name) exprs) scope@(Scope inenv outenv infenv outfenv store) = VInt 0
  where
    func@(Func _ _ args stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore)) = getFunc name scope
    outenv' = Map.union funinenv funoutenv
    outfenv' = Map.insert name func (Map.union infenv outfenv)
    paramNames = map (\(Arg _ (Ident argname)) -> argname) args
    -- paramLocs = replicate (length paramNames) (repeat newloc funstore)
    paramValues = map (`evalExpr` scope) exprs
    (store', locs_rev) = foldl (\(store', locs) value -> let store''@(_, loc) = insertStore value store' in (store'', loc:locs)) (store, []) paramValues
    inenv' = foldl (\inenv' (varname, loc) -> Map.insert varname loc inenv') emptyEnv (zip paramNames (reverse locs_rev))
    infenv' = emptyFEnv
    scope' = Scope inenv' outenv' infenv' outfenv' store'
    scope'' = execStateT (execStmt stmt) scope'
    -- inenv' = foldl (\env (paramName, paramVal) -> Map.insert paramName paramVal env) emptyEnv paramZip
-- TODO
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

{-
get scope
dodać argsy do inenv
wywołać blok
zebrać return z 0
przywrócić parametry
dodać funckję do fenv
-}

evalExpr _ _ = VInt 0 -- TODO
