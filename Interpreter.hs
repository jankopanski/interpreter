module Interpreter where


import Control.Monad
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
  let scope@(Scope _ _ _ outfenv _ _) = foldl addTopDef emptyScope topdefs
  unless (Map.member "main" outfenv) $ error "Undefined reference to 'main'"
  put scope
  case Map.lookup "main" outfenv of
    -- Just main@(Func "main" Int [] _ _) -> todo
    Just main@(Func "main" Int [] _ _) -> runMain main
    _ -> error "Invalid 'main' declaration"
    where
      addTopDef :: Scope -> TopDef -> Scope
      addTopDef sc@(Scope inenv outenv infenv outfenv store ret) (FnDef retType (Ident name) args block) =
        if Map.member name outfenv then error "TopDef function definition duplication" else
        Scope inenv outenv infenv (Map.insert name (Func name retType args (BStmt block) sc) outfenv) store ret


-- Main to wyjątek, który nie potrzebuje podmiany scopa
runMain :: Func -> Interpreter
-- runMain (Func _ _ _ (BStmt block) _) = execBlock block
runMain (Func _ _ _ (BStmt block) _) = get >>= lift . print >> execBlock block
-- TODO Main discards return
  -- dodać argsy
  -- sprawdzić ret

-- TODO quickfix modifyStore, sprawdzać ret value
execBlock :: Block -> Interpreter
execBlock (Block []) = return ()
-- execBlock (Block (VRet:_)) = return () -- TODO dodać typ void
-- execBlock (Block (Ret expr:_)) = get >>= \scope -> void $ modifyStore (\(s, n) -> (Map.insert undefLoc (evalExpr expr scope) s, n))
-- execBlock (Block (Ret expr:_)) = get >>= \scope -> void $ modifyStore (\store -> Map.insert undefLoc (evalExpr expr scope) store)
-- execBlock (Block ((Ret expr):_)) = get >>= \scope -> modifyStore (\store -> Map.insert 0 (evalExpr expr scope) store)
-- Dodaje wartość expr na lokacje 0
-- execBlock (Block (stmt:stmts)) = execStmt stmt >> execBlock (Block stmts)
execBlock (Block (stmt:stmts)) = do
  execStmt stmt
  Scope _ _ _ _ _ ret <- get
  case ret of
    Just _ -> return ()
    Nothing -> execBlock (Block stmts)

-- Statements --

execStmt :: Stmt -> Interpreter

execStmt Empty = return ()

execStmt (BStmt block) = do
  Scope inenv outenv infenv outfenv store ret <- get
  let newoutenv = Map.union inenv outenv
      newoutfenv = Map.union infenv outfenv
  put (Scope emptyEnv newoutenv emptyFEnv newoutfenv store ret)
  execBlock block
  Scope _ _ _ _ store' ret' <- get
  put (Scope inenv outenv infenv outfenv store' ret')

execStmt (Decl _ []) = return ()
execStmt (Decl t (item:items)) = declVar t item >> execStmt (Decl t items)
  where
    -- TODO kontrola typów
    declVar :: Type -> Item -> Interpreter
    declVar t (NoInit (Ident name)) = do
      Scope inenv outenv infenv outfenv store ret <- get
      when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
      let inenv' = Map.insert name undefLoc inenv
      put (Scope inenv' outenv infenv outfenv store ret)
    declVar t (Init (Ident name) expr) = do
      Scope inenv outenv infenv outfenv store ret <- get
      when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
      -- TODO kontrola typów
      -- let val = evalExpr expr scope
      val <- evalExpr expr
      let store'@(_, loc) = insertStore val store
          -- loc = newloc store
          inenv' = Map.insert name loc inenv
          -- store' = Map.insert loc val store
      lift $ print val
      put (Scope inenv' outenv infenv outfenv store' ret)

execStmt (Ret expr) = do
  val <- evalExpr expr
  lift $ print val
  modify (\(Scope inenv outenv infenv outfenv store _) ->
    Scope inenv outenv infenv outfenv store (Just val))

execStmt VRet = modify (\(Scope inenv outenv infenv outfenv store _) ->
  Scope inenv outenv infenv outfenv store (Just VVoid))

-- execStmt (SExp expr) = get >>= \scope -> void $ evalExpr expr scope
execStmt (SExp expr) = do
  -- scope <- get
  _ <- evalExpr expr
  return ()

-- Expressions --

-- evalExpr :: Expr -> Scope -> Value
-- evalExpr :: Expr -> Scope -> InterpreterT Value
evalExpr :: Expr -> InterpreterT Value
-- evalExpr :: Expr -> Interpreter -> Value

evalExpr (EVar (Ident name)) = do
  Scope inenv outenv _ _ store _ <- get
  if Map.member name inenv
    then return $ getValue (inenv Map.! name) store
    else if Map.member name outenv
      then return $ getValue (outenv Map.! name) store
      else error $ "Undefined variable: " ++ name

evalExpr (ELitInt n) = return $ VInt n

-- evalExpr (EApp (Ident name) exprs) scope@(Scope inenv outenv infenv outfenv store) = return $ VInt 0
--   where
--     func@(Func _ _ args stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore)) = getFunc name scope
--     outenv' = Map.union funinenv funoutenv
--     outfenv' = Map.insert name func (Map.union infenv outfenv)
--     paramNames = map (\(Arg _ (Ident argname)) -> argname) args
--     -- paramLocs = replicate (length paramNames) (repeat newloc funstore)
--     paramValues = map (`evalExpr` scope) exprs
--     (store', locs_rev) = foldl (\(store', locs) value -> let store''@(_, loc) = insertStore value store' in (store'', loc:locs)) (store, []) paramValues
--     inenv' = foldl (\inenv' (varname, loc) -> Map.insert varname loc inenv') emptyEnv (zip paramNames (reverse locs_rev))
--     infenv' = emptyFEnv
--     scope' = Scope inenv' outenv' infenv' outfenv' store'
--     scope'' = execStateT (execStmt stmt) scope'

    -- inenv' = foldl (\env (paramName, paramVal) -> Map.insert paramName paramVal env) emptyEnv paramZip
evalExpr (EApp (Ident name) exprs) = do
  scope@(Scope inenv outenv infenv outfenv store _) <- get
  -- when (Map.member infenv || Map.member outenv) $ error ("Invalid number of arguments")
  --sprawdzanie liczby argumentów przy typach
  paramValues <- mapM evalExpr exprs
  case getFunc name scope of
    Print -> lift $ inbuildPrint paramValues
    func@(Func _ _ args stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore _)) -> do
      let outenv' = Map.union funinenv funoutenv
          outfenv' = Map.insert name func (Map.union funinfenv funoutfenv)
          paramNames = map (\(Arg _ (Ident argname)) -> argname) args
          -- paramValues = mapM evalExpr exprs
          -- paramValues = map (`evalExpr` scope) exprs
      let (store', locs_rev) = foldl (\(store', locs) value ->
            let store''@(_, loc) = insertStore value store' in (store'', loc:locs))
            (funstore, []) paramValues
          inenv' = foldl (\inenv' (varname, loc) -> Map.insert varname loc inenv')
            emptyEnv (zip paramNames (reverse locs_rev))
          infenv' = emptyFEnv
          scope' = Scope inenv' outenv' infenv' outfenv' store' Nothing
      put scope'
          -- scope'' = lift $ execStateT (execStmt stmt) scope'
      execStmt stmt
      Scope _ _ _ _ _ ret <- get
      put (Scope inenv outenv infenv outfenv store Nothing)
      case ret of
        Just value -> return value
        Nothing -> return VVoid
      -- Scope inenv outenv infenv outfenv store = scope''
  -- let ret =
  -- TODO ret value
  -- return $ VInt 42

{-
get scope
dodać argsy do inenv
wywołać blok
zebrać return z 0
przywrócić parametry
dodać funckję do fenv
-}

evalExpr (EAdd expr1 addop expr2) = do
  VInt val1 <- evalExpr expr1
  VInt val2 <- evalExpr expr2
  let op = case addop of
        Plus -> (+)
        Minus -> (-)
      val = val1 `op` val2
  return $ VInt val

-- evalExpr _ _ = return $ VInt 0 -- TODO
evalExpr _ = return $ VInt 42 -- TODO
