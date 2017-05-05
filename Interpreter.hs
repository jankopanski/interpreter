module Interpreter where


-- import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map

import AbsMacchiato

import StaticTypeControl
import DataStructures
import InbuildFunctions
import Utils


-- interpret :: Program-> IO ()
-- interpret p = evalStateT (evalProgram p) emptyScope

interpret :: Program-> IO ()
interpret program = case typeControl program of
  Right () -> evalStateT (evalProgram program) emptyScope
  Left err -> print err

todo :: Interpreter
todo = return ()

evalProgram :: Program -> Interpreter
evalProgram (Program topdefs) = do
  let scope@(Scope _ _ _ outfenv _ _) = foldl addTopDef emptyScope topdefs
  unless (Map.member "main" outfenv) $ error "Undefined reference to 'main'"
  put scope
  case Map.lookup "main" outfenv of
    Just main@(Func "main" [] _ _) -> runMain main
    _ -> error "Invalid 'main' declaration"
    where
      addTopDef :: Scope -> TopDef -> Scope
      addTopDef sc@(Scope inenv outenv infenv outfenv store ret) (FnDef _ (Ident name) args block) =
        if Map.member name outfenv then error "TopDef function definition duplication" else
        Scope inenv outenv infenv (Map.insert name (Func name args (BStmt block) sc) outfenv) store ret

runMain :: Func -> Interpreter
runMain (Func _ _ (BStmt block) _) = execBlock block
-- runMain (Func _ _ _ (BStmt block) _) = get >>= lift . print >> execBlock block
-- dodać argsy ?
-- sprawdzić ret

execBlock :: Block -> Interpreter
execBlock (Block []) = return ()
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
    declVar :: Type -> Item -> Interpreter
    declVar t (NoInit (Ident name)) = do
      Scope inenv outenv infenv outfenv store ret <- get
      when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
      let inenv' = Map.insert name undefLoc inenv
      put (Scope inenv' outenv infenv outfenv store ret)
    declVar t (Init (Ident name) expr) = do
      Scope inenv outenv infenv outfenv store ret <- get
      when (Map.member name inenv) $ error ("Redefinition of '" ++ name ++ "'")
      val <- evalExpr expr
      let store'@(_, loc) = insertStore val store
          inenv' = Map.insert name loc inenv
      put (Scope inenv' outenv infenv outfenv store' ret)

execStmt (Ret expr) = do
  val <- evalExpr expr
  modify (\(Scope inenv outenv infenv outfenv store _) ->
    Scope inenv outenv infenv outfenv store (Just val))

execStmt VRet = modify (\(Scope inenv outenv infenv outfenv store _) ->
  Scope inenv outenv infenv outfenv store (Just VVoid))

execStmt (SExp expr) = do
  _ <- evalExpr expr
  return ()

-- Expressions --
evalExpr :: Expr -> InterpreterT Value

evalExpr (EVar (Ident name)) = do
  Scope inenv outenv _ _ store _ <- get
  if Map.member name inenv
    then return $ getValue (inenv Map.! name) store
    else if Map.member name outenv
      then return $ getValue (outenv Map.! name) store
      else error $ "Undefined variable: " ++ name

evalExpr (ELitInt n) = return $ VInt n

evalExpr ELitTrue = return $ VBool True

evalExpr ELitFalse = return $ VBool False

evalExpr (EApp (Ident name) exprs) = do
  scope@(Scope inenv outenv infenv outfenv store _) <- get
  -- when (Map.member infenv || Map.member outenv) $ error ("Invalid number of arguments")
  --sprawdzanie liczby argumentów przy typach
  paramValues <- mapM evalExpr exprs
  case getFunc name scope of
    Print -> lift $ inbuildPrint paramValues
    func@(Func _ args stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore _)) -> do
      let outenv' = Map.union funinenv funoutenv
          outfenv' = Map.insert name func (Map.union funinfenv funoutfenv)
          paramNames = map (\(Arg _ (Ident argname)) -> argname) args
      let (store', locs_rev) = foldl (\(store', locs) value ->
            let store''@(_, loc) = insertStore value store' in (store'', loc:locs))
            (funstore, []) paramValues
          inenv' = foldl (\inenv' (varname, loc) -> Map.insert varname loc inenv')
            emptyEnv (zip paramNames (reverse locs_rev))
          infenv' = emptyFEnv
          scope' = Scope inenv' outenv' infenv' outfenv' store' Nothing
      put scope'
      execStmt stmt
      Scope _ _ _ _ _ ret <- get
      put (Scope inenv outenv infenv outfenv store Nothing)
      case ret of
        Just value -> return value
        Nothing -> return VVoid

evalExpr (EString s) = return $ VString s

evalExpr (Neg expr) = do
  VInt val <- evalExpr expr
  return $ VInt $ negate val

evalExpr (Not expr) = do
  VBool bval <- evalExpr expr
  return $ VBool $ not bval

evalExpr (EMul expr1 mulop expr2) = do
  VInt val1 <- evalExpr expr1
  VInt val2 <- evalExpr expr2
  when (val2 == 0 && (mulop == Div || mulop == Mod)) $ error "Division by zero"
  let op = case mulop of
        Times -> (*)
        Div -> div
        Mod -> mod
      val = val1 `op` val2
  return $ VInt val

evalExpr (EAdd expr1 addop expr2) = do
  VInt val1 <- evalExpr expr1
  VInt val2 <- evalExpr expr2
  let op = case addop of
        Plus -> (+)
        Minus -> (-)
      val = val1 `op` val2
  return $ VInt val

evalExpr (ERel expr1 relop expr2) = do
  boxval1 <- evalExpr expr1
  boxval2 <- evalExpr expr2
  let op = case relop of
        LTH -> (<)
        LE -> (<=)
        GTH -> (>)
        GE -> (>=)
        EQU -> (==)
        NE -> (/=)
      bval = boxval1 `op` boxval2
  return $ VBool bval

evalExpr (EAnd expr1 expr2) = do
  VBool bval1 <- evalExpr expr1
  VBool bval2 <- evalExpr expr2
  return $ VBool (bval1 && bval2)

evalExpr (EOr expr1 expr2) = do
  VBool bval1 <- evalExpr expr1
  VBool bval2 <- evalExpr expr2
  return $ VBool (bval1 || bval2)
