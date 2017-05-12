module Interpreter where


-- import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import AbsMacchiato

import StaticTypeControl
import DataStructures
import InbuildFunctions
import Utils


-- interpret :: Program-> IO ()
-- interpret p = evalStateT (evalProgram p) emptyScope

-- interpret :: Program-> IO ()
-- interpret program = case typeControl program of
--   Right () -> evalStateT (evalProgram program) emptyScope
--   Left err -> print err

interpret :: Program -> IO ()
interpret program = typeControl program >> evalStateT (evalProgram program) emptyScope

-- TODO konwersje string int

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
        if Map.member name outfenv
          then error "TopDef function definition duplication"
          else
            let argnames = map (\(Arg _ (Ident argname)) -> argname) args
                func = Func name argnames (BStmt block) sc
            in Scope inenv outenv infenv (Map.insert name func outfenv) store ret

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

execStmt (FunLoc _ (Ident name) args stmt) = do
  scope@(Scope inenv outenv infenv outfenv store ret) <- get
  when (Map.member name infenv) $ error ("Redefinition of '" ++ name ++ "'")
  let argnames = map (\(Arg _ (Ident argname)) -> argname) args
      func = Func name argnames stmt scope
      infenv' = Map.insert name func infenv
  put (Scope inenv outenv infenv' outfenv store ret)

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

execStmt (Ass (Ident name) expr) = do
  scope@(Scope inenv outenv _ _ _ _) <- get
  val <- evalExpr expr
  case Map.lookup name inenv of
    Just loc -> updateInenvValue loc val scope
    Nothing -> case Map.lookup name outenv of
      Just loc -> updateOutenvValue loc val scope
      Nothing -> error $ "Undefined variable: '" ++ name ++ "'"
  where
    updateInenvValue :: Loc -> Value -> Scope -> Interpreter
    updateInenvValue 0 val (Scope inenv outenv infenv outfenv store ret) = do
      let store'@(_, loc') = insertStore val store
          inenv' = Map.insert name loc' inenv
      put (Scope inenv' outenv infenv outfenv store' ret)
    updateInenvValue loc val (Scope inenv outenv infenv outfenv store ret) = do
      let store' = updateStore loc val store
      put (Scope inenv outenv infenv outfenv store' ret)
    updateOutenvValue :: Loc -> Value -> Scope -> Interpreter
    updateOutenvValue 0 val (Scope inenv outenv infenv outfenv store ret) = do
      let store'@(_, loc') = insertStore val store
          outenv' = Map.insert name loc' outenv
      put (Scope inenv outenv' infenv outfenv store' ret)
    updateOutenvValue loc val (Scope inenv outenv infenv outfenv store ret) = do
      let store' = updateStore loc val store
      put (Scope inenv outenv infenv outfenv store' ret)

execStmt (ArrAss (Ident name) expr1 expr2) = do
  scope@(Scope inenv outenv _ _ _ _) <- get
  VInt n <- evalExpr expr1
  val <- evalExpr expr2
  let n_int = fromInteger n
  case Map.lookup name inenv of
    Just loc -> updateValue loc n_int val scope
    Nothing -> case Map.lookup name outenv of
      Just loc -> updateValue loc n_int val scope
      Nothing -> error $ "Undefined variable: '" ++ name ++ "'"
    where
      updateValue :: Loc -> Int -> Value -> Scope -> Interpreter
      updateValue loc n_int val (Scope inenv outenv infenv outfenv store ret) = do
        let VArr arr = getValueByLoc loc store
        when (n_int >= Vector.length arr) $ error ("Array index out of bound: " ++ name)
        let arr' = arr Vector.// [(n_int, val)]
            store' = updateStore loc (VArr arr') store
        put (Scope inenv outenv infenv outfenv store' ret)

execStmt (MapAss (Ident name) expr1 expr2) = do
  scope@(Scope inenv outenv _ _ _ _) <- get
  key <- evalExpr expr1
  val <- evalExpr expr2
  case Map.lookup name inenv of
    Just loc -> updateValue loc key val scope
    Nothing -> case Map.lookup name outenv of
      Just loc -> updateValue loc key val scope
      Nothing -> error $ "Undefined variable: '" ++ name ++ "'"
    where
      updateValue :: Loc -> Value -> Value -> Scope -> Interpreter
      updateValue loc key val (Scope inenv outenv infenv outfenv store ret) = do
        let VMap m = getValueByLoc loc store
        let m' = Map.insert key val m
            store' = updateStore loc (VMap m') store
        put (Scope inenv outenv infenv outfenv store' ret)

    -- updateInenvValue :: Loc -> Value -> Scope -> Interpreter
    -- updateInenvValue 0 _ _ = error $ "Undefined variable: '" ++ name ++ "'"
    -- updateInenvValue loc val (Scope inenv outenv infenv outfenv store ret) = do
    --   let store' = updateStore loc val store
    --   put (Scope inenv outenv infenv outfenv store' ret)
    -- updateOutenvValue :: Loc -> Value -> Scope -> Interpreter
    -- updateOutenvValue 0 _ _ = error $ "Undefined variable: '" ++ name ++ "'"
    -- updateOutenvValue loc val (Scope inenv outenv infenv outfenv store ret) = do
    --   let store' = updateStore loc val store
    --   put (Scope inenv outenv infenv outfenv store' ret)
  --


execStmt (Incr ident) = do
  VInt n <- evalExpr (EVar ident)
  execStmt (Ass ident (ELitInt (n + 1)))

execStmt (Decr ident) = do
  VInt n <- evalExpr (EVar ident)
  execStmt (Ass ident (ELitInt (n - 1)))

execStmt (Cond expr stmt) = execStmt (CondElse expr stmt Empty)

execStmt (CondElse expr stmt1 stmt2) = do
  Scope inenv outenv infenv outfenv _ _ <- get
  VBool b <- evalExpr expr
  if b then execStmt stmt1 else execStmt stmt2
  modify (\(Scope _ _ _ _ store' ret') -> Scope inenv outenv infenv outfenv store' ret')

execStmt token@(While expr stmt) = do
  Scope inenv outenv infenv outfenv _ _ <- get
  VBool b <- evalExpr expr
  when b $ do
    execStmt stmt
    modify (\(Scope _ _ _ _ store' ret') -> Scope inenv outenv infenv outfenv store' ret')
    execStmt token

execStmt (ForUp (Ident name) expr1 expr2 stmt) = do
  val@(VInt n1) <- evalExpr expr1
  VInt n2 <- evalExpr expr2
  Scope inenv outenv infenv outfenv store ret <- get
  let store'@(_, loc) = insertStore val store
      inenv' = Map.insert name loc inenv
  put (Scope inenv' outenv infenv outfenv store' ret)
  iterateUp n1 n2 loc
  modify (\(Scope _ _ _ _ store'' ret') -> Scope inenv outenv infenv outfenv store'' ret')
    where
      iterateUp :: Integer -> Integer -> Loc -> Interpreter
      iterateUp n1 n2 loc
        | n1 > n2 = return ()
        | n1 <= n2 = do
          Scope inenv outenv infenv outfenv _ _ <- get
          execStmt stmt
          Scope _ _ _ _ store' ret' <- get

          let VInt n1' = getValueByLoc loc store'
              n1'' = n1' + 1
              store'' = updateStore loc (VInt n1'') store'
          put (Scope inenv outenv infenv outfenv store'' ret')
          iterateUp n1'' n2 loc

execStmt (ForDown (Ident name) expr1 expr2 stmt) = do
  val@(VInt n1) <- evalExpr expr1
  VInt n2 <- evalExpr expr2
  Scope inenv outenv infenv outfenv store ret <- get
  let store'@(_, loc) = insertStore val store
      inenv' = Map.insert name loc inenv
  put (Scope inenv' outenv infenv outfenv store' ret)
  iterateDown n1 n2 loc
  modify (\(Scope _ _ _ _ store'' ret') -> Scope inenv outenv infenv outfenv store'' ret')
    where
      iterateDown :: Integer -> Integer -> Loc -> Interpreter
      iterateDown n1 n2 loc
        | n1 < n2 = return ()
        | n1 >= n2 = do
          Scope inenv outenv infenv outfenv _ _ <- get
          execStmt stmt
          Scope _ _ _ _ store' ret' <- get
          let VInt n1' = getValueByLoc loc store'
              n1'' = n1' - 1
              store'' = updateStore loc (VInt n1'') store'
          put (Scope inenv outenv infenv outfenv store'' ret')
          iterateDown n1'' n2 loc

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

evalExpr (EVar (Ident name)) = get >>= \scope -> return $ getValueByName name scope

-- evalExpr (EVar (Ident name)) = do
--   Scope inenv outenv _ _ store _ <- get
--   if Map.member name inenv
--     then return $ getValueByLoc (inenv Map.! name) store
--     else if Map.member name outenv
--       then return $ getValueByLoc (outenv Map.! name) store
--       else error $ "Undefined variable: " ++ name

evalExpr (ELitInt n) = return $ VInt n

evalExpr ELitTrue = return $ VBool True

evalExpr ELitFalse = return $ VBool False

evalExpr (EApp (Ident name) exprs) = do
  scope@(Scope inenv outenv infenv outfenv store _) <- get
  -- when (Map.member infenv || Map.member outenv) $ error ("Invalid number of arguments")
  --sprawdzanie liczby argumentów przy typach
  argvalues <- mapM evalExpr exprs
  case getFunc name scope of
    Print -> lift $ inbuildPrint argvalues
    IntToStr -> return $ intToStr $ head argvalues
    StrToInt -> return $ strToInt $ head argvalues
    func@(Func _ argnames stmt (Scope funinenv funoutenv funinfenv funoutfenv funstore _)) -> do
      let outenv' = Map.union funinenv funoutenv
          -- Adding function definition to function scope for recursion
          outfenv' = Map.insert name func (Map.union funinfenv funoutfenv)
      let (store', locs_rev) = foldl (\(store', locs) value ->
            let store''@(_, loc) = insertStore value store' in (store'', loc:locs))
            (funstore, []) argvalues
            -- Adding args to inenv
          inenv' = foldl (\inenv' (argname, loc) -> Map.insert argname loc inenv')
            emptyEnv (zip argnames (reverse locs_rev))
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

evalExpr (ENewTup exprs) = fmap VTup (mapM evalExpr exprs)

-- evalExpr (ENewTup exprs) = do--return $ VTup $ mapM evalExpr exprs
--   -- let sth = foldM (\l e -> evalExpr e : l) [] exprs
--   sth <- mapM evalExpr exprs
--   return $ VTup sth

-- access
-- evalExpr (EAccTup (Ident name) expr) = do
--   scope@(Scope inenv outenv infenv outfenv store ret) <- get


  -- Scope inenv outenv _ _ store _ <- get
  -- if Map.member name inenv
  --   then return $ getValueByLoc (inenv Map.! name) store
  --   else if Map.member name outenv
  --     then return $ getValueByLoc (outenv Map.! name) store
  --     else error $ "Undefined variable: " ++ name

evalExpr (EAccTup (Ident name) n) = do
  scope <- get
  -- VInt n <- evalExpr expr
  let n_int = fromInteger n
      VTup tup = getValueByName name scope
  when (n_int >= length tup) $ error ("Tuple index out of bound: " ++ name)
  return $ tup !! n_int

evalExpr token@(ENewArr t expr) = do
  VInt n <- evalExpr expr
  let vec = Vector.replicate (fromInteger n) (getDefValue t)
  return $ VArr vec
    where
      getDefValue :: Type -> Value
      getDefValue Int = VInt 0
      getDefValue Str = VString ""
      getDefValue Bool = VBool False
      getDefValue (Tup types) = VTup $ map getDefValue types
      getDefValue _ = error $ show token

evalExpr (EAccArr (Ident name) expr) = do
  scope <- get
  VInt n <- evalExpr expr
  let n_int = fromInteger n
      VArr arr = getValueByName name scope
  when (n_int >= Vector.length arr) $ error ("Array index out of bound: " ++ name)
  return $ arr Vector.! n_int

evalExpr (ENewMap _ _) = return $ VMap Map.empty

evalExpr (EAccMap (Ident name) expr) = do
  key <- evalExpr expr
  scope <- get
  let VMap m = getValueByName name scope
  case Map.lookup key m of
    Nothing -> error ("No entry for key '" ++ show key ++ "' in map '" ++ name ++ "'")
    Just value -> return value

evalExpr (EHasMap (Ident name) expr) = do
  key <- evalExpr expr
  scope <- get
  let VMap m = getValueByName name scope
  return $ VBool $ Map.member key m

-- TODO del
  -- evalExpr (EDelMap (Ident name) expr) = do
  --   key <- evalExpr expr
  --   scope@(Scope inenv outenv infenv outfenv store ret) <- get

evalExpr (EDelMap (Ident name) expr) = do
  scope@(Scope inenv outenv _ _ _ _) <- get
  key <- evalExpr expr
  case Map.lookup name inenv of
    Just loc -> deleteEntry loc key scope
    Nothing -> case Map.lookup name outenv of
      Just loc -> deleteEntry loc key scope
      Nothing -> error $ "Undefined variable: '" ++ name ++ "'"
    where
      deleteEntry :: Loc -> Value -> Scope -> InterpreterT Value
      deleteEntry loc key (Scope inenv outenv infenv outfenv store ret) = do
        let VMap m = getValueByLoc loc store
        unless (Map.member key m) $ error ("No entry for key '" ++ show key ++ "' in map '" ++ name ++ "'")
        let val = m Map.! key
            m' = Map.delete key m
            store' = updateStore loc (VMap m') store
        error $ show store'
        put (Scope inenv outenv infenv outfenv store' ret)
        return val

    -- let VMap m = getValueByName name scope
    -- case Map.lookup key m of
    --   Nothing -> error ("No entry for key '" ++ show key ++ "' in map '" ++ name ++ "'")
    --   Just value -> return value

-- evalExpr (EAccArr (Ident name) expr) = do
--   VInt n <- evalExpr n
--   let

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
