module StaticTypeControl where


import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import AbsMacchiato


import Debug.Trace
debug = flip trace

type Name = String
type TypeEnv = Map.Map Name Type
type TypeScope = TypeEnv
type TypeCheckerT a = State TypeScope a
type TypeCheckerExpr = TypeCheckerT Type -- type of expression
type TypeCheckerStmt = TypeCheckerT (Maybe Type) -- type function application return

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty -- Ręczna obsługa funkcji wbudowanych

emptyTypeScope :: TypeScope
emptyTypeScope = emptyTypeEnv

fromRight :: Either e a -> a
fromRight (Right a) = a

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

typeControl :: Program -> IO ()
typeControl program = return $! evalState (checkProgram program) emptyTypeScope

banan :: Program -> IO()
banan program = do
  print $ evalState (checkProgram program) emptyTypeScope
  putStrLn "banan"

checkProgram :: Program -> TypeCheckerT ()
checkProgram (Program topdefs) = mapM_ checkTopDef topdefs

checkTopDef :: TopDef -> TypeCheckerT ()
checkTopDef token@(FnDef t (Ident name) args block) = do
  env <- get -- env before function definition
  let params = map (\(Arg argtype (Ident argname)) -> (argtype, argname)) args
      env' = Map.insert name (Fun t (map fst params)) env -- env' with function definition
      env'' = foldl (\e (argtype, argname) -> Map.insert argname argtype e) env' params
      -- env'' with function definition and parameters
  put env''
  ret <- checkBlock block --`debug` show block
  put env'
  let ret_type = fromMaybe Void ret
  unless (ret_type == t) $ error $ show token

checkBlock :: Block -> TypeCheckerStmt
checkBlock token@(Block stmts) = do
  env <- get
  return_maybe_types <- mapM typeOfStmt stmts
  put env
  let return_types = catMaybes return_maybe_types
      types_number = length $ nub return_types
  return $ case types_number of
    0 -> Nothing
    1 -> Just $ head return_types
    _ -> error $ show token

-- Statements --
typeOfStmt :: Stmt -> TypeCheckerStmt

typeOfStmt Empty = return Nothing

typeOfStmt (BStmt block) = do
  env <- get
  return_type <- checkBlock block
  put env
  return return_type

-- typeOfStmt token@(FunLoc t (Ident name) args stmt) = do
--   (env, ret) <- get
--   res <- typeOfStmt stmt
--   case res of
--     Left err -> return $ Left err
--     _ -> do
--     (_, ret') <- get
--     let argtypes = map (\(Arg argtype _) -> argtype) args
--         env' = Map.insert name (Fun t argtypes) env
--         b = case ret' of
--           Nothing -> t == Void
--           Just rt -> rt == t
--     if b then put (env', ret) >> emptyStmt else return $ Left $ show token


-- TODO przypisanie nie sprawdza typu
typeOfStmt token@(Decl t items) = mapM_ checkDecl items >> return Nothing
  where
    checkDecl :: Item -> TypeCheckerStmt
    checkDecl (NoInit (Ident name)) = modify (Map.insert name t) >> return Nothing
    checkDecl (Init (Ident name) expr) = do
      expr_type <- typeOfExpr expr
      if expr_type == t
        then checkDecl (NoInit (Ident name))
        else error $ show token

typeOfStmt token@(Ass (Ident name) expr) = do
  env <- get
  expr_type <- typeOfExpr expr
  case Map.lookup name env of
    Nothing -> error $ show token
    Just t -> if expr_type == t then return Nothing else error $ show token

typeOfStmt token@(Incr (Ident name)) = isIdentInt token name

typeOfStmt token@(Decr (Ident name)) = isIdentInt token name

-- -- typeOfStmt token@(Cond expr _) = isExprBool token expr
-- --
-- -- typeOfStmt token@(CondElse expr _ _) = isExprBool token expr

typeOfStmt token@(Cond expr stmt) = do
  expr_type <- typeOfExpr expr
  if expr_type == Bool then typeOfStmt stmt else error $ show token

-- typeOfStmt token@(CondElse expr stmt1 stmt2) = do
--   et <- typeOfExpr expr
--   case et of
--     Left err -> return $ Left err
--     Right

typeOfStmt token@(CondElse expr stmt1 stmt2) = do
  expr_type <- typeOfExpr expr
  when (expr_type /= Bool) $ error $ show token
  env <- get
  return_type1 <- typeOfStmt stmt1
  return_type2 <- typeOfStmt stmt2
  put env
  return $ case (return_type1, return_type2) of
    (Just t1, Just t2) -> if t1 == t2 then return_type1 else error $ show token
    (Nothing, Just _) -> return_type1
    (Just _, Nothing) -> return_type2
    (Nothing, Nothing) -> Nothing

-- -- TODO przemyśleć pętle
-- typeOfStmt token@(While expr _) = do
--   et <- typeOfExpr expr
--   case et of
--     Left err -> return $ Left err
--     Right Bool -> emptyStmt
--     _ -> failStmt token
--
-- -- TODO pętle do poprawy
-- typeOfStmt token@(ForUp _ expr1 expr2 _) = isExprInt token expr1 >> isExprInt token expr2
--
-- typeOfStmt token@(ForDown _ expr1 expr2 _) = isExprInt token expr1 >> isExprInt token expr2
--
typeOfStmt (Ret expr) = do
  expr_type <- typeOfExpr expr --`debug` show expr -- TODO
  return $ Just expr_type

typeOfStmt VRet = return $ Just Void

typeOfStmt (SExp expr) = typeOfExpr expr >> return Nothing

isIdentInt :: Stmt -> Name -> TypeCheckerStmt
isIdentInt token name = do
  env <- get
  case Map.lookup name env of
    Nothing -> error $ "Variable type not found in entvironment " ++ show token
    Just t -> if t == Int then return Nothing else error $ show token

-- isExprInt :: Stmt -> Expr -> TypeChecker
-- isExprInt token expr = do
--   et <- typeOfExpr expr
--   case et of
--     Left err -> return $ Left err
--     Right Int -> emptyStmt
--     _ -> failStmt token
--
-- isExprBool :: Stmt -> Expr -> TypeChecker
-- isExprBool token expr = do
--   et <- typeOfExpr expr
--   case et of
--     Left err -> return $ Left err
--     Right Bool -> emptyStmt
--     _ -> failStmt token
--
-- Expressions --
typeOfExpr :: Expr -> TypeCheckerT Type

typeOfExpr token@(EVar (Ident name)) = do
  env <- get
  case Map.lookup name env of
    Just t -> return t
    Nothing -> error $ "Variable type not found in entvironment " ++ show token

typeOfExpr (ELitInt _) = return Int
--
typeOfExpr ELitTrue = return Bool
--
typeOfExpr ELitFalse = return Bool

typeOfExpr token@(EApp (Ident name) exprs) = do
  env <- get
  case Map.lookup name env of
    Nothing -> return $ lookupInbuilds name
    Just (Fun return_type arg_types) -> do
      expr_types <- mapM typeOfExpr exprs
      let check = foldl (\b (t1, t2) -> b && t1 == t2) True (zip expr_types arg_types)
      if check then return return_type else error $ show token
    Just _ -> error $ "Variable " ++ name ++ " is not a function " ++ show token
    where
      lookupInbuilds :: Name -> Type
      lookupInbuilds "print" = Void
      -- TODO jeśli nie ma w inbuilds funkcji to zwrócić błąd
      -- lookupInbuilds _ = Left $ "Function " ++ name ++ " undefined " ++ show token

typeOfExpr (EString _) = return Str

typeOfExpr token@(Neg expr) = typeOfExpr expr >>=
  \t -> if t == Int then return Int else error $ show token

typeOfExpr token@(Not expr) = typeOfExpr expr >>=
  \t -> if t == Bool then return Bool else error $ show token

typeOfExpr token@(EMul expr1 _ expr2) = typeOfAddMul token expr1 expr2

typeOfExpr token@(EAdd expr1 _ expr2) = typeOfAddMul token expr1 expr2

typeOfExpr token@(ERel expr1 _ expr2) = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  if t1 == t2 then return Bool else error $ show token

typeOfExpr token@(EAnd expr1 expr2) = typeOfAndOr token expr1 expr2

typeOfExpr token@(EOr expr1 expr2) = typeOfAndOr token expr1 expr2

typeOfAddMul :: Expr -> Expr -> Expr -> TypeCheckerExpr
typeOfAddMul token expr1 expr2 = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  if t1 == Int && t2 == Int then return Int else error $ show token

typeOfAndOr :: Expr -> Expr -> Expr -> TypeCheckerT Type
typeOfAndOr token expr1 expr2 = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  if t1 == Bool && t2 == Bool then return Bool else error $ show token
