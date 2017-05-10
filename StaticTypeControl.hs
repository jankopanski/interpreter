module StaticTypeControl where


import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Either
import Control.Arrow
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
--
-- typeOfStmt (BStmt block) = do
--   (env, _) <- get
--   _ <- checkBlock block
--   modify (\(_, ret) -> (env, ret))
--   emptyStmt
--
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

-- typeOfStmt token@(Ass (Ident name) expr) = do
--   (env, _) <- get
--   et <- typeOfExpr expr
--   case Map.lookup name env of
--     Nothing -> failStmt token
--     Just t -> case et of
--       Right t' -> if t == t' then emptyStmt else failStmt token
--       Left err -> return $ Left err
--
-- typeOfStmt token@(Incr (Ident name)) = isIdentInt token name
--
-- typeOfStmt token@(Decr (Ident name)) = isIdentInt token name
--
-- -- typeOfStmt token@(Cond expr _) = isExprBool token expr
-- --
-- -- typeOfStmt token@(CondElse expr _ _) = isExprBool token expr
--
-- typeOfStmt token@(Cond expr stmt) = do
--   et <- typeOfExpr expr
--   case et of
--     Left err -> return $ Left err
--     Right Bool -> typeOfStmt stmt
--     _ -> failStmt token
--
-- -- typeOfStmt token@(CondElse expr stmt1 stmt2) = do
-- --   et <- typeOfExpr expr
-- --   case et of
-- --     Left err -> return $ Left err
-- --     Right
--
-- typeOfStmt token@(CondElse expr stmt1 stmt2) = do
--   et <- typeOfExpr expr `debug` (show expr ++ " if et")
--   case et of
--     Left err -> return $ Left err `debug` err
--     -- Right Bool -> typeOfStmt stmt1 >> typeOfStmt stmt2
--     Right Bool -> do
--       (env, _) <- get
--       _ <- typeOfStmt stmt1
--       (_, ret1) <- get
--       _ <- typeOfStmt stmt2
--       (_, ret2) <- get
--       case (ret1, ret2) of
--         (Just t1, Just t2) -> if t1 == t2
--                                 then put (env, ret1) >> emptyStmt
--                                 else failStmt token
--         (Nothing, Just t) -> put (env, Just t) >> emptyStmt
--         (Just t, Nothing) -> put (env, Just t) >> emptyStmt
--         (Nothing, Nothing) -> put (env, Nothing) >> emptyStmt
--     _ -> failStmt token `debug` (show et ++ " if ala") -- TODO bug, dlaczego a ma value 20
--
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
-- typeOfStmt (Ret expr) = do
--   exprtype <- typeOfExpr expr --`debug` show expr -- TODO
--   case exprtype of
--     Right t -> do
--       modify (\(env, _) -> (env, Just t))
--       emptyStmt
--     Left err -> return $ Left err
--
-- typeOfStmt VRet = modify (\(env, _) -> (env, Just Void)) >> emptyStmt
--
-- typeOfStmt (SExp expr) = do
--   _ <- typeOfExpr expr
--   emptyStmt
--

--
-- isIdentInt :: Stmt -> Name -> TypeChecker
-- isIdentInt token name = do
--   (env, _) <- get
--   case Map.lookup name env of
--     Nothing -> failStmt token
--     Just t -> if t == Int then emptyStmt else failStmt token
--
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
--
-- typeOfExpr token@(EVar (Ident name)) = do
--   (env, _) <- get
--   return $ maybeToEither (show token) (Map.lookup name env)
--
typeOfExpr (ELitInt _) = return Int
--
typeOfExpr ELitTrue = return Bool
--
typeOfExpr ELitFalse = return Bool
--
-- typeOfExpr token@(EApp (Ident name) exprs) = do
--   (env, _) <- get
--   case Map.lookup name env of
--     Nothing -> return $ lookupInbuilds name
--     -- Nothing -> return $ Left $ "Function " ++ name ++ " undefined " ++ show token
--     Just (Fun rettype argtypes) -> do
--       either_exprtypes <- mapM typeOfExpr exprs
--       let check = foldM checkArgType True (zip either_exprtypes argtypes)
--       case check of
--         Left err -> return $ Left err
--         Right True -> return $ Right rettype
--         Right False -> return $ Left $ show token
--     Just _ -> return $ Left $ "Variable " ++ name ++ " is not a function " ++ show token
--     where
--       checkArgType :: Bool -> (Either String Type, Type) -> Either String Bool
--       checkArgType acc (et, argt) = fmap (\t -> acc && t == argt) et
--       lookupInbuilds :: Name -> Either String Type
--       lookupInbuilds "print" = Right Void
--       -- TODO jeśli nie ma w inbuilds funkcji to zwrócić błąd
--       -- lookupInbuilds _ = Left $ "Function " ++ name ++ " undefined " ++ show token
--
typeOfExpr (EString _) = return Str
--
-- typeOfExpr token@(Neg expr) = do
--   t <- typeOfExpr expr
--   case t of
--     Right Int -> return $ Right Int
--     _ -> return $ Left $ show token
--
-- typeOfExpr token@(Not expr) = do
--   t <- typeOfExpr expr
--   case t of
--     Right Bool -> return $ Right Bool
--     _ -> return $ Left $ show token
--
-- typeOfExpr token@(EMul expr1 _ expr2) = typeOfAddMul token expr1 expr2
--
-- typeOfExpr token@(EAdd expr1 _ expr2) = typeOfAddMul token expr1 expr2
--
-- typeOfExpr token@(ERel expr1 _ expr2) = do
--   t1 <- typeOfExpr expr1
--   t2 <- typeOfExpr expr2
--   let beither = liftM2 (==) t1 t2
--   case beither of
--     Right True -> return $ Right Bool
--     Right False -> return $ Left $ show token
--     Left err -> return $ Left err
--
-- typeOfExpr token@(EAnd expr1 expr2) = typeOfAndOr token expr1 expr2
--
-- typeOfExpr token@(EOr expr1 expr2) = typeOfAndOr token expr1 expr2
--
-- typeOfAddMul :: Expr -> Expr -> Expr -> TypeCheckerT Type
-- typeOfAddMul token expr1 expr2 = do
--   t1 <- typeOfExpr expr1
--   t2 <- typeOfExpr expr2
--   let beither = liftM2 (\t1' t2' -> t1' == Int && t2' == Int) t1 t2
--   case beither of
--     Right True -> return $ Right Int
--     Right False -> return $ Left $ show token
--     Left err -> return $ Left err
--
-- typeOfAndOr :: Expr -> Expr -> Expr -> TypeCheckerT Type
-- typeOfAndOr token expr1 expr2 = do
--   t1 <- typeOfExpr expr1
--   t2 <- typeOfExpr expr2
--   let beither = liftM2 (\t1' t2' -> t1' == Bool && t2' == Bool) t1 t2
--   case beither of
--     Right True -> return $ Right Bool
--     Right False -> return $ Left $ show token
--     Left err -> return $ Left err
