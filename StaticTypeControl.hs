module StaticTypeControl where


import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Either
import Control.Arrow
import qualified Data.Map as Map

import AbsMacchiato


type Name = String
type TypeEnv = Map.Map Name Type
type TypeScope = (TypeEnv, Maybe Type)
type TypeCheckerT a = State TypeScope (Either String a)
type TypeChecker = TypeCheckerT ()

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty -- Ręczna obsługa funkcji wbudowanych

emptyTypeScope :: TypeScope
emptyTypeScope = (emptyTypeEnv, Nothing)

emptyStmt :: TypeChecker
emptyStmt = return $ Right ()

fromRight :: Either e a -> a
fromRight (Right a) = a

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

ok :: TypeChecker
ok = return $ Right ()

typeControl :: Program -> Either String ()
typeControl program = evalState (checkProgram program) emptyTypeScope

checkProgram :: Program -> TypeChecker
checkProgram (Program topdefs) = do
  checkedTopdefs <- mapM checkTopDef topdefs
  return $ sequence_ checkedTopdefs

checkTopDef :: TopDef -> TypeChecker
checkTopDef token@(FnDef t (Ident name) args block) = do
  (env, ret) <- get
  let params = map (\(Arg argtype (Ident argname)) -> (argtype, argname)) args
      env' = Map.insert name (Fun t (map fst params)) env
      env'' = foldl (\e (argtype, argname) -> Map.insert argname argtype e) env' params
  put (env'', ret)
  _ <- checkBlock block
  (_, ret') <- get
  put (env', ret')
  let ret'' = if isNothing ret' then Just Void else ret'
  let bmaybe = fmap (==t) ret''
  case bmaybe of
    Just True -> emptyStmt
    _ -> return $ Left $ show token

checkBlock :: Block -> TypeChecker
checkBlock token@(Block stmts) = do
  scopes <- mapM (\stmt -> modify (\(env, _) -> (env, Nothing)) >> typeOfStmt stmt >> get) stmts
  let return_types = mapMaybe snd scopes
  case length (nub return_types) of
    0 -> emptyStmt
    1 -> do
      modify (\(env, _) -> (env, Just $ head return_types))
      emptyStmt
    _ -> return $ Left $ show token

-- Statements --
typeOfStmt :: Stmt -> TypeChecker

typeOfStmt Empty = emptyStmt

typeOfStmt (BStmt block) = do
  (env, _) <- get
  _ <- checkBlock block
  modify (\(_, ret) -> (env, ret))
  emptyStmt

--spr ret, przywróć stan
-- dodać fun type do env
--fun type nie musi znać nazw argumentów
-- nadpisać env
typeOfStmt token@(FunLoc t (Ident name) args stmt) = do
  (env, ret) <- get
  res <- typeOfStmt stmt
  case res of
    Left err -> return $ Left err
    _ -> do
    (_, ret') <- get
    let argtypes = map (\(Arg argtype _) -> argtype) args
        env' = Map.insert name (Fun t argtypes) env
        b = case ret' of
          Nothing -> t == Void
          Just rt -> rt == t
    if b then put (env', ret) >> emptyStmt else return $ Left $ show token


typeOfStmt (Decl _ []) = emptyStmt
typeOfStmt (Decl t (item:items)) = checkDecl item >> typeOfStmt (Decl t items)
  where
    checkDecl :: Item -> TypeChecker
    checkDecl (NoInit (Ident name)) = modify (first (Map.insert name t)) >> emptyStmt
    checkDecl token@(Init (Ident name) expr) = do
      exprtype <- typeOfExpr expr
      let beither = fmap (==t) exprtype
      case beither of
        Right True -> checkDecl (NoInit (Ident name))
        Right False -> return $ Left $ show token
        Left err -> return $ Left err

typeOfStmt (Ret expr) = do
  exprtype <- typeOfExpr expr
  case exprtype of
    Right t -> do
      modify (\(env, _) -> (env, Just t))
      emptyStmt
    Left err -> return $ Left err

typeOfStmt VRet = modify (\(env, _) -> (env, Just Void)) >> emptyStmt

typeOfStmt (SExp expr) = do
  _ <- typeOfExpr expr
  emptyStmt

-- Expressions --
typeOfExpr :: Expr -> TypeCheckerT Type

typeOfExpr token@(EVar (Ident name)) = do
  (env, _) <- get
  return $ maybeToEither (show token) (Map.lookup name env)

typeOfExpr (ELitInt _) = return $ Right Int

typeOfExpr ELitTrue = return $ Right Bool

typeOfExpr ELitFalse = return $ Right Bool

typeOfExpr token@(EApp (Ident name) exprs) = do
  (env, _) <- get
  case Map.lookup name env of
    Nothing -> return $ lookupInbuilds name
    -- Nothing -> return $ Left $ "Function " ++ name ++ " undefined " ++ show token
    Just (Fun rettype argtypes) -> do
      either_exprtypes <- mapM typeOfExpr exprs
      let check = foldM checkArgType True (zip either_exprtypes argtypes)
      case check of
        Left err -> return $ Left err
        Right True -> return $ Right rettype
        Right False -> return $ Left $ show token
    Just _ -> return $ Left $ "Variable " ++ name ++ " is not a function " ++ show token
    where
      checkArgType :: Bool -> (Either String Type, Type) -> Either String Bool
      checkArgType acc (et, argt) = fmap (\t -> acc && t == argt) et
      lookupInbuilds :: Name -> Either String Type
      lookupInbuilds "print" = Right Void
      -- lookupInbuilds _ = Left $ "Function " ++ name ++ " undefined " ++ show token

typeOfExpr (EString _) = return $ Right Str

typeOfExpr token@(Neg expr) = do
  t <- typeOfExpr expr
  case t of
    Right Int -> return $ Right Int
    _ -> return $ Left $ show token

typeOfExpr token@(Not expr) = do
  t <- typeOfExpr expr
  case t of
    Right Bool -> return $ Right Bool
    _ -> return $ Left $ show token

typeOfExpr token@(EMul expr1 _ expr2) = typeOfAddMul token expr1 expr2

typeOfExpr token@(EAdd expr1 _ expr2) = typeOfAddMul token expr1 expr2

typeOfExpr token@(ERel expr1 _ expr2) = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  let beither = liftM2 (==) t1 t2
  case beither of
    Right True -> case fromRight t1 of
      Int -> return t1
      Bool -> return t1
      Str -> return t1
      Tup _ -> return t1
      _ -> return $ Left $ show token
    Right False -> return $ Left $ show token
    Left err -> return $ Left err

typeOfExpr token@(EAnd expr1 expr2) = typeOfAndOr token expr1 expr2

typeOfExpr token@(EOr expr1 expr2) = typeOfAndOr token expr1 expr2

typeOfAddMul :: Expr -> Expr -> Expr -> TypeCheckerT Type
typeOfAddMul token expr1 expr2 = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  let beither = liftM2 (\t1' t2' -> t1' == Int && t2' == Int) t1 t2
  case beither of
    Right True -> return $ Right Int
    Right False -> return $ Left $ show token
    Left err -> return $ Left err

typeOfAndOr :: Expr -> Expr -> Expr -> TypeCheckerT Type
typeOfAndOr token expr1 expr2 = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  let beither = liftM2 (\t1' t2' -> t1' == Bool && t2' == Bool) t1 t2
  case beither of
    Right True -> return $ Right Bool
    Right False -> return $ Left $ show token
    Left err -> return $ Left err
