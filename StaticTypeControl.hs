module StaticTypeControl where


import Control.Monad.State
import Data.List
import Data.Maybe
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

typeOfExpr token@(EAdd expr1 _ expr2) = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  let beither = liftM2 (\t1' t2' -> t1' == Int && t2' == Int) t1 t2
  case beither of
    Right True -> return $ Right Int
    Right False -> return $ Left $ show token
    Left err -> return $ Left err
