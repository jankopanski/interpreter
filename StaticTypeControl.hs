module StaticTypeControl where


import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import AbsMacchiato


type Name = String
type TypeEnv = Map.Map Name Type
type TypeScope = (TypeEnv, Maybe Type)

-- type TypeChecker = Reader TypeEnv Type
-- type TypeCheckerT a = Reader TypeEnv (Either String a)
-- type TypeChecker = TypeCheckerT ()
type TypeCheckerT a = State TypeScope (Either String a)
type TypeChecker = TypeCheckerT ()

emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty -- Ręczna obsługa funkcji wbudowanych

emptyTypeScope :: TypeScope
emptyTypeScope = (emptyTypeEnv, Nothing)

ok :: TypeChecker
ok = return $ Right ()

typeControl :: Program -> Either String ()
typeControl program = evalState (checkProgram program) emptyTypeScope
-- typeControl program = runReader (checkProgram program) emptyTypeEnv
-- typeControl p = Right ()

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
  let bmaybe = fmap (==t) ret'
  case bmaybe of
    Just True -> emptyStmt
    _ -> return $ Left $ show token

checkBlock :: Block -> TypeChecker
-- checkBlock (Block []) = emptyStmt
checkBlock token@(Block stmts) = do
  scopes <- mapM (\stmt -> modify (\(env, _) -> (env, Nothing)) >> typeOfStmt stmt >> get) stmts
  let return_types = mapMaybe snd scopes
  case length (nub return_types) of
    0 -> emptyStmt
    1 -> do
      modify (\(env, _) -> (env, Just $ head return_types))
      emptyStmt
    _ -> return $ Left $ show token

  -- if null return_types
  --   then return $ Right ()
  --   else if length (nub return_types) > 1
  --     then return $ Left $ show token
  --     else modify (\(env, _) -> (env, head return_types)) >>= return $ Right ()
    -- else if length $ nub return_types > 1
          -- then return $ Left token
          -- else modify (\(env, _) -> (env, head return_types) >> return $ Right ()

-- checkBlock (Block stmts) = do
  -- scope <- get
  -- foldM checkStmt scope stmts
  --   where
  --     checkStmt :: TypeScope -> Stmt -> TypeChecker
  --     checkStmt modify (\(env, _) -> (env, Nothing))
  --     typeOfStmt stmt

-- typeOf :: Token Program -> Either String Type
-- typeOf (Token (Program topdefs)) = Left "ala"

emptyStmt :: TypeChecker
emptyStmt = return $ Right ()

-- Statement block - przywraca env typów

typeOfStmt :: Stmt -> TypeChecker
typeOfStmt Empty = emptyStmt

typeOfStmt (SExp expr) = do
  _ <- typeOfExpr expr
  emptyStmt

typeOfExpr :: Expr -> TypeCheckerT Type
typeOfExpr token@(EAdd expr1 _ expr2) = do
  t1 <- typeOfExpr expr1
  t2 <- typeOfExpr expr2
  let beither = liftM2 (\t1' t2' -> t1' == Int && t2' == Int) t1 t2
  case beither of
    Right True -> return $ Right Int
    Right False -> return $ Left $ show token
    Left err -> return $ Left err
  -- when True $ error "ala"
  -- return $ Right Int

typeOfExpr _ = return $ Right Int
