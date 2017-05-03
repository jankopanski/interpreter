module StaticTypeControl where


import Control.Monad.State
import qualified Data.Map as Map

import AbsMacchiato


type TypeName = String
type TypeEnv = Map.Map TypeName Type
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
checkTopDef (FnDef t (Ident name) args block) = do
  -- modify (\(env, _) -> (env, Nothing))
  checkBlock block
  (_, ret) <- get
  ok
  -- if t == ret then return t else Nothing
-- type Token = Program | TopDef | Block
-- newtype Token a = Token a

checkBlock :: Block -> TypeChecker
checkBlock (Block stmts) = do
  ok

-- typeOf :: Token Program -> Either String Type
-- typeOf (Token (Program topdefs)) = Left "ala"

emptyStmt :: TypeChecker
emptyStmt = return $ Right ()

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
