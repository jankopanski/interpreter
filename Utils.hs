module Utils where


import Control.Monad.State
import qualified Data.Map as Map

import DataStructures


getFunc :: FName -> Scope -> Func
getFunc name (Scope _ _ infenv outfenv _ _) =
  case Map.lookup name infenv of
    Just func -> func
    Nothing -> case Map.lookup name outfenv of
      Just func -> func
      Nothing -> error $ "Undefined Function: " ++ name

modifyStore :: (Store -> Store) -> Interpreter
modifyStore f = modify (\(Scope inenv outenv infenv outfenv store ret) ->
  (Scope inenv outenv infenv outfenv (f store) ret))


insertStore :: Value -> Store -> Store
insertStore val (s, n) = let n' = n + 1 in (Map.insert n' val s, n')

updateStore :: Loc -> Value -> Store -> Store
updateStore loc val (s, l) = (Map.insert loc val s, l)

getValueByLoc :: Loc -> Store -> Value
getValueByLoc loc (s, _) = s Map.! loc

getValueByName :: Name -> Scope -> Value
getValueByName name (Scope inenv outenv _ _ store _)
  | Map.member name inenv = getValueByLoc (inenv Map.! name) store
  | Map.member name outenv = getValueByLoc (outenv Map.! name) store
  | otherwise = error $ "Undefined variable: " ++ name
