module Utils where


import Control.Monad.State
import qualified Data.Map as Map

import DataStructures

-- getVar :: Name -> Scope -> Value
-- getVar name (Scope inenv outenv _ _ _) = case Map.lookup name inenv of
--   Just var -> var
--   Nothing -> case Map.lookup name outenv of
--     Just var -> var
--     Nothing -> error "Variable not defined"

getFunc :: FName -> Scope -> Func
getFunc name (Scope _ _ infenv outfenv _ _) = case Map.lookup name infenv of
  Just func -> func
  Nothing -> case Map.lookup name outfenv of
    Just func -> func
    Nothing -> error "Function not defined"

modifyStore :: (Store -> Store) -> Interpreter
modifyStore f = modify (\(Scope inenv outenv infenv outfenv store ret) ->
  (Scope inenv outenv infenv outfenv (f store) ret))

-- insertStore :: Store -> (Store, Loc)
-- insertStore (s, n) = let n' = n + 1 in ((s, n'), n')

insertStore :: Value -> Store -> Store
insertStore val (s, n) = let n' = n + 1 in (Map.insert n' val s, n')

updateStore :: Loc -> Value -> Store -> Store
updateStore loc val (s, l) = (Map.insert loc val s, l)

getValue :: Loc -> Store -> Value
getValue loc (s, _) = s Map.! loc
