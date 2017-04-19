module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexMacchiato
import ParMacchiato
import SkelMacchiato
import PrintMacchiato
import AbsMacchiato

import ErrM

-- import Data.Typeable
import Interpreter

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s = let ts = myLexer s in case pProgram ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn "Tokens:"
                          putStrLn $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree tree
                          interpret tree
                          exitSuccess


showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  (file)         Parse content of file."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run
    [f] -> runFile f
