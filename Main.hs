module Main where

import System.IO ( getContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexMacchiato
import ParMacchiato
import SkelMacchiato
import PrintMacchiato
import AbsMacchiato
import ErrM

import Interpreter

runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

run :: String -> IO ()
run s = let ts = myLexer s in case pProgram ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrLn "Tokens:"
                          print ts
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

argsError :: IO ()
argsError = do
  putStrLn "Invalid arguments"
  s <- getProgName
  putStrLn ("Type \"./" ++ s ++ " --help\"")
  exitFailure

usage :: IO ()
usage = putStr $ unlines
  [ "usage: Call with one of the following argument combinations:"
  , "  --help          Display this help message."
  , "  (no arguments)  Parse stdin."
  , "  (file)         Parse content of file."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run
    [f] -> runFile f
    _ -> argsError
