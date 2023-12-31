-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- | Program to test parser.

module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Grammar.Abs   ( Program )
import Grammar.Lex   ( Token, mkPosToken )
import Grammar.Par   ( pProgram, myLexer )
import Grammar.Print ( Print, printTree )
import Grammar.Skel  ()

import Interpreter ( interpret )
import TypeChecker ( typeCheck )
import System.IO (hPutStrLn, stderr)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> String -> IO ()
runFile v p f arg = readFile f >>= run v p arg

run :: Verbosity -> ParseFun Program -> String -> String -> IO ()
run v parsefun arg s =
  let tokens = myLexer s in
  let showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ] in
  case parsefun tokens of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      typeCheck tree
      interpret tree arg

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getContents >>= run 2 pProgram ""
    [file] -> runFile 2 pProgram file ""
    file:(arg:_) -> runFile 2 pProgram file arg