{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-} -- debugging
{-# LANGUAGE LambdaCase #-}

module Interpreter (interpret) where

import Prelude hiding (log)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.RWS
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError, catchError))
import Control.Monad.Fail (MonadFail)

import Grammar.Par
import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

import qualified Data.Foldable
import Data.Typeable
import qualified Data.Sequence
import Control.Exception (Exception, throw)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Foldable (foldl')
import System.Environment (getArgs)
import Text.Read (readMaybe)

deriving instance Typeable Expr

type Exc = Exc' BNFC'Position
data Exc' a = 
  UnknownIdentifier !a !Ident 
  | UnknownLocation !a !Ident 
  | Return !a !Val 
  | NotImplemented !a
  | ParseError !a -- for errors on atoi() built-in function call
  | DivisionByZero !a
  deriving (Typeable)

instance {-# OVERLAPPING #-} Show BNFC'Position where
  show BNFC'NoPosition = ""
  show (BNFC'Position l c) = "(Line " ++ show l ++ ", column " ++ show c ++ ")"
  show _ = ""

instance Show Exc where
  show (UnknownIdentifier pos ident) = "Unknown identifier " ++ show ident ++ " at " ++ show pos
  show (UnknownLocation pos ident) = "Unknown location " ++ show ident ++ " at " ++ show pos
  show (Return pos val) = "Return " ++ show val ++ " " ++ show pos
  show (NotImplemented pos) = "Not implemented feature used at " ++ show pos
  show (ParseError pos) = "Cannot parse int at " ++ show pos
  show (DivisionByZero pos) = "Division by zero at " ++ show pos

instance Exception Exc

type Loc = Int
type Env = Map Ident Loc
data Val = ValVoid
  | ValInt !Integer
  | ValBool !Bool
  | ValStr !String
  | ValFun !Env ![ArgC] !BlockC
  deriving Show
type Store = Map Loc Val
type Log = Data.Sequence.Seq String

type IM a = ExceptT Exc (RWST Env Log Store Identity) a

alloc :: Store -> Loc
alloc store = case Map.maxViewWithKey store of
  Nothing -> 0
  Just ((loc, _), _) -> loc + 1

interpret :: Program -> String -> IO ()
interpret p arg = do
  let (excOrVal, store, output) = runIdentity $ runRWST (runExceptT (evalWithArgs p arg)) Map.empty Map.empty
  putStrLn $ concat $ Data.Foldable.toList output
  case excOrVal of
    Left err -> print err
    Right _ -> pure ()

evalMaybe :: Exc -> Maybe a -> IM a
evalMaybe e Nothing = throwError e
evalMaybe _ (Just a) = pure a

class Evaluable a where
  eval :: a -> IM Val

declareVar :: Env -> Item -> IM Env
declareVar env (Init _ (LIdent _ ident) expr) = do
  store <- get
  local (const env) $ do
    e <- eval expr
    let l = alloc store
    let env' = Map.insert ident l env
    let store' = Map.insert l e store
    put store'
    pure env'
declareVar env (NoInit _ (LIdent _ ident)) = do
  store <- get
  let l = alloc store
  let env' = Map.insert ident l env
  let store' = Map.insert l ValVoid store
  put store'
  pure env'

bindArgs :: [ArgC] -> [Expr] -> IM Env
bindArgs [] [] = ask
bindArgs (Arg _ _ ident : args) (expr : exprs) = do
  env <- ask
  val <- eval expr
  store <- get
  let l = alloc store
  let env' = Map.insert ident l env
  let store' = Map.insert l val store
  put store'
  local (const env') $ bindArgs args exprs
bindArgs (ArgRef _ _ ident : args) (ELVal _ lval@(LIdent _ ident_orig) : vals) = do
  env <- ask
  l <- getLoc lval
  let env' = Map.insert ident l env
  local (const env') $ bindArgs args vals
bindArgs a b = notImplemented ("bindArgs", a, b)

getLoc :: LValue -> IM Loc
getLoc (LIdent pos ident) = do
  env <- ask
  store <- get
  evalMaybe (UnknownIdentifier pos ident) (Map.lookup ident env)

log :: String -> IM ()
log s = tell $ Data.Sequence.singleton s

notImplemented :: (Show a) => a -> IM b
notImplemented x = do
  log "Not implemented: "
  log $ show x
  throwError $ NotImplemented BNFC'NoPosition

evalWithArgs :: Program -> String -> IM Val
evalWithArgs (Prog pos stmts) arg = do
  let param = Arg BNFC'NoPosition (Str BNFC'NoPosition) (Ident "arg")
  let arg' = EString BNFC'NoPosition arg
  env' <- bindArgs [param] [arg']
  local (const env') $ do
    eval $ Block pos stmts
    pure ValVoid

instance MonadFail Identity where
  fail s = error "Type error!"

instance Evaluable LValue where
  eval lval@(LIdent pos ident) = do
    env <- ask
    store <- get
    l <- getLoc lval
    evalMaybe (UnknownLocation pos ident) (Map.lookup l store)

instance Evaluable Expr where
  eval (ELitInt _ n) = pure (ValInt n)
  eval (ELitTrue _) = pure (ValBool True)
  eval (ELitFalse _) = pure (ValBool False)
  eval (EApp _ (Ident "print") [expr]) = do
    (ValStr s) <- eval expr
    log s
    pure ValVoid
  eval (EApp pos (Ident "printInt") [expr]) = do
    (ValInt n) <- eval expr
    eval (EApp pos (Ident "print") [EString BNFC'NoPosition $ show n])
  eval (EApp pos (Ident "atoi") [ELVal _ lval]) = do
    (ValStr str) <- eval lval
    n <- evalMaybe (ParseError pos) (readMaybe str :: Maybe Integer)
    pure $ ValInt n

  eval (EApp pos ident exprs) = do
    env <- ask
    store <- get
    l <- evalMaybe (UnknownIdentifier pos ident) (Map.lookup ident env)
    (ValFun funEnv params block) <- evalMaybe (UnknownLocation pos ident) (Map.lookup l store)
    env' <- bindArgs params exprs
    local (const funEnv) $ do
      local (const env') $ do
        do { eval block; return ValVoid; } `catchError` handler
        where handler = \case
                Return pos val -> pure val
                e -> throwError e
  eval (EString _ str) = pure (ValStr str)
  eval (ELVal _ lval) = eval lval
  eval (ERel _ lhs (EQU _) rhs) = do
    lhs' <- eval lhs
    rhs' <- eval rhs
    case lhs' of
      ValInt lhs'' -> do
        ValInt rhs'' <- eval rhs
        pure $ ValBool (lhs'' == rhs'')
      ValStr lhs'' -> do
        ValStr rhs'' <- eval rhs
        pure $ ValBool (lhs'' == rhs'')
      -- no need to implement that: the type checker should catch it
      _ -> notImplemented ("rel", lhs', rhs')
  eval (ERel _ lhs (LE _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    pure $ ValBool (lhs' <= rhs')
  eval (ERel _ lhs (GTH _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    pure $ ValBool (lhs' > rhs')
  eval (EAdd _ lhs (Plus _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    pure $ ValInt (lhs' + rhs')
  eval (EAdd _ lhs (Minus _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    pure $ ValInt (lhs' - rhs')
  eval (EMul pos lhs (Div _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    if rhs' == 0 then
      throwError $ DivisionByZero pos
    else
      pure $ ValInt (lhs' `div` rhs')
  eval (EMul pos lhs (Times _) rhs) = do
    ValInt lhs' <- eval lhs
    ValInt rhs' <- eval rhs
    pure $ ValInt (lhs' * rhs')
  eval e = notImplemented ("eval", e)

evalStmt :: Stmt -> IM Env
evalStmt (Empty _) = ask
evalStmt (Ret pos expr) = do
  val <- eval expr
  throwError $ Return pos val
evalStmt (SExp _ expr) = do
  eval expr
  ask
evalStmt (Decl _ _ item) = do
  env <- ask
  declareVar env item
evalStmt (DeclFun _ _ ident params block) = do
  env <- ask 
  store <- get
  let l = alloc store
  let env' = Map.insert ident l env
  -- env' <- bindArgs params exprs
  let store' = Map.insert l (ValFun env' params block) store
  put store'
  pure env'
evalStmt (Cond _ expr block1) = do
  ValBool b <- eval expr
  if b then do { eval block1; ask } else ask
evalStmt (CondElse _ expr block1 block2) = do
  ValBool b <- eval expr
  if b then eval block1 else eval block2
  ask
evalStmt (Ass _ lval expr) = do
  e <- eval expr
  l <- getLoc lval
  store <- get
  let store' = Map.insert l e store
  put store'
  ask
evalStmt stmt@(While _ expr block) = do
  ValBool b <- eval expr
  if not b then ask else do {
    eval block;
    evalStmt stmt;
    -- będę implementować break i continue używając specjalnych wyjątków
  } -- `catchError` handler
  ask
  -- where
  --   handler e@(Return _ val) = throwError e
  --   handler 

evalStmt e = notImplemented ("stmt", e)
  
instance Evaluable BlockC where
  eval (Block _ stmts) = do
    env <- ask
    store <- get
    foldM_ (\env stmt -> local (const env) (evalStmt stmt)) env stmts
    return ValVoid
