{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT
import qualified Syntax as S

one = cons $ C.Float (F.Double 1.0)
zero =cons $ C.Float (F.Double 0.0)
false = zero
true = one


toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) =
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM_ args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret
  

codegenTop (S.Extern name args) =
  external double name fnargs
  where fnargs = toSig args

codegenTop (S.BinDef name args body) =
  codegenTop $ S.Function ("binary" ++ name) args body

codegenTop exp =
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret


binops = Map.fromList [
  ("+", fadd)
  , ("-", fsub)
  , ("*", fmul)
  , ("/", fdiv)
  , ("<", lt)
  ]

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test



cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)

cgen (S.Var x) = getvar x >>= load

cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

cgen (S.UnOp op a) =
  cgen $ S.Call ("unary" ++ op) [a]

cgen (S.BinOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval


cgen (S.BinOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (S.Call ("binary" ++ op) [a, b])

cgen (S.Bind var val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval

cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse
  -- if.then
  setBlock ifthen
  trval <- cgen tr
  br ifexit
  ifthen <- getBlock
  -- if.else
  setBlock ifelse
  flval <- cgen fl
  br ifexit
  ifelse <- getBlock
  -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

cgen (S.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"
  -- %entry
  i <- alloca double
  istart <- cgen start
  stepval <- cgen step
  store i istart
  assign ivar i
  br forloop
  -- for.loop
  setBlock forloop
  cgen body
  ival <- load i
  inext <- fadd ival stepval
  store i inext
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test forloop forexit
  setBlock forexit
  return zero

cgen (S.Let a b c) = do
  i <- alloca double
  val <- cgen b
  store i val
  assign a i
  cgen c

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return


codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    return newast
   where
     modn = mapM codegenTop fns
     newast = runLLVM mod modn


