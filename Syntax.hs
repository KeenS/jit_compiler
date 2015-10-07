module Syntax where

type Name = String

data Expr
  = Float Double
  | UnOp Name Expr
  | BinOp Name Expr Expr
  | UnDef Name [Name] Expr
  | BinDef Name [Name] Expr
  | Bind Name Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)
