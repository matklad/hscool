module Hscool.CGen.Intermediate where

data Program = Program [Class] [Method] [String] [String]
    deriving Show

data Class = Class Int String String Int [String]
    deriving Show

data Method = Method String Int Int Expr
    deriving Show

data Expr =
    Assign Id Expr
  | Dispatch Expr Int [Expr]
  | StaticDispatch Expr (String, Int) [Expr]
  | Cond Expr Expr Expr
  | Loop Expr Expr
  | TypeCase Expr [Branch]
  | Block [Expr]
  | Add Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Le Expr Expr
  | Eq Expr Expr
  | Leq Expr Expr
  | Comp Expr
  | New String
  | IsVoid Expr
  | Object Id
  deriving Show

data Id = A Int | P Int | L Int | C String | S
    deriving Show

data Branch = String Expr
    deriving Show
