module Hscool.CGen.Intermediate where

data Program = Program [Class] [Method]

data Class = Class String String Int [String]

data Method = Method String Int Expr

data Expr =
    Assign Id Expr
  | Dispatch Expr Id [Expr]
  | StaticDispatch Expr Id String [Expr]
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
  | IntConst String
  | StringConst String
  | BoolConst Bool
  | New String
  | IsVoid Expr
  | Object Id

data Id = A Int | P Int | L Int

data Branch = String Expr
