module Hscool.Types.AST where

type Symbol = String

data Program = Program [Class]

data Class = Class Symbol Symbol [Feature]

data Feature = Method Symbol [Formal] Symbol Expression

data Formal = Formal Symbol Symbol

data Expression =
    Assign Symbol Expression
  | Dispatch Expression Symbol Symbol [Expression]
  | Cond Expression Expression Expression
  | Loop Expression Expression
  | TypeCase Expression [Case]
  | Block [Expression]
  | Let Symbol Symbol Expression Expression
  | Expression :+ Expression
  | Expression :- Expression
  | Expression :* Expression
  | Expression :/ Expression
  | Neg Expression
  | Expression :< Expression
  | Expression := Expression
  | Expression :<= Expression
  | Comp Expression
  | IntConst Symbol
  | StringConst Symbol
  | New Symbol
  | IsVoid Expression
  | NoExpr
  | Object Symbol

data Case = Case
