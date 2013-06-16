module Hscool.Types.AST where

import Text.Printf (printf)


class Syntax a where
  pretyPrint :: a -> String

type Symbol = String

joinAndIndent :: (Syntax a) => [a] -> String
joinAndIndent = unlines . map indent

indent :: (Syntax a) => a -> String
indent = init . unlines . map ("  " ++) . lines . pretyPrint

data Program = Program [Class]
             deriving Show

instance Syntax Program where
  pretyPrint (Program cls) = "#1\n_program\n" ++ joinAndIndent cls

data Class = Class Symbol Symbol [Feature]
           deriving Show

instance Syntax Class where
  pretyPrint (Class name super features) =
    printf format name super (joinAndIndent features)
    where
      format = "#1\n_class\n  %s\n  %s\n  \"file name here\"\n  (\n%s  )"

data Feature = Method Symbol [Formal] Symbol Expression
             deriving Show

instance Syntax Feature where
  pretyPrint (Method name formals type_ body) =
    printf format name type_ (indent body)
    where
      format = "#1\n_method\n  %s\n  %s\n%s"


data Formal = Formal Symbol Symbol
            deriving Show

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
  deriving Show

instance Syntax Expression where
  pretyPrint expr = case expr of
    IntConst s -> printf "#1\n_int\n  %s\n: _no_type" s

data Case = Case
          deriving Show
