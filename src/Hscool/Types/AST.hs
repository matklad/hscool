module Hscool.Types.AST where

import Text.Printf (printf)


type Symbol = String

joinAndIndent :: (Show a) => [a] -> String
joinAndIndent = unlines . map indent

indent :: (Show a) => a -> String
indent = init . unlines . map ("  " ++) . lines . show

data Program = Program [Class]

instance Show Program where
  show (Program cls) = "#1\n_program\n" ++ joinAndIndent cls


data Class = Class Symbol Symbol [Feature]

instance Show Class where
  show (Class name super features) =
    printf format name super (joinAndIndent features)
    where
      format = "#1\n_class\n  %s\n  %s\n  \"file name here\"\n  (\n%s  )"


data Feature = Method Symbol [Formal] Symbol Expression

instance Show Feature where
  show (Method name formals type_ body) =
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

instance Show Expression where
  show = (++ "\n: _no_type") . aux
    where
      aux expr = case expr of
        Assign s e -> printf "#1\n_assign\n  %s\n%s" s (indent e)
        IntConst s -> printf "#1\n_int\n  %s" s

data Case = Case
          deriving Show
