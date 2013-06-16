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


data Class = Class Symbol Symbol [Feature] String

instance Show Class where
  show (Class name super features fileName) =
    printf format name super fileName (joinAndIndent features)
    where
      format = "#1\n_class\n  %s\n  %s\n  %s\n  (\n%s  )"


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
  | Dispatch Expression Symbol [Expression]
  | StaticDispatch Expression Symbol Symbol [Expression]
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
        Dispatch e s actuals -> printf "#1\n_dispatch\n%s\n  %s\n  (\n%s\n  )"
                                (indent e) s (joinAndIndent actuals)
        StaticDispatch e type_ name actuals ->
          printf "#1\n_static_dispatch\n%s\n  %s\n  \n%s\n  (\n%s\n  )"
          (indent e) type_ name (joinAndIndent actuals)
        Cond e1 e2 e3 -> printf "#1\n_cond\n%s\n%s\n%s"
                         (indent e1) (indent e2) (indent e3)
        Loop e1 e2 -> printf "#1\n_loop\n%s\n%s"
                      (indent e1) (indent e2)
        Block es -> printf "#1\n_block\n%s" (joinAndIndent es)
        e1 :+ e2 -> printf "#1\n_plus\n%s\n%s" (indent e1) (indent e2)
        e1 :- e2 -> printf "#1\n_minus\n%s\n%s" (indent e1) (indent e2)
        e1 :* e2 -> printf "#1\n_mul\n%s\n%s" (indent e1) (indent e2)
        e1 :/ e2 -> printf "#1\n_divide\n%s\n%s" (indent e1) (indent e2)
        Neg e -> printf "#1\n_neg\n%s" (indent e)
        e1 :< e2 -> printf "#1\n_lt\n%s\n%s" (indent e1) (indent e2)
        e1 := e2 -> printf "#1\n_eq\n%s\n%s" (indent e1) (indent e2)
        e1 :<= e2 -> printf "#1\n_leq\n%s\n%s" (indent e1) (indent e2)
        Comp e -> printf "#1\n_comp\n%s" (indent e)
        IntConst s -> printf "#1\n_int\n  %s" s
        StringConst s -> printf "#1\n_string\n  %s" s
        New s -> printf "#1\n_new\n  %s" s
        IsVoid e -> printf "#1\n_isvoid\n%s" (indent e)
        Object s -> printf "#1\n_object\n  %s" s
        _ -> error "can't show this expression"

data Case = Case
          deriving Show
