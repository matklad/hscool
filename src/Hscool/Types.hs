module Hscool.Types
    ( Token(..)
    ) where

data Token =
      Assign
    | At
    | BoolConst String
    | Case
    | Class
    | Colon
    | Coma
    | Darrow
    | Div
    | Dot
    | Else
    | Eof
    | Eq
    | Error
    | Esac
    | Fi
    | If
    | In
    | Inherits
    | IntConst String
    | IsVoid
    | Lbrace
    | Le
    | Let
    | LetStmt
    | Loop
    | Lparen
    | Lt
    | Minus
    | Mult
    | Neg
    | New
    | Not
    | ObjectId String
    | Of
    | Plus
    | Pool
    | Rbrace
    | Rparen
    | Semi
    | StrConst String
    | Then
    | TypeId String
    | While
    deriving Eq

instance Show Token where
  show t = case t of
    Assign -> "ASSIGN"
    At -> "'@'"
    BoolConst s -> "BOOL_CONST " ++ s
    Case -> "CASE"
    Class -> "CLASS"
    Colon -> "':'"
    Coma -> "','"
    Darrow -> "DARROW"
    Div -> "'/'"
    Dot -> "'.'"
    Else -> "ELSE"
    Eof -> "EOF"
    Eq -> "'='"
    Error -> "ERROR"
    Esac -> "ESAC"
    Fi -> "FI"
    If -> "IF"
    In -> "IN"
    Inherits -> "INHERITS"
    IntConst s -> "INT_CONST " ++ s
    IsVoid -> "ISVOID"
    Lbrace -> "'{'"
    Le -> "LE"
    Let -> "LET"
    LetStmt -> "LET_STMT"
    Loop -> "LOOP"
    Lparen -> "'('"
    Lt -> "'<'"
    Minus -> "'-'"
    Mult -> "'*'"
    Neg -> "'~'"
    New -> "NEW"
    Not -> "NOT"
    ObjectId s -> "OBJECTID " ++ s
    Of -> "OF"
    Plus -> "'+'"
    Pool -> "POOL"
    Rbrace -> "'}'"
    Rparen -> "')'"
    Semi -> "';'"
    StrConst s -> "STR_CONST " ++ s
    Then -> "THEN"
    TypeId s -> "TYPEID " ++ s
    While -> "WHILE"
