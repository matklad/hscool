module Hscool.Types.Token
    (
      Token(..)
    , readTokens
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
    | Error String
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
    | FileName String
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
    Error s -> "ERROR " ++ s
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
    FileName s -> "FILE_NAME" ++ s

readTokens :: String -> [Token]
readTokens = (map readToken) . lines
  where
    readToken = getToken . words
    getToken ws =
      let line_number = ws !! 0
          tok_type = ws !! 1
          arg = ws !! 2
      in
       if line_number == "#name"
       then FileName tok_type
       else
         case tok_type of
           "ASSIGN" -> Assign
           "BOOL_CONST" -> BoolConst arg
           "CASE" -> Case
           "CLASS" -> Class
           "':'" -> Colon
           "','" -> Coma
           "DARROW" -> Darrow
           "'/;" -> Div
           "'.'" -> Dot
           "ELSE" -> Else
           "EOF" -> Eof
           "'='" -> Eq
           "ERROR" -> error arg
           "ESAC" -> Esac
           "FI" -> Fi
           "IF" -> If
           "IN" -> In
           "INHERITS" -> Inherits
           "INT_CONST" -> IntConst arg
           "ISVOID" -> IsVoid
           "'{'" -> Lbrace
           "LE" -> Le
           "LET" -> Let
           "LOOP" -> Loop
           "'('" -> Lparen
           "'<'" -> Lt
           "'-'" -> Minus
           "'*'" -> Mult
           "'~'" -> Neg
           "NEW" -> New
           "NOT" -> Not
           "OBJECTID" -> ObjectId arg
           "OF" -> Of
           "'+'" -> Plus
           "POOL" -> Pool
           "'}'" -> Rbrace
           "')'" -> Rparen
           "';'" -> Semi
           "STR_CONST" -> StrConst arg
           "THEN" -> Then
           "TYPEID" -> TypeId arg
           "WHILE" -> While
           x -> error x
