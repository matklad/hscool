module Hscool.Types where

data Token =
  Assign
  |At
  |BoolConst String
  |Case
  |Class
  |Colon
  |Coma
  |Darrow
  |Div
  |Dot
  |Else
  |Eof
  |Eq
  |Error
  |Esac
  |Fi
  |If
  |In
  |Inherits
  |IntConst String
  |IsVoid
  |Lbrace
  |Le
  |Let
  |LetStmt
  |Loop
  |Lparen
  |Lt
  |Minus
  |Mult
  |Neg
  |New
  |Not
  |ObjectId String
  |Of
  |Plus
  |Pool
  |Rbrace
  |Rparen
  |Semi
  |StrConst String
  |Then
  |TypeId String
  |While
  deriving (Show, Eq)
