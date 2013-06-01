module Hscool.Types where

data Token =
  Assign
  |At
  |BoolConst
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
  |IntConst
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
  |ObjectId
  |Of
  |Plus
  |Pool
  |Rbrace
  |Rparen
  |Semi
  |StrConst
  |Then
  |TypeId
  |While
  deriving (Show, Eq)
