{
module Main (main) where
import qualified Data.Map as Map
}
%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic    = $printable # $white

@string     = \" ($graphic # \")* \"

tokens :-

  $white+               ;
  $digit+ {\s -> Int}
  [$digit $alpha \_]+ {\s -> processId}
  @string {\s -> StrConst}
{
-- Each action has type :: String -> Token

  -- The token type:
data Token =
  Class
  |Else
  |Fi
  |If
  |In
  |Inherits
  |Let
  |Loop
  |Pool
  |Then
  |While
  |Case
  |Esac
  |Of
  |Darrow
  |New
  |Isvoid
  |StrConst
  |IntConst
  |BoolConst
  |Typeid
  |Objectid
  |Assign
  |Not
  |Le
  |Error
  |LetStmt

processId :: String -> Token
processId s =
  where
    keywordMap = Map.fromList [
      ("class"),
      ("else"),
      ("fi"),
      ("if"),
      ("in"),
      ("inherits"),
      ("isvoid"),
      ("let"),
      ("loop"),
      ("pool"),
      ("then"),
      ("while"),
      ("case"),
      ("esac"),
      ("new"),
      ("of"),
      ("not")
      ]

main = do
  s <- getContents
  print (alexScanTokens s)
}
