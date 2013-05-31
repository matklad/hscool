{
module Main (main) where
import qualified Data.Map as Map
import Data.Char
import Control.Monad
}
%wrapper "basic"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic    = $printable # $white

@string     = \" ($graphic # \")* \"

tokens :-

  $white+               ;
  "@" {\s -> At}
  ":" {\s -> Colon}
  "," {\s -> Coma}
  "=>" {\s -> Darrow}
  "/" {\s -> Div}
  "." {\s -> Dot}
  "=" {\s -> Eq}
  "{" {\s -> Lbrace}
  "<=" {\s -> Le}
  "(" {\s -> Lparen}
  "<" {\s -> Lt}
  "-" {\s -> Minus}
  "*" {\s -> Mult}
  "~" {\s -> Neg}
  "+" {\s -> Plus}
  "}" {\s -> Rbrace}
  ")" {\s -> Rparen}
  ";" {\s -> Semi}
  $digit+ {\s -> IntConst}
  [$digit $alpha \_]+ {processId}
  @string {\s -> StrConst}
{
-- Each action has type :: String -> Token

  -- The token type:
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

processId :: String -> Token
processId s
  | s' == "false" && (isLower $ head s) = BoolConst
  | s' == "true" && (isLower $ head s) = BoolConst
  | s' `Map.member` keywordMap = keywordMap Map.! s'
  | otherwise = ObjectId
  where
    keywordMap = Map.fromList [
      ("class", Class),
      ("else", Else),
      ("fi", Fi),
      ("if", If),
      ("in", In),
      ("inherits", Inherits),
      ("isvoid", IsVoid),
      ("let", Let),
      ("loop", Loop),
      ("pool", Pool),
      ("then", Then),
      ("while", While),
      ("case", Case),
      ("esac", Esac),
      ("new", New),
      ("of", Of),
      ("not", Not)
      ]
    s' = map toLower s

main = do
  s <- getContents
  forM (alexScanTokens s) print
}
