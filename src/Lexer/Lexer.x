{
module Lexer.Lexer where
import qualified Data.Map as Map
import Data.Char
import Control.Monad

import Common.Types
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


processId :: String -> Token
processId s
  | isLower $ head s = if s' `elem` ["true", "false"]
                       then BoolConst
                       else ObjectId
  | isUpper $ head s = TypeId
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
