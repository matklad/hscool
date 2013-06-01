{
module Lexer.Lexer where
import qualified Data.Map as Map
import Data.Char
import Control.Monad

import Common.Types
}
%wrapper "posn"

$digit = 0-9       -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$graphic    = $printable # $white

@string     = \" ($graphic # \")* \"

tokens :-

  $white+               ;
  "@"  {\(AlexPn _ c _) s -> (c, At)}
  ":"  {\(AlexPn _ c _) s -> (c, Colon)}
  ","  {\(AlexPn _ c _) s -> (c, Coma)}
  "=>" {\(AlexPn _ c _) s -> (c, Darrow)}
  "/"  {\(AlexPn _ c _) s -> (c, Div)}
  "."  {\(AlexPn _ c _) s -> (c, Dot)}
  "="  {\(AlexPn _ c _) s -> (c, Eq)}
  "{"  {\(AlexPn _ c _) s -> (c, Lbrace)}
  "<=" {\(AlexPn _ c _) s -> (c, Le)}
  "("  {\(AlexPn _ c _) s -> (c, Lparen)}
  "<"  {\(AlexPn _ c _) s -> (c, Lt)}
  "-"  {\(AlexPn _ c _) s -> (c, Minus)}
  "*"  {\(AlexPn _ c _) s -> (c, Mult)}
  "~"  {\(AlexPn _ c _) s -> (c, Neg)}
  "+"  {\(AlexPn _ c _) s -> (c, Plus)}
  "}"  {\(AlexPn _ c _) s -> (c, Rbrace)}
  ")"  {\(AlexPn _ c _) s -> (c, Rparen)}
  ";"  {\(AlexPn _ c _) s -> (c, Semi)}

  $digit+                 {\(AlexPn _ c _) s -> (c, IntConst)}
  [$digit $alpha \_]+     {\(AlexPn _ c _) s -> (c, processId s)}
  @string                 {\(AlexPn _ c _) s -> (c, StrConst)}
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
