{
module Hscool.Lexer.Base where

import Debug.Trace (traceShow)
import Data.Char (isLower, isUpper, toLower)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)

import Hscool.Types (Token(..))
}

%wrapper "posn"

$digit   = 0-9
$alpha   = [a-zA-Z]
$graphic = $printable # $white

@string  = \" ($graphic # \")* \"

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

  $digit+                 {\(AlexPn _ c _) s -> (c, IntConst s)}
  -- FIXME(superbobry): do we allow identifiers like '42foo'?
  [$digit $alpha \_]+     {\(AlexPn _ c _) s -> (c, string2Token s)}
  @string                 {\(AlexPn _ c _) s -> (c, StrConst s)}
{

keywordMap :: Map String Token
keywordMap = Map.fromList
             [ ("class", Class)
             , ("else", Else)
             , ("fi", Fi)
             , ("if", If)
             , ("in", In)
             , ("inherits", Inherits)
             , ("isvoid", IsVoid)
             , ("let", Let)
             , ("loop", Loop)
             , ("pool", Pool)
             , ("then", Then)
             , ("while", While)
             , ("case", Case)
             , ("esac", Esac)
             , ("new", New)
             , ("of", Of)
             , ("not", Not)
             ]

string2Token :: String -> Token
string2Token "" = error "string2Token: empty string"
string2Token s@(ch:_)
  | s' `Map.member` keywordMap = keywordMap Map.! s'
  | isLower ch && s' `elem` ["true", "false"] = BoolConst s'
  | isUpper ch = TypeId s
  | otherwise  = ObjectId s
  where
    s' = map toLower s

-- this should be here because I said so
main :: IO ()
main = do
    [fileName] <- getArgs
    contents <- readFile fileName
    printf "#name \"%s\"\n" fileName
    forM_ (alexScanTokens contents) $ \(c, t) ->
        printf "#%d %s\n" c (show t)
}
