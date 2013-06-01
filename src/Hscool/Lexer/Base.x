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
  "<-"
  | "@"
  | ":"
  | ","
  | "=>"
  | "/"
  | "."
  | "="
  | "{"
  | "<="
  | "("
  | "<"
  | "-"
  | "*"
  | "~"
  | "+"
  | "}"
  | ")"
  | ";"  {\(AlexPn _ c _) s -> (c, simpleTokenMap Map.! s)}

  $digit+                          {\(AlexPn _ c _) s -> (c, IntConst s)}
  [$alpha \_]([$digit $alpha \_]*) {\(AlexPn _ c _) s -> (c, string2Token s)}
  @string                          {\(AlexPn _ c _) s -> (c, StrConst s)}

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

simpleTokenMap :: Map String Token
simpleTokenMap = Map.fromList
                 [ ("<-", Assign)
                 , ("@", At)
                 , (":", Colon)
                 , (",", Coma)
                 , ("=>", Darrow)
                 , ("/", Div)
                 , (".", Dot)
                 , ("=", Eq)
                 , ("{", Lbrace)
                 , ("<=", Le)
                 , ("(", Lparen)
                 , ("<", Lt)
                 , ("-", Minus)
                 , ("*", Mult)
                 , ("~", Neg)
                 , ("+", Plus)
                 , ("}", Rbrace)
                 , (")", Rparen)
                 , (";", Semi)
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
