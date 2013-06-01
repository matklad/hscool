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
$symbol  = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\{\}\~\(\)]

@string  = \" ($graphic # \")* \"

tokens :-
  $white+               ;
  $symbol / { isKnownOp }
            { \(AlexPn _ c _ ) s -> (c, string2Op s) }
  $digit+                 {\(AlexPn _ c _) s -> (c, IntConst s)}
  -- FIXME(superbobry): do we allow identifiers like '42foo'?
  [$digit $alpha \_]+     {\(AlexPn _ c _) s -> (c, string2Token s)}
  @string                 {\(AlexPn _ c _) s -> (c, StrConst s)}
{

isKnownOp :: String -> AlexInput -> Int -> AlexInput -> Bool
isKnownOp s _left _len _right = s `Map.member` opMap

opMap :: Map String Token
opMap = Map.fromList
        [ ("@", At)
        , (":", Colon)
        , (",", Coma)
        , ("=>", Darrow)
        , ("/", Div)
        , (".", Dot)
        , ("{", Lbrace)
        , ("<=", Le)
        , ("(" , Lparen)
        , ("<" , Lt)
        , ("-" , Minus)
        , ("*" , Mult)
        , ("~" , Neg)
        , ("+" , Plus)
        , ("}" , Rbrace)
        , (")" , Rparen)
        , (";" , Semi)
        ]

string2Op :: String -> Token
string2Op = (opMap Map.!)

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
  | isUpper ch = TypeId s
  | isLower ch && s' `elem` ["true", "false"] = BoolConst s'
  | otherwise = ObjectId s
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
