{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Hscool.Lexer.Base where

import Debug.Trace (traceShow)
import Data.Char (isLower, isUpper, toLower)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)

import Hscool.Types.Token (Token(..))
}

%wrapper "monadUserState"

$digit   = 0-9
$alpha   = [a-zA-Z]
$graphic = $printable # $white

@string  = \" (($printable # \") | (\\\"))* \"
tokens :-
  <0>
  $white+               ;

  <0, pcomment>
  "(*"     {lvlDown}

  <pcomment>
  "*)"     {lvlUp}

  <0>
  "--"     {begin mcomment}

  <mcomment>
  \n       {begin 0}

  <pcomment>
  \n       {skip}

  <pcomment, mcomment>
  ((~[\*\)\(\-])+) | ( \* | \) | \( | \) | \- )
           {skip}



    "<-" | "@" | ":" | ","  | "=>" | "/"
  | "."  | "=" | "{" | "<=" | "("  | "<"
  | "-"  | "*" | "~" | "+"  | "}"  | ")"
  | ";"    {tokenPos (simpleTokenMap Map.!)}

  $digit+  {tokenPos IntConst}
  [$alpha \_]([$digit $alpha \_]*)
           {tokenPos string2Token}
  @string  {tokenPos StrConst}

  .        {tokenPos Error}
{

type AlexUserState = Int

alexInitUserState :: AlexUserState
alexInitUserState = 0

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState f = Alex $ \s@(AlexState { alex_ust }) ->
    Right (s { alex_ust = f alex_ust }, ())

getLvl :: Alex Int
getLvl = Alex $ \s@AlexState{alex_ust=lvl} -> Right (s, lvl)

setLvl :: Int -> Alex()
setLvl lvl = Alex $ \s -> Right (s{alex_ust=lvl}, ())

alexGetPos :: Alex AlexPosn
alexGetPos = Alex $ \s@(AlexState { alex_pos }) -> Right (s, alex_pos)

alexEOF :: Alex (Int, Token)
alexEOF = do
    AlexPn _offset line _column <- alexGetPos
    return (line, Eof)

tokenPos :: (String -> Token) -> AlexAction (Int, Token)
tokenPos token (pos, _ch, _pending, s) len = case pos of
    AlexPn _offset line _column -> return (line, token $ take len s)

lvlDown :: AlexAction (Int, Token)
lvlDown input len = do
    lvl <- getLvl
    if lvl == 0
    then alexSetStartCode pcomment
    else return ()
    alexModifyUserState succ
    skip input len

lvlUp :: AlexAction (Int, Token)
lvlUp input len = do
    alexModifyUserState pred
    lvl <- getLvl
    if lvl == 0
    then alexSetStartCode 0
    else return ()
    skip input len

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


-- Note(matklad): this should be here because I said so.
main :: IO ()
main = do
    fileNames <- getArgs
    forM_ fileNames (\fileName -> do
                       contents <- readFile fileName
                       printf "#name \"%s\"\n" fileName
                       case runAlex contents (collect []) of
                         Left err     -> putStrLn err
                         Right tokens -> forM_ tokens $ \(line, token) ->
                           printf "#%d %s\n" line (show token))
  where
    collect :: [(Int, Token)] -> Alex [(Int, Token)]
    collect !acc = do
        (c, token) <- alexMonadScan
        if token == Eof
        then return . reverse $! acc
        else collect $! (c, token) : acc
}
