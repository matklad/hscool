{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Hscool.Types.AST
       (UProgram, UClass, UFeature, UExpr, UBranch, Symbol(..), Formal(..)
       , Program(..), Class(..), Feature(..), Expr(..), Expr'(..), Branch(..),
         parseUProgram)
       where

import Text.Printf (printf)
import Data.List (intercalate)
import Data.Attoparsec.Char8 (Parser, many1, many', space, string, skipWhile,
                              skipSpace, notInClass, endOfLine, letter_ascii,
                              char, notChar, anyChar, choice, parse,
                              IResult(..), parseOnly)
import Data.ByteString.Char8 (ByteString, unpack)

import Control.Applicative ((<$>), (<|>), (<*>))


type Symbol = String

joinAndIndent :: (Show a) => [a] -> String
joinAndIndent = unlines . map indent
-- intercalate down and unlines up. They behave differently with empty lists
indent :: (Show a) => a -> String
indent = intercalate "\n" .  map ("  " ++) . lines . show

data Program a = Program [Class a]

type UProgram = Program ()

instance Show UProgram where
  show (Program cls) = "#1\n_program\n" ++ joinAndIndent cls


data Class a = Class Symbol Symbol [Feature a] String

type UClass = Class ()

instance Show UClass where
  show (Class name super features fileName) =
    printf format name super fileName (joinAndIndent features)
    where
      format = "#1\n_class\n  %s\n  %s\n  %s\n  (\n%s  )\n"


data Feature a =
    Method Symbol [Formal] Symbol (Expr a)
  | Attribute Symbol Symbol (Expr a)

type UFeature = Feature ()

instance Show UFeature where
  show f = case f of
    Method name formals type_ body ->  printf "#1\n_method\n  %s\n%s  %s\n%s"
                                       name (joinAndIndent formals) type_ (indent body)
    Attribute name type_ init -> printf "#1\n_attr\n  %s\n  %s\n%s"
                                 name type_ (indent init)


data Formal = Formal Symbol Symbol

instance Show Formal where
  show (Formal name type_) = printf "#1\n_formal\n  %s\n  %s\n" name type_

data Expr a = Expr a (Expr' a)

data Expr' a =
    Assign Symbol (Expr a)
  | Dispatch (Expr a) Symbol [(Expr a)]
  | StaticDispatch (Expr a) Symbol Symbol [(Expr a)]
  | Cond (Expr a) (Expr a) (Expr a)
  | Loop (Expr a) (Expr a)
  | TypeCase (Expr a) [Branch a]
  | Block [(Expr a)]
  | Let Symbol Symbol (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  | Minus (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Neg (Expr a)
  | Le (Expr a) (Expr a)
  | Eq (Expr a) (Expr a)
  | Leq (Expr a) (Expr a)
  | Comp (Expr a)
  | IntConst Symbol
  | StringConst Symbol
  | BoolConst Bool
  | New Symbol
  | IsVoid (Expr a)
  | NoExpr
  | Object Symbol

type UExpr = Expr ()

instance Show UExpr where
  show = (++ ": _no_type") . aux
    where
      aux (Expr _ expr) = case expr of
        Assign s e -> printf "#1\n_assign\n  %s\n%s\n" s (indent e)
        Dispatch e s actuals -> printf "#1\n_dispatch\n%s\n  %s\n  (\n%s  )\n"
                                (indent e) s (joinAndIndent actuals)
        StaticDispatch e type_ name actuals ->
          printf "#1\n_static_dispatch\n%s\n  %s\n  %s\n  (\n%s  )\n"
          (indent e) type_ name (joinAndIndent actuals)
        Cond e1 e2 e3 -> printf "#1\n_cond\n%s\n%s\n%s\n"
                         (indent e1) (indent e2) (indent e3)
        Loop e1 e2 -> printf "#1\n_loop\n%s\n%s\n"
                      (indent e1) (indent e2)
        TypeCase e bs -> printf "#1\n_typcase\n%s\n%s"
                         (indent e) (joinAndIndent bs)
        Block es -> printf "#1\n_block\n%s" (joinAndIndent es)
        Let name type_ ini e -> printf "#1\n_let\n  %s\n  %s\n%s\n%s\n"
                                name type_ (indent ini) (indent e)
        Add e1 e2 -> printf "#1\n_plus\n%s\n%s\n" (indent e1) (indent e2)
        Minus e1 e2 -> printf "#1\n_sub\n%s\n%s\n" (indent e1) (indent e2)
        Mul e1 e2 -> printf "#1\n_mul\n%s\n%s\n" (indent e1) (indent e2)
        Div e1 e2 -> printf "#1\n_divide\n%s\n%s\n" (indent e1) (indent e2)
        Neg e -> printf "#1\n_neg\n%s\n" (indent e)
        Le e1 e2 -> printf "#1\n_lt\n%s\n%s\n" (indent e1) (indent e2)
        Eq e1 e2 -> printf "#1\n_eq\n%s\n%s\n" (indent e1) (indent e2)
        Leq e1  e2 -> printf "#1\n_leq\n%s\n%s\n" (indent e1) (indent e2)
        Comp e -> printf "#1\n_comp\n%s\n" (indent e)
        IntConst s -> printf "#1\n_int\n  %s\n" s
        StringConst s -> printf "#1\n_string\n  %s\n" s
        BoolConst b -> printf "#1\n_bool\n  %d\n" ((if b then 1 else 0)::Int)
        New s -> printf "#1\n_new\n  %s\n" s
        IsVoid e -> printf "#1\n_isvoid\n%s\n" (indent e)
        NoExpr -> printf "#1\n_no_expr\n"
        Object s -> printf "#1\n_object\n  %s\n" s

data Branch a = Branch Symbol Symbol (Expr a)

type UBranch = Branch ()

instance Show UBranch where
  show (Branch name type_ e) = printf "#1\n_branch\n  %s\n  %s\n%s"
                               name type_ (indent e)

uProgram :: Parser UProgram
uProgram = header "_program" >> (Program <$> many1 uClass)

uClass :: Parser UClass
uClass = do
  header "_class"
  name <- symbol
  super <-  symbol
  file <- stringLiteral
  op
  features <- many' uFeature
  cp
  return $ Class name super features file

op = line $ string "("
cp = line $ string ")"

uFeature :: Parser UFeature
uFeature = method <|> attribute
  where
    method = header "_method" >>
             (Method <$> symbol <*> (many' formal) <*> symbol <*> uExpr)
    attribute = header "_attr" >>
                (Attribute <$> symbol <*> symbol <*> uExpr)

formal = header "_formal" >>
         (Formal <$> symbol <*> symbol)

uExpr = do
        e <- choice [assign, dispatch, staticDispatch, cond, loop, typeCase,
                     block, let_, add, minus, mul, div, neg, le, eq, leq, comp,
                     intConst, stringConst, boolConst, new, isVoid, noExpr, object]
        line $ string ": _no_type"
        return $ Expr () e
  where
    assign = header "_assign" >>
             Assign <$> symbol <*> uExpr
    dispatch = header "_dispatch" >>
               Dispatch <$> uExpr <*> symbol <*> wrap op (many' uExpr) cp
    staticDispatch = header "_static_dispatch" >>
                     StaticDispatch <$> uExpr <*> symbol <*> symbol
                     <*> wrap op (many' uExpr) cp
    cond = header "_cond" >>
           Cond <$> uExpr <*> uExpr <*> uExpr
    loop = header "_loop" >>
           Loop <$> uExpr <*> uExpr
    typeCase = header "_typcase" >>
                TypeCase <$> uExpr <*> many1 uBranch
    block = header "_block" >>
            Block <$> many' uExpr
    let_ = header "_let" >>
           Let <$> symbol <*> symbol <*> uExpr <*> uExpr
    add = header "_plus" >>
          Add <$> uExpr <*> uExpr
    minus = header "_sub" >>
            Minus <$> uExpr <*> uExpr
    mul = header "_mul" >>
          Mul <$> uExpr <*> uExpr
    div = header "_divide" >>
          Div <$> uExpr <*> uExpr
    neg = header "_neg" >>
          Neg <$> uExpr
    le = header "_lt" >>
         Le <$> uExpr <*> uExpr
    eq = header "_eq" >>
         Eq <$> uExpr <*> uExpr
    leq = header "_leq" >>
          Leq <$> uExpr <*> uExpr
    comp = header "_comp" >>
           Comp <$> uExpr
    intConst = header "_int" >>
               IntConst <$> symbol
    stringConst = header "_string" >>
                  StringConst <$> symbol
    boolConst = header "_bool" >>
                BoolConst <$> ((== "1") <$> symbol)
    new = header "_new" >>
          New <$> symbol
    isVoid = header "_isvoid" >>
             IsVoid <$> uExpr
    noExpr = header "_no_expr" >>
             return NoExpr
    object = header "_object" >>
             Object <$> symbol

uBranch :: Parser UBranch
uBranch = header "_branch" >> Branch <$> symbol <*> symbol <*> uExpr

line :: Parser a -> Parser a
line p = wrap skipSpace p endOfLine

header :: ByteString -> Parser ()
header h = line (lineNumber) >> line (string h) >> return ()

lineNumber :: Parser ByteString
lineNumber = string "#1"

symbol :: Parser String
symbol = line $ many' (notChar '\n')

stringLiteral :: Parser String
stringLiteral = line $ many1 $ notChar '\n'

wrap p1 p2 p3 = do
  p1
  r <- p2
  p3
  return r

--parseUProgram :: ByteString -> UProgram
-- parseUProgram input = case parse uProgram input of
--   Fail t cs s -> error (s ++ "\n\n" ++ (unlines cs))
--   Partial _ -> error "partial result"
--   Done t r -> r
parseUProgram input = case parseOnly uProgram input of Right r -> r
--parseUProgram  = parse uProgram
