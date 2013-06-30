{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hscool.Types.AST
       (UProgram, UClass, UFeature, UExpr, UBranch, Symbol, Formal(..), NT(..)
       , TProgram, TClass, TFeature, TExpr, TBranch
       , Program(..), Class(..), Feature(..), Expr(..), Expr'(..), Branch(..),
         parseUProgram)
       where

import           Control.Applicative   (Applicative, (*>), (<$>), (<*), (<*>),
                                        (<|>), pure)
import           Control.Monad         (void)
import           Data.Attoparsec.Char8 (Parser, choice, endOfLine, many', many1,
                                        notChar, parseOnly, skipSpace, string)
import           Data.ByteString.Char8 (ByteString)
import           Data.List             (intercalate)
import           Text.Printf           (printf)


type Symbol = String

joinAndIndent :: Show a => [a] -> String
joinAndIndent = unlines . map indent
-- intercalate down and unlines up. They behave differently with empty lists
indent :: (Show a) => a -> String
indent = intercalate "\n" .  map ("  " ++) . lines . show

data Program a = Program [Class a]
data NT = NT

class Foo a where
    printFoo :: a -> String
    fooParser :: Parser a

instance Foo NT where
    printFoo _ = "_no_type"
    fooParser =  line $ (string ": _no_type" *> pure NT)

instance Foo String where
    printFoo = id
    fooParser = undefined

type UProgram = Program NT
type TProgram = Program String

instance Foo a => Show (Program a) where
  show (Program cls) = "#1\n_program\n" ++ joinAndIndent cls


data Class a = Class Symbol Symbol [Feature a] String

instance Eq (Class a) where
  (Class n1 _ _ _) == (Class n2 _ _ _) = n1 == n2

instance Ord (Class a) where
  compare (Class n1 _ _ _) (Class n2 _ _ _) = compare n1 n2

type UClass = Class NT
type TClass = Class String

instance Foo a => Show (Class a) where
  show (Class name super features fileName) =
    printf format name super fileName (joinAndIndent features)
    where
      format = "#1\n_class\n  %s\n  %s\n  %s\n  (\n%s  )\n"


data Feature a =
    Method Symbol [Formal] Symbol (Expr a)
  | Attribute Symbol Symbol (Expr a)

type UFeature = Feature NT
type TFeature = Feature String

instance Foo a => Show (Feature a) where
  show f = case f of
    Method name formals type_ body ->  printf "#1\n_method\n  %s\n%s  %s\n%s"
                                       name (joinAndIndent formals) type_ (indent body)
    Attribute name type_ init_ -> printf "#1\n_attr\n  %s\n  %s\n%s"
                                 name type_ (indent init_)


data Formal = Formal Symbol Symbol

instance Show Formal where
  show (Formal name type_) = printf "#1\n_formal\n  %s\n  %s\n" name type_

instance Eq Formal where
  (Formal n1 _)== (Formal n2 _) = n1 == n2

data Expr a = Expr a (Expr' a)

data Expr' a =
    Assign Symbol (Expr a)
  | Dispatch (Expr a) Symbol [Expr a]
  | StaticDispatch (Expr a) Symbol Symbol [Expr a]
  | Cond (Expr a) (Expr a) (Expr a)
  | Loop (Expr a) (Expr a)
  | TypeCase (Expr a) [Branch a]
  | Block [Expr a]
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

type UExpr = Expr NT
type TExpr = Expr String

instance Foo a => Show (Expr a) where
  show (Expr t expr)= aux ++ ": " ++ printFoo t
    where
      aux = case expr of
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

type UBranch = Branch NT
type TBranch = Branch String

instance Foo a => Show (Branch a) where
  show (Branch name type_ e) = printf "#1\n_branch\n  %s\n  %s\n%s"
                               name type_ (indent e)

uProgram :: Parser UProgram
uProgram = header "_program" *> (Program <$> many1 uClass)

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

op :: Parser ByteString
op = line $ string "("
cp :: Parser ByteString
cp = line $ string ")"

uFeature :: Parser UFeature
uFeature = method <|> attribute
  where
    method = header "_method" *>
             (Method <$> symbol <*> many' formal <*> symbol <*> uExpr)
    attribute = header "_attr" *>
                (Attribute <$> symbol <*> symbol <*> uExpr)

formal :: Parser Formal
formal = header "_formal" *>
         (Formal <$> symbol <*> symbol)

uExpr :: Parser UExpr
uExpr = do
        e <- choice [assign, dispatch, staticDispatch, cond, loop, typeCase,
                     block, let_, add, minus, mul, divide, neg, le, eq, leq, comp,
                     intConst, stringConst, boolConst, new, isVoid, noExpr, object]
        line $ string ": _no_type"
        return $ Expr NT e
  where
    assign = header "_assign" *>
             (Assign <$> symbol <*> uExpr)
    dispatch = header "_dispatch" *>
               (Dispatch <$> uExpr <*> symbol <*> wrap op (many' uExpr) cp)
    staticDispatch = header "_static_dispatch" *>
                     (StaticDispatch <$> uExpr <*> symbol <*> symbol
                     <*> wrap op (many' uExpr) cp)
    cond = header "_cond" *>
           (Cond <$> uExpr <*> uExpr <*> uExpr)
    loop = header "_loop" *>
           (Loop <$> uExpr <*> uExpr)
    typeCase = header "_typcase" *>
                (TypeCase <$> uExpr <*> many1 uBranch)
    block = header "_block" *>
            (Block <$> many' uExpr)
    let_ = header "_let" *>
           (Let <$> symbol <*> symbol <*> uExpr <*> uExpr)
    add = header "_plus" *>
          (Add <$> uExpr <*> uExpr)
    minus = header "_sub" *>
            (Minus <$> uExpr <*> uExpr)
    mul = header "_mul" *>
          (Mul <$> uExpr <*> uExpr)
    divide = header "_divide" *>
          (Div <$> uExpr <*> uExpr)
    neg = header "_neg" *>
          (Neg <$> uExpr)
    le = header "_lt" *>
         (Le <$> uExpr <*> uExpr)
    eq = header "_eq" *>
         (Eq <$> uExpr <*> uExpr)
    leq = header "_leq" *>
          (Leq <$> uExpr <*> uExpr)
    comp = header "_comp" *>
           (Comp <$> uExpr)
    intConst = header "_int" *>
               (IntConst <$> symbol)
    stringConst = header "_string" *>
                  (StringConst <$> symbol)
    boolConst = header "_bool" *>
                (BoolConst <$> ((== "1") <$> symbol))
    new = header "_new" *>
          (New <$> symbol)
    isVoid = header "_isvoid" *>
             (IsVoid <$> uExpr)
    noExpr = header "_no_expr" *>
             pure NoExpr
    object = header "_object" *>
             (Object <$> symbol)

uBranch :: Parser UBranch
uBranch = header "_branch" *> (Branch <$> symbol <*> symbol <*> uExpr)

line :: Parser a -> Parser a
line p = wrap skipSpace p endOfLine

header :: ByteString -> Parser ()
header h = void (line lineNumber *> line (string h))

lineNumber :: Parser ByteString
lineNumber = string "#1"

symbol :: Parser String
symbol = line $ many' (notChar '\n')

stringLiteral :: Parser String
stringLiteral = line $ many1 $ notChar '\n'


wrap :: Parser a -> Parser b -> Parser c -> Parser b
wrap p1 p2 p3 = (p1 *> p2) <* p3

parseUProgram :: ByteString -> UProgram
parseUProgram input = case parseOnly uProgram input of Right r -> r
