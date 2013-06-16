{
module Hscool.Parser.Base where
import qualified Hscool.Types.Token as T
import Hscool.Types.AST
}

%name parse
%tokentype { T.Token }
%error { parseError }

%token
  ASSIGN    { T.Assign }
  '@'       { T.At }
  BOOL      { T.BoolConst $$ }
  CASE      { T.Case }
  CLASS     { T.Class }
  ':'       { T.Colon }
  ','       { T.Coma }
  DARROW    { T.Darrow }
  '/'       { T.Div }
  '.'       { T.Dot }
  ELSE      { T.Else }
  '='       { T.Eq }
  ESAC      { T.Esac }
  FI        { T.Fi }
  IF        { T.If }
  IN        { T.In }
  INHERITS  { T.Inherits }
  INT       { T.IntConst $$ }
  ISVOID    { T.IsVoid }
  '{'       { T.Lbrace }
  LE        { T.Le }
  LET       { T.Let }
  LETSTMT   { T.LetStmt }
  LOOP      { T.Loop }
  '('       { T.Lparen }
  '<'       { T.Lt }
  '-'       { T.Minus }
  '*'       { T.Mult }
  '~'       { T.Neg }
  NEW       { T.New }
  NOT       { T.Not }
  ID        { T.ObjectId $$ }
  OF        { T.Of }
  '+'       { T.Plus }
  POOL      { T.Pool }
  '}'       { T.Rbrace }
  ')'       { T.Rparen }
  ';'       { T.Semi }
  STR       { T.StrConst $$ }
  THEN      { T.Then }
  TYPE      { T.TypeId $$ }
  WHILE     { T.While }
  FILE_NAME { T.FileName $$}

%left '.'
%left '@'
%left '~'
%left ISVOID
%left '*' '/'
%left '+' '-'
%left LE '<' '='
%left not
%right ASSIGN

%%

program :: { Program }
program
  : files             { Program ((concat . reverse) $1) }

files :: { [[Class]] }
files
  : file              { [$1] }
  | files file        { $2 : $1 }

file :: { [Class] }
  : FILE_NAME classes {map (\x -> x $1) (reverse $2)}

classes :: { [String -> Class] }
classes
  : class ';'           { [$1] }
  | classes class       { $2 : $1 }

class :: { String -> Class }
class
  : CLASS TYPE '{' features '}'
                        { Class $2 "Object" (reverse $4) }
  | CLASS TYPE INHERITS TYPE '{' features '}'
                        { Class $2 $4 (reverse $6) }

features :: { [Feature] }
features
  :                     { [] }
  | feature ';'         { [$1] }
  | features feature    { $2 : $1 }

feature :: { Feature }
feature
  : ID '(' formals ')' ':' TYPE '{' expr '}'
                        { Method $1 (reverse $3) $6 $8 }

formals :: { [Formal] }
formals
  :                     { [] }
  | formal              { [$1] }
  | formals ',' formal  { $3 : $1 }

formal :: { Formal }
formal
  : ID ':' TYPE         { Formal $1 $3 }

expr :: { Expression }
expr
  : ID ASSIGN expr      { Assign $1 $3}
  | ID '(' expressions ')' { Dispatch (Object "self") "SELF_TYPE" $1 (reverse $3) }
  | expr '+' expr       { $1 :+ $3 }
  | expr '-' expr       { $1 :- $3 }
  | expr '*' expr       { $1 :* $3 }
  | expr '/' expr       { $1 :/ $3 }
  | '~' expr            { Neg $2 }
  | expr '<' expr       { $1 :< $3 }
  | expr LE expr        { $1 :<= $3 }
  | expr '=' expr       { $1 := $3}

  | INT                 { IntConst $1 }
  | STR                 { StringConst $1 }

expressions :: { [Expression] }
  :                     { [] }
  | expr                { [$1] }
  | expressions ',' expr { $3 : $1 }

{
parseError :: [T.Token] -> a
parseError _ = error "Parse error"

main :: IO ()
main = getContents >>= putStr . show . parse . T.readTokens

}
