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

%right ASSIGN
%left not
%left LE '<' '='
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'

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
  : class               { [$1] }
  | classes class       { $2 : $1 }

class :: { String -> Class }
class
  : CLASS TYPE '{' features '}' ';'
                        { Class $2 "Object" (reverse $4) }
  | CLASS TYPE INHERITS TYPE '{' features '}' ';'
                        { Class $2 $4 (reverse $6) }

features :: { [Feature] }
features
  :                     { [] }
  | feature             { [$1] }
  | features feature    { $2 : $1 }

feature :: { Feature }
feature
  : ID '(' formals ')' ':' TYPE '{' expr '}' ';'
                        { Method $1 (reverse $3) $6 $8 }
  | decl ';'            { let (name, type_, expr) = $1 in
                             Attribute name type_ expr}

decl :: { Symbol, Symbol, Expression }
  : ID ':' TYPE         {  ($1, $3, NoExpr) }
  | ID ':' TYPE ASSIGN expr
                        { ($1, $3, $5) }

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
  | expr '.' ID '(' exprs_coma ')'
                        { Dispatch $1 $3 (reverse $5) }
  | expr '@' TYPE '.' ID '(' exprs_coma ')'
                        { StaticDispatch $1 $3 $5 (reverse $7) }
  | ID '(' exprs_coma ')'
                        { Dispatch (Object "self") $1 (reverse $3) }
  | IF expr THEN expr ELSE expr FI
                        { Cond $2 $4 $6 }
  | WHILE expr LOOP expr POOL
                        { Loop $2 $4 }
  | '{' exprs_semi '}'  { Block ( reverse $2)}
  | LET decls IN expr   { makeLet (reverse $2) $4 }
  | CASE expr OF branches ESAC
                        { TypeCase $2 (reverse $4) }
  | NEW TYPE            { New $2 }
  | ISVOID expr         { IsVoid $2 }
  | expr '+' expr       { $1 :+ $3 }
  | expr '-' expr       { $1 :- $3 }
  | expr '*' expr       { $1 :* $3 }
  | expr '/' expr       { $1 :/ $3 }
  | '~' expr            { Neg $2 }
  | expr '<' expr       { $1 :< $3 }
  | expr LE expr        { $1 :<= $3 }
  | expr '=' expr       { $1 := $3 }
  | NOT expr            { Comp $2 }
  | '(' expr ')'        { $2 }
  | ID                  { Object $1 }
  | INT                 { IntConst $1 }
  | STR                 { StringConst $1 }
  | BOOL                { BoolConst ($1 == "true")}

exprs_coma :: { [Expression] }
  :                     { [] }
  | expr                { [$1] }
  | exprs_coma ',' expr { $3 : $1 }

exprs_semi :: { [Expression] }
  : expr ';'            { [$1] }
  | exprs_semi expr ';' { $2 : $1 }

decls :: { [(Symbol, Symbol, Expression)] }
  : decl                { [$1] }
  | decls ',' decl      { $3 : $1 }

branch :: { Branch }
  : ID ':' TYPE DARROW expr ';'
                        { Branch $1 $3 $5}
branches :: { [Branch] }
  : branch                { [$1] }
  | branches branch       { $2 : $1 }

{
parseError :: [T.Token] -> a
parseError t = error $ "Parse error. Token:\n" ++ (show t)

makeLet :: [(Symbol, Symbol, Expression)] -> Expression -> Expression
makeLet ([(name, type_, ini)]) e = Let name type_ ini e
makeLet ((name, type_, ini):xs) e = Let name type_ ini (makeLet xs e)

main :: IO ()
main = getContents >>= putStr . show . parse . T.readTokens

}
