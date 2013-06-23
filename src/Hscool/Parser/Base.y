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

program :: { UProgram }
program
  : files             { Program ((concat . reverse) $1) }

files :: { [[UClass]] }
files
  : file              { [$1] }
  | files file        { $2 : $1 }

file :: { [UClass] }
  : FILE_NAME classes {map (\x -> x $1) (reverse $2)}

classes :: { [String -> UClass] }
classes
  : class               { [$1] }
  | classes class       { $2 : $1 }

class :: { String -> UClass }
class
  : CLASS TYPE '{' features '}' ';'
                        { Class $2 "Object" (reverse $4) }
  | CLASS TYPE INHERITS TYPE '{' features '}' ';'
                        { Class $2 $4 (reverse $6) }

features :: { [UFeature] }
features
  :                     { [] }
  | feature             { [$1] }
  | features feature    { $2 : $1 }

feature :: { UFeature }
feature
  : ID '(' formals ')' ':' TYPE '{' expr '}' ';'
                        { Method $1 (reverse $3) $6 $8 }
  | decl ';'            { let (name, type_, expr) = $1 in
                             Attribute name type_ expr}

decl :: { Symbol, Symbol, UExpr }
  : ID ':' TYPE         {  ($1, $3, (Expr NT NoExpr)) }
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

expr :: { UExpr }
expr
  : ID ASSIGN expr      { Expr NT (Assign $1 $3)}
  | expr '.' ID '(' exprs_coma ')'
                        { Expr NT (Dispatch $1 $3 (reverse $5)) }
  | expr '@' TYPE '.' ID '(' exprs_coma ')'
                        { Expr NT (StaticDispatch $1 $3 $5 (reverse $7)) }
  | ID '(' exprs_coma ')'
                        { Expr NT (Dispatch (Expr NT (Object "self")) $1 (reverse $3)) }
  | IF expr THEN expr ELSE expr FI
                        { Expr NT (Cond $2 $4 $6) }
  | WHILE expr LOOP expr POOL
                        { Expr NT (Loop $2 $4) }
  | '{' exprs_semi '}'  { Expr NT (Block (reverse $2)) }
  | LET decls IN expr   { makeLet (reverse $2) $4 }
  | CASE expr OF branches ESAC
                        { Expr NT (TypeCase $2 (reverse $4)) }
  | NEW TYPE            { Expr NT (New $2) }
  | ISVOID expr         { Expr NT (IsVoid $2) }
  | expr '+' expr       { Expr NT (Add $1 $3) }
  | expr '-' expr       { Expr NT (Minus $1 $3) }
  | expr '*' expr       { Expr NT (Mul $1 $3) }
  | expr '/' expr       { Expr NT (Div $1 $3) }
  | '~' expr            { Expr NT (Neg $2) }
  | expr '<' expr       { Expr NT (Le $1 $3) }
  | expr LE expr        { Expr NT (Leq $1  $3) }
  | expr '=' expr       { Expr NT (Eq $1 $3) }
  | NOT expr            { Expr NT (Comp $2) }
  | '(' expr ')'        { $2 }
  | ID                  { Expr NT (Object $1) }
  | INT                 { Expr NT (IntConst $1) }
  | STR                 { Expr NT (StringConst $1) }
  | BOOL                { Expr NT (BoolConst ($1 == "true")) }

exprs_coma :: { [UExpr] }
  :                     { [] }
  | expr                { [$1] }
  | exprs_coma ',' expr { $3 : $1 }

exprs_semi :: { [UExpr] }
  : expr ';'            { [$1] }
  | exprs_semi expr ';' { $2 : $1 }

decls :: { [(Symbol, Symbol, UExpr)] }
  : decl                { [$1] }
  | decls ',' decl      { $3 : $1 }

branch :: { UBranch }
  : ID ':' TYPE DARROW expr ';'
                        { Branch $1 $3 $5}
branches :: { [UBranch] }
  : branch                { [$1] }
  | branches branch       { $2 : $1 }

{
parseError :: [T.Token] -> a
parseError t = error $ "Parse error. Token:\n" ++ (show t)

makeLet :: [(Symbol, Symbol, UExpr)] -> UExpr -> UExpr
makeLet ([(name, type_, ini)]) e = Expr NT (Let name type_ ini e)
makeLet ((name, type_, ini):xs) e = Expr NT (Let name type_ ini (makeLet xs e))

main :: IO ()
main = getContents >>= putStr . show . parse . T.readTokens

}
