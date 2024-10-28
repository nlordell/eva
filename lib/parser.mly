%token <string> IDENT
%token <Z.t> INT
%token <int> IMMEDIATE
%token <string> STRING

%token <string> LABEL
%token DATA

%token EXPR
%token LPAREN RPAREN
%token PLUS MINUS TIMES SLASH

%token EOL EOF

%left PLUS MINUS
%left TIMES SLASH
%nonassoc unary

%start <Ast.t> main

%%

main:
  | r = rmain; EOF { List.rev r }

rmain:
  |                                      { [] }
  | r = rmain; EOL                       { r }
  | r = rmain; l = LABEL; EOL            { Ast.(Label l) :: r }
  | r = rmain; i = IDENT; a = rargs; EOL { Ast.(Inst (i, List.rev a)) :: r }
  | r = rmain; DATA; s = STRING; EOL     { Ast.(Data s) :: r }

rargs:
  |                    { [] }
  | r = rargs; a = arg { a :: r }

arg:
  | i = IMMEDIATE          { Ast.Immediate i }
  | i = INT                { Ast.(Expr (Value i)) }
  | EXPR; e = expr; RPAREN { Ast.Expr e }

expr:
  | i = IDENT                   { Ast.Ref i }
  | i = INT                     { Ast.Value i }
  | LPAREN; e = expr; RPAREN    { e }
  | MINUS; e = expr %prec unary { Ast.Neg e }
  | e = expr; PLUS; f = expr    { Ast.Add (e, f) }
  | e = expr; MINUS; f = expr   { Ast.Sub (e, f) }
  | e = expr; TIMES; f = expr   { Ast.Mul (e, f) }
  | e = expr; SLASH; f = expr   { Ast.Div (e, f) }
