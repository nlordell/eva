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

%{

  let cons_opt x xs = match x with
    | None -> xs
    | Some x -> x :: xs

%}

%%

main:
  | r = rmain; EOF { List.rev r }

rmain:
  | s = stmt                 { cons_opt s [] }
  | r = rmain; EOL; s = stmt { cons_opt s r }

stmt:
  |                      { None }
  | l = LABEL            { Some Ast.(Label l) }
  | i = IDENT; a = rargs { Some Ast.(Inst (i, List.rev a)) }
  | DATA; s = STRING     { Some Ast.(Data s) }

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
