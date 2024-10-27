%{

  type r = { l : (string * int) list; c : Ast.code list; n : int }

  let empty = { l = []; c = []; n = 0 }
  let add_label r l = { r with l = (l, r.n) :: r.l }
  let add_code r c = { r with c = c :: r.c; n = r.n + 1 }

  let to_ast { l; c; n = _ } =
    Ast.{ labels = List.rev l; code = List.rev c |> Array.of_list }

%}

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

%start <Ast.t> prog

%%

prog:
  | r = rprog; EOF { to_ast r }

rprog:
  |                                      { empty }
  | r = rprog; EOL                       { r }
  | r = rprog; l = LABEL; EOL            { add_label r l }
  | r = rprog; i = IDENT; a = rargs; EOL { add_code r (Ast.Inst (i, List.rev a)) }
  | r = rprog; DATA; s = STRING; EOL     { add_code r (Ast.Data s) }

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
