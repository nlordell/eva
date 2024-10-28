{

  open Parser

  let lexeme_1 lexbuf =
    let l = Lexing.lexeme lexbuf in
    String.sub l 1 (String.length l - 1)

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let digit = ['0'-'9' 'a'-'f' 'A'-'F']
let int = ("0b" | "0o" | "0x")? digit+
let immediate = '%' ['0'-'9']+

rule read =
  parse
  | white     { read lexbuf }
  | newline   { EOL }
  | ident     { IDENT (Lexing.lexeme lexbuf) }
  | int       { INT (Z.of_string (Lexing.lexeme lexbuf)) }
  | immediate { IMMEDIATE (int_of_string (lexeme_1 lexbuf)) }
  | '"'       { read_string (Buffer.create 32) lexbuf }
  | ':' ident { LABEL (lexeme_1 lexbuf) }
  | ".data"   { DATA }
  | "$("      { EXPR }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { TIMES }
  | '/'       { SLASH }
  | _         { failwith ("unexepected character " ^ Lexing.lexeme lexbuf) }
  | eof       { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '0'  { Buffer.add_char buf '\x00'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | "\\x" digit digit
    {
      let i = int_of_string ("0" ^ lexeme_1 lexbuf) in
      Buffer.add_char buf (Char.chr i);
      read_string buf lexbuf
    }
  | [^ '"' '\\']+
    {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { failwith ("String is not terminated") }
