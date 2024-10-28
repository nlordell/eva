let parse s =
  let lexbuf = Lexing.from_string s in
  Eva.Parser.main Eva.Lexer.read lexbuf

let ast =
  let open Eva.Ast in
  let module E = struct
    let rec pp fmt = function
      | Value v -> Format.fprintf fmt "(Value %a)" Z.pp_print v
      | Ref r -> Format.fprintf fmt "(Ref %s)" r
      | Neg e -> Format.fprintf fmt "(Neg %a)" pp e
      | Add (e, f) -> Format.fprintf fmt "(Add %a %a)" pp e pp f
      | Sub (e, f) -> Format.fprintf fmt "(Sub %a %a)" pp e pp f
      | Mul (e, f) -> Format.fprintf fmt "(Mul %a %a)" pp e pp f
      | Div (e, f) -> Format.fprintf fmt "(Div %a %a)" pp e pp f

    let rec eq e f =
      match (e, f) with
      | Value v, Value w -> Z.equal v w
      | Ref r, Ref s -> String.equal r s
      | Neg e, Neg f -> eq e f
      | Add (e, f), Add (g, h) -> eq e g && eq f h
      | Sub (e, f), Sub (g, h) -> eq e g && eq f h
      | Mul (e, f), Mul (g, h) -> eq e g && eq f h
      | Div (e, f), Div (g, h) -> eq e g && eq f h
      | _ -> false
  end in
  let module A = struct
    let pp fmt = function
      | Immediate i -> Format.fprintf fmt "(Immediate %d)" i
      | Expr e -> Format.fprintf fmt "(Expr %a)" E.pp e

    let eq a b =
      match (a, b) with
      | Immediate i, Immediate j -> Int.equal i j
      | Expr e, Expr f -> E.eq e f
      | _ -> false
  end in
  let module C = struct
    let args =
      let arg = Alcotest.testable A.pp A.eq in
      Alcotest.list arg

    let pp_args = Alcotest.pp args
    let eq_args = Alcotest.equal args

    let pp fmt = function
      | Label l -> Format.fprintf fmt "(Label %s)" l
      | Inst (i, a) -> Format.fprintf fmt "(Inst %s %a)" i pp_args a
      | Data s -> Format.fprintf fmt "(Data %S)" s

    let eq c d =
      match (c, d) with
      | Label l, Label m -> String.equal l m
      | Inst (i, a), Inst (j, b) -> String.equal i j && eq_args a b
      | Data s, Data t -> String.equal s t
      | _ -> false
  end in
  let code = Alcotest.testable C.pp C.eq in
  Alcotest.list code

let test_parse () =
  Alcotest.(check ast)
    "equals"
    (parse
       {|
        :init
          push 0
          push $(runtime - end)
          dup %1
          push $(runtime)
          push 0
          calldatacopy
          return
        :runtime
          push 0
          push 42
          mstore
          push 0
          push 32
          return
        :end
          .data "some metadata \x01\x02\x03\xff"
       |})
    Eva.Ast.
      [
        Label "init";
        Inst ("push", [ Expr (Value Z.zero) ]);
        Inst ("push", [ Expr (Sub (Ref "runtime", Ref "end")) ]);
        Inst ("dup", [ Immediate 1 ]);
        Inst ("push", [ Expr (Ref "runtime") ]);
        Inst ("push", [ Expr (Value Z.zero) ]);
        Inst ("calldatacopy", []);
        Inst ("return", []);
        Label "runtime";
        Inst ("push", [ Expr (Value Z.zero) ]);
        Inst ("push", [ Expr (Value Z.(~$42)) ]);
        Inst ("mstore", []);
        Inst ("push", [ Expr (Value Z.zero) ]);
        Inst ("push", [ Expr (Value Z.(~$32)) ]);
        Inst ("return", []);
        Label "end";
        Data "some metadata \001\002\003\255";
      ]

let test_expr () =
  Alcotest.(check ast)
    "equals"
    (parse {|
      push %32 $(x + 0x1 * (a + b * 0b11) / (-y - -1))
    |})
    Eva.Ast.
      [
        Inst
          ( "push",
            [
              Immediate 32;
              Expr
                (Add
                   ( Ref "x",
                     Div
                       ( Mul
                           ( Value Z.one,
                             Add (Ref "a", Mul (Ref "b", Value Z.(~$3))) ),
                         Sub (Neg (Ref "y"), Neg (Value Z.one)) ) ));
            ] );
      ]

let tests =
  Alcotest.
    [
      test_case "Parse" `Quick test_parse;
      test_case "Complex expression" `Quick test_expr;
    ]
