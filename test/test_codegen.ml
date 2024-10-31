let emit s =
  let lexbuf = Lexing.from_string s in
  Eva.Parser.main Eva.Lexer.read lexbuf |> Eva.Codegen.emit

let test_emit () = Alcotest.(check string) "equals" "\x00" (emit "stop")

let test_emit_opt () =
  Alcotest.(check string)
    "equals" "\x5f\x60\x04\xf3"
    (emit
       {|
        :init
          push $(init)
          push $(end)
          return
        :end
       |});
  Alcotest.(check string)
    "equals"
    "\x5f\x60\xff\xf3456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde"
    (emit
       {|
          :init
            push $(init)
            push $(end)
            return
            .data "456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcdef"
            .data "0123456789abcde"
          :end
         |})

let test_undefined_ref () =
  let undefined_reference s =
    try
      emit s |> ignore;
      None
    with
    | Eva.Expr.Undefined_reference r -> Some r
    | _ -> None
  in
  Alcotest.(check (option string))
    "equals" (Some "invalid_ref")
    (undefined_reference {|
      push $(invalid_ref)
      stop
    |})

let tests =
  Alcotest.
    [
      test_case "Emit" `Quick test_emit;
      test_case "Emit optimization" `Quick test_emit_opt;
      test_case "Overflow" `Quick test_undefined_ref;
    ]
