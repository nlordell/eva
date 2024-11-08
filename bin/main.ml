let main () =
  let version = ref false in
  let input = ref None in
  let output = ref None in
  let hex = ref false in

  Arg.parse
    [
      ("-v", Arg.Set version, "Print version information and exit");
      ("-o", Arg.String (fun s -> output := Some s), "Output file");
      ("-x", Arg.Set hex, "Output as hexadecimal instead of binary");
    ]
    (fun s ->
      input :=
        match !input with
        | None -> Some s
        | Some _ -> raise (Arg.Bad "Cannot specify more than one input file"))
    (Printf.sprintf "Usage: %s [OPTIONS] INPUT" Sys.argv.(0));

  if !version then (
    print_endline "Eva v0.1";
    exit 0);

  let input, input_name =
    match !input with Some s -> (open_in s, s) | None -> (stdin, "a")
  in
  let code =
    Fun.protect
      ~finally:(fun () -> close_in_noerr input)
      (fun () ->
        Lexing.from_channel input
        |> Eva.Parser.main Eva.Lexer.read
        |> Eva.Codegen.emit)
  in

  let output =
    match !output with
    | Some "-" -> stdout
    | Some s -> open_out s
    | None -> Filename.remove_extension input_name ^ ".bin" |> open_out
  in

  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
      if !hex || output == stdout then
        String.iter (fun c -> Printf.fprintf output "%02x" (Char.code c)) code
      else output_string output code;
      if output == stdout then output_char output '\n')

let () = main ()
