let () =
  Alcotest.run "Eva"
    [
      ("Codegen", Test_codegen.tests);
      ("Intn", Test_intn.tests);
      ("Parser", Test_parser.tests);
    ]
