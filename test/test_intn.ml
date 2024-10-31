let intn =
  let pp fmt x =
    let bits = Eva.Intn.to_bits x in
    Format.fprintf fmt "0x";
    String.iter (fun c -> Char.code c |> Format.fprintf fmt "%02x") bits
  in
  let eq (n0, z0) (n1, z1) = Int.equal n0 n1 && Z.equal z0 z1 in
  Alcotest.testable pp eq

let test_unsized () =
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None Z.zero)
    (Eva.Intn.of_z (Some 0) Z.zero);
  Alcotest.(check intn)
    "equals" (Eva.Intn.of_z None Z.one)
    (Eva.Intn.of_z (Some 1) Z.one);
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None Z.(~$0x12345))
    (Eva.Intn.of_z (Some 3) (Z.of_string "0x012345"));
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None Z.(~-one))
    (Eva.Intn.of_z (Some 32)
       (Z.of_string
          "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"));
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None Z.(~- (~$256)))
    (Eva.Intn.of_z (Some 32)
       (Z.of_string
          "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00"));

  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None
       Z.(
         of_string
           "115792089237316195423570985008687907853269984665640564039457584007913129639935"))
    (Eva.Intn.of_z (Some 32)
       (Z.of_string
          "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"));
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None
       Z.(
         ~-(of_string
              "57896044618658097711785492504343953926634992332820282019728792003956564819968")))
    (Eva.Intn.of_z (Some 32)
       (Z.of_string
          "0x8000000000000000000000000000000000000000000000000000000000000000"));
  Alcotest.(check intn)
    "equals"
    (Eva.Intn.of_z None
       Z.(
         ~-(of_string
              "115792089237316195423570985008687907853269984665640564039457584007913129639936")))
    (Eva.Intn.of_z (Some 0) (Z.of_string "0x"))

let test_overflow () =
  let overflow n z =
    try
      Eva.Intn.of_z n z |> ignore;
      false
    with
    | Eva.Intn.Integer_overflow -> true
    | _ -> false
  in
  Alcotest.(check bool) "overflow" true (overflow (Some 0) Z.one);
  Alcotest.(check bool) "overflow" true (overflow (Some 1) Z.(~$256));
  Alcotest.(check bool)
    "overflow" true
    (overflow None
       Z.(
         of_string
           "115792089237316195423570985008687907853269984665640564039457584007913129639936"));
  Alcotest.(check bool)
    "overflow" true
    (overflow None
       Z.(
         ~-(of_string
              "115792089237316195423570985008687907853269984665640564039457584007913129639937")))

let tests =
  Alcotest.
    [
      test_case "Unsized" `Quick test_unsized;
      test_case "Overflow" `Quick test_unsized;
    ]
