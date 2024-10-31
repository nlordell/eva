type t = int * Z.t

exception Integer_overflow

let of_z n z =
  let pow256 = Z.(shift_left one 256) in
  let mask = Z.(pow256 - one) in
  let norm z =
    if Z.(abs z > if sign z >= 0 then mask else pow256) then
      raise Integer_overflow;
    Z.(z land mask)
  in
  let z' = norm z in
  let size = (Z.numbits z' + 7) / 8 in
  let n =
    match n with
    | Some n ->
        if size > n then raise Integer_overflow;
        n
    | None ->
        assert (size <= 32);
        size
  in
  (n, z')

let size (n, _) = n

let to_bits (n, z) =
  let bits = Z.to_bits z in
  let len = String.length bits in
  String.init n (fun i ->
      let j = n - i - 1 in
      if j < len then bits.[j] else '\x00')
