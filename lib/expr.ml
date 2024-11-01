module Refs = Map.Make (String)

exception Undefined_reference of string
exception Divide_by_zero

let rec eval refs =
  let open Ast in
  let find_ref refs r =
    match Refs.find_opt r refs with
    | Some x -> Z.of_int x
    | None -> raise (Undefined_reference r)
  in
  function
  | Value x -> x
  | Ref r -> find_ref refs r
  | Neg e ->
      let e = eval refs e in
      Z.neg e
  | Add (e, f) ->
      let e = eval refs e in
      let f = eval refs f in
      Z.add e f
  | Sub (e, f) ->
      let e = eval refs e in
      let f = eval refs f in
      Z.sub e f
  | Mul (e, f) ->
      let e = eval refs e in
      let f = eval refs f in
      Z.mul e f
  | Div (e, f) ->
      let e = eval refs e in
      let f = eval refs f in
      if Z.equal f Z.zero then raise Divide_by_zero;
      Z.div e f
