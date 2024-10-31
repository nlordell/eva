type ir = Op of Op.t | Data of string | Label of string

let to_ir ast =
  let rec to_ir' ir = function
    | [] -> List.rev ir
    | Ast.Label l :: ast' -> to_ir' (Label l :: ir) ast'
    | Ast.Inst (i, a) :: ast' -> to_ir' (Op (Op.of_inst i a) :: ir) ast'
    | Ast.Data d :: ast' -> to_ir' (Data d :: ir) ast'
  in
  to_ir' [] ast

let sizing_pass size ir =
  let rec build_refs' refs pos = function
    | [] -> (refs, pos)
    | Label l :: ir' ->
        let refs' = Expr.Refs.add l pos refs in
        build_refs' refs' pos ir'
    | Data d :: ir' ->
        let pos' = pos + String.length d in
        build_refs' refs pos' ir'
    | Op o :: ir' ->
        let pos' = pos + size o in
        build_refs' refs pos' ir'
  in
  build_refs' Expr.Refs.empty 0 ir

let emit ast =
  let ir = to_ir ast in
  let rec opt (refs, size) =
    let refs', size' = sizing_pass (Op.size refs) ir in
    if size' >= size then (refs, size) else opt (refs', size')
  in
  let refs, size = sizing_pass Op.max_size ir |> opt in
  let rec write_buf buf = function
    | [] -> Buffer.contents buf
    | Label _ :: ir -> write_buf buf ir
    | Op o :: ir ->
        Op.write_buf buf refs o;
        write_buf buf ir
    | Data d :: ir ->
        Buffer.add_string buf d;
        write_buf buf ir
  in
  write_buf (Buffer.create size) ir
