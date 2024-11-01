type ir = Op of Op.t | Data of string | Label of string

let to_ir ast =
  let rec loop ir = function
    | [] -> List.rev ir
    | Ast.Label l :: ast' -> loop (Label l :: ir) ast'
    | Ast.Inst (i, a) :: ast' -> loop (Op (Op.of_inst i a) :: ir) ast'
    | Ast.Data d :: ast' -> loop (Data d :: ir) ast'
  in
  loop [] ast

let emit ast =
  let sizing_pass size ir =
    let rec loop refs pos = function
      | [] -> (refs, pos)
      | Label l :: ir' ->
          let refs' = Expr.Refs.add l pos refs in
          loop refs' pos ir'
      | Data d :: ir' ->
          let pos' = pos + String.length d in
          loop refs pos' ir'
      | Op o :: ir' ->
          let pos' = pos + size o in
          loop refs pos' ir'
    in
    loop Expr.Refs.empty 0 ir
  in
  let opt ir =
    let rec loop (refs, size) =
      let refs', size' = sizing_pass (Op.size refs) ir in
      let converged = size = size' && Expr.Refs.equal Int.equal refs refs' in
      if converged then (refs, size) else loop (refs', size')
    in
    sizing_pass Op.min_size ir |> loop
  in
  let ir = to_ir ast in
  let refs, size = opt ir in
  let rec write_buf buf refs = function
    | [] -> Buffer.contents buf
    | Label _ :: ir -> write_buf buf refs ir
    | Op o :: ir ->
        Op.write_buf buf refs o;
        write_buf buf refs ir
    | Data d :: ir ->
        Buffer.add_string buf d;
        write_buf buf refs ir
  in
  write_buf (Buffer.create size) refs ir
