type t = Code of char | Push of int option * Ast.expr

let of_inst i a =
  match (i, a) with
  | "stop", [] -> Code '\x00'
  | "add", [] -> Code '\x01'
  | "mul", [] -> Code '\x02'
  | "sub", [] -> Code '\x03'
  | "div", [] -> Code '\x04'
  | "sdiv", [] -> Code '\x05'
  | "mod", [] -> Code '\x06'
  | "smod", [] -> Code '\x07'
  | "addmod", [] -> Code '\x08'
  | "mulmod", [] -> Code '\x09'
  | "exp", [] -> Code '\x0a'
  | "signextend", [] -> Code '\x0b'
  | "lt", [] -> Code '\x10'
  | "gt", [] -> Code '\x11'
  | "slt", [] -> Code '\x12'
  | "sgt", [] -> Code '\x13'
  | "eq", [] -> Code '\x14'
  | "iszero", [] -> Code '\x15'
  | "and", [] -> Code '\x16'
  | "or", [] -> Code '\x17'
  | "xor", [] -> Code '\x18'
  | "not", [] -> Code '\x19'
  | "byte", [] -> Code '\x1a'
  | "shl", [] -> Code '\x1b'
  | "shr", [] -> Code '\x1c'
  | "sar", [] -> Code '\x1d'
  | "keccak256", [] -> Code '\x20'
  | "address", [] -> Code '\x30'
  | "balance", [] -> Code '\x31'
  | "origin", [] -> Code '\x32'
  | "caller", [] -> Code '\x33'
  | "callvalue", [] -> Code '\x34'
  | "calldataload", [] -> Code '\x35'
  | "calldatasize", [] -> Code '\x36'
  | "calldatacopy", [] -> Code '\x37'
  | "codesize", [] -> Code '\x38'
  | "codecopy", [] -> Code '\x39'
  | "gasprice", [] -> Code '\x3a'
  | "extcodesize", [] -> Code '\x3b'
  | "extcodecopy", [] -> Code '\x3c'
  | "returndatasize", [] -> Code '\x3d'
  | "returndatacopy", [] -> Code '\x3e'
  | "extcodehash", [] -> Code '\x3f'
  | "blockhash", [] -> Code '\x40'
  | "coinbase", [] -> Code '\x41'
  | "timestamp", [] -> Code '\x42'
  | "number", [] -> Code '\x43'
  | "prevrandao", [] -> Code '\x44'
  | "gaslimit", [] -> Code '\x45'
  | "chainid", [] -> Code '\x46'
  | "selfbalance", [] -> Code '\x47'
  | "basefee", [] -> Code '\x48'
  | "blobhash", [] -> Code '\x49'
  | "blobbasefee", [] -> Code '\x4a'
  | "pop", [] -> Code '\x50'
  | "mload", [] -> Code '\x51'
  | "mstore", [] -> Code '\x52'
  | "mstore8", [] -> Code '\x53'
  | "sload", [] -> Code '\x54'
  | "sstore", [] -> Code '\x55'
  | "jump", [] -> Code '\x56'
  | "jumpi", [] -> Code '\x57'
  | "pc", [] -> Code '\x58'
  | "msize", [] -> Code '\x59'
  | "gas", [] -> Code '\x5a'
  | "jumpdest", [] -> Code '\x5b'
  | "tload", [] -> Code '\x5c'
  | "tstore", [] -> Code '\x5d'
  | "mcopy", [] -> Code '\x5e'
  | "push", Ast.[ Expr e ] -> Push (None, e)
  | "push", Ast.[ Immediate 0 ] -> Push (Some 0, Value Z.zero)
  | "push", Ast.[ Immediate 0; Expr e ] -> Push (Some 0, e)
  | "push", Ast.[ Immediate 1; Expr e ] -> Push (Some 1, e)
  | "push", Ast.[ Immediate 2; Expr e ] -> Push (Some 2, e)
  | "push", Ast.[ Immediate 3; Expr e ] -> Push (Some 3, e)
  | "push", Ast.[ Immediate 4; Expr e ] -> Push (Some 4, e)
  | "push", Ast.[ Immediate 5; Expr e ] -> Push (Some 5, e)
  | "push", Ast.[ Immediate 6; Expr e ] -> Push (Some 6, e)
  | "push", Ast.[ Immediate 7; Expr e ] -> Push (Some 7, e)
  | "push", Ast.[ Immediate 8; Expr e ] -> Push (Some 8, e)
  | "push", Ast.[ Immediate 9; Expr e ] -> Push (Some 9, e)
  | "push", Ast.[ Immediate 10; Expr e ] -> Push (Some 10, e)
  | "push", Ast.[ Immediate 11; Expr e ] -> Push (Some 11, e)
  | "push", Ast.[ Immediate 12; Expr e ] -> Push (Some 12, e)
  | "push", Ast.[ Immediate 13; Expr e ] -> Push (Some 13, e)
  | "push", Ast.[ Immediate 14; Expr e ] -> Push (Some 14, e)
  | "push", Ast.[ Immediate 15; Expr e ] -> Push (Some 15, e)
  | "push", Ast.[ Immediate 16; Expr e ] -> Push (Some 16, e)
  | "push", Ast.[ Immediate 17; Expr e ] -> Push (Some 17, e)
  | "push", Ast.[ Immediate 18; Expr e ] -> Push (Some 18, e)
  | "push", Ast.[ Immediate 19; Expr e ] -> Push (Some 19, e)
  | "push", Ast.[ Immediate 20; Expr e ] -> Push (Some 20, e)
  | "push", Ast.[ Immediate 21; Expr e ] -> Push (Some 21, e)
  | "push", Ast.[ Immediate 22; Expr e ] -> Push (Some 22, e)
  | "push", Ast.[ Immediate 23; Expr e ] -> Push (Some 23, e)
  | "push", Ast.[ Immediate 24; Expr e ] -> Push (Some 24, e)
  | "push", Ast.[ Immediate 25; Expr e ] -> Push (Some 25, e)
  | "push", Ast.[ Immediate 26; Expr e ] -> Push (Some 26, e)
  | "push", Ast.[ Immediate 27; Expr e ] -> Push (Some 27, e)
  | "push", Ast.[ Immediate 28; Expr e ] -> Push (Some 28, e)
  | "push", Ast.[ Immediate 29; Expr e ] -> Push (Some 29, e)
  | "push", Ast.[ Immediate 30; Expr e ] -> Push (Some 30, e)
  | "push", Ast.[ Immediate 31; Expr e ] -> Push (Some 31, e)
  | "push", Ast.[ Immediate 32; Expr e ] -> Push (Some 32, e)
  | "dup", Ast.[ Immediate 1 ] -> Code '\x80'
  | "dup", Ast.[ Immediate 2 ] -> Code '\x81'
  | "dup", Ast.[ Immediate 3 ] -> Code '\x82'
  | "dup", Ast.[ Immediate 4 ] -> Code '\x83'
  | "dup", Ast.[ Immediate 5 ] -> Code '\x84'
  | "dup", Ast.[ Immediate 6 ] -> Code '\x85'
  | "dup", Ast.[ Immediate 7 ] -> Code '\x86'
  | "dup", Ast.[ Immediate 8 ] -> Code '\x87'
  | "dup", Ast.[ Immediate 9 ] -> Code '\x88'
  | "dup", Ast.[ Immediate 10 ] -> Code '\x89'
  | "dup", Ast.[ Immediate 11 ] -> Code '\x8a'
  | "dup", Ast.[ Immediate 12 ] -> Code '\x8b'
  | "dup", Ast.[ Immediate 13 ] -> Code '\x8c'
  | "dup", Ast.[ Immediate 14 ] -> Code '\x8d'
  | "dup", Ast.[ Immediate 15 ] -> Code '\x8e'
  | "dup", Ast.[ Immediate 16 ] -> Code '\x8f'
  | "swap", Ast.[ Immediate 1 ] -> Code '\x90'
  | "swap", Ast.[ Immediate 2 ] -> Code '\x91'
  | "swap", Ast.[ Immediate 3 ] -> Code '\x92'
  | "swap", Ast.[ Immediate 4 ] -> Code '\x93'
  | "swap", Ast.[ Immediate 5 ] -> Code '\x94'
  | "swap", Ast.[ Immediate 6 ] -> Code '\x95'
  | "swap", Ast.[ Immediate 7 ] -> Code '\x96'
  | "swap", Ast.[ Immediate 8 ] -> Code '\x97'
  | "swap", Ast.[ Immediate 9 ] -> Code '\x98'
  | "swap", Ast.[ Immediate 10 ] -> Code '\x99'
  | "swap", Ast.[ Immediate 11 ] -> Code '\x9a'
  | "swap", Ast.[ Immediate 12 ] -> Code '\x9b'
  | "swap", Ast.[ Immediate 13 ] -> Code '\x9c'
  | "swap", Ast.[ Immediate 14 ] -> Code '\x9d'
  | "swap", Ast.[ Immediate 15 ] -> Code '\x9e'
  | "swap", Ast.[ Immediate 16 ] -> Code '\x9f'
  | "log", Ast.[ Immediate 0 ] -> Code '\xa0'
  | "log", Ast.[ Immediate 1 ] -> Code '\xa1'
  | "log", Ast.[ Immediate 2 ] -> Code '\xa2'
  | "log", Ast.[ Immediate 3 ] -> Code '\xa3'
  | "log", Ast.[ Immediate 4 ] -> Code '\xa4'
  | "create", [] -> Code '\xf0'
  | "call", [] -> Code '\xf1'
  | "callcode", [] -> Code '\xf2'
  | "return", [] -> Code '\xf3'
  | "delegatecall", [] -> Code '\xf4'
  | "create2", [] -> Code '\xf5'
  | "staticcall", [] -> Code '\xfa'
  | "revert", [] -> Code '\xfd'
  | "invalid", [] -> Code '\xfe'
  | "selfdestruct", [] -> Code '\xff'
  | _ -> Printf.sprintf "invalid instruction %s" i |> failwith

let max_size = function
  | Code _ -> 1
  | Push (n, _) -> 1 + Option.value n ~default:32

let size refs = function
  | Code _ -> 1
  | Push (n, e) ->
      let x = Expr.eval refs e |> Intn.of_z n in
      1 + Intn.size x

let write_buf buf refs = function
  | Code c -> Buffer.add_char buf c
  | Push (n, e) ->
      let x = Expr.eval refs e |> Intn.of_z n in
      Buffer.add_char buf (Char.chr (0x5f + Intn.size x));
      Buffer.add_string buf (Intn.to_bits x)
