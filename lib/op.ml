type t =
  | Stop
  | Add
  | Mul
  | Sub
  | Div
  | Sdiv
  | Mod
  | Smod
  | Addmod
  | Mulmod
  | Exp
  | Signextend
  | Lt
  | Gt
  | Slt
  | Sgt
  | Eq
  | Iszero
  | And
  | Or
  | Xor
  | Not
  | Byte
  | Shl
  | Shr
  | Sar
  | Keccak256
  | Address
  | Balance
  | Origin
  | Caller
  | Callvalue
  | Calldataload
  | Calldatasize
  | Calldatacopy
  | Codesize
  | Codecopy
  | Gasprice
  | Extcodesize
  | Extcodecopy
  | Returndatasize
  | Returndatacopy
  | Extcodehash
  | Blockhash
  | Coinbase
  | Timestamp
  | Number
  | Prevrandao
  | Gaslimit
  | Chainid
  | Selfbalance
  | Basefee
  | Blobhash
  | Blobbasefee
  | Pop
  | Mload
  | Mstore
  | Mstore8
  | Sload
  | Sstore
  | Jump
  | Jumpi
  | Pc
  | Msize
  | Gas
  | Jumpdest
  | Tload
  | Tstore
  | Mcopy
  | Push of int * Z.t
  | Dup of int
  | Swap of int
  | Log of int
  | Create
  | Call
  | Callcode
  | Return
  | Delegatecall
  | Create2
  | Staticcall
  | Revert
  | Invalid
  | Selfdestruct

let code = function
  | Stop -> '\x00'
  | Add -> '\x01'
  | Mul -> '\x02'
  | Sub -> '\x03'
  | Div -> '\x04'
  | Sdiv -> '\x05'
  | Mod -> '\x06'
  | Smod -> '\x07'
  | Addmod -> '\x08'
  | Mulmod -> '\x09'
  | Exp -> '\x0a'
  | Signextend -> '\x0b'
  | Lt -> '\x10'
  | Gt -> '\x11'
  | Slt -> '\x12'
  | Sgt -> '\x13'
  | Eq -> '\x14'
  | Iszero -> '\x15'
  | And -> '\x16'
  | Or -> '\x17'
  | Xor -> '\x18'
  | Not -> '\x19'
  | Byte -> '\x1a'
  | Shl -> '\x1b'
  | Shr -> '\x1c'
  | Sar -> '\x1d'
  | Keccak256 -> '\x20'
  | Address -> '\x30'
  | Balance -> '\x31'
  | Origin -> '\x32'
  | Caller -> '\x33'
  | Callvalue -> '\x34'
  | Calldataload -> '\x35'
  | Calldatasize -> '\x36'
  | Calldatacopy -> '\x37'
  | Codesize -> '\x38'
  | Codecopy -> '\x39'
  | Gasprice -> '\x3a'
  | Extcodesize -> '\x3b'
  | Extcodecopy -> '\x3c'
  | Returndatasize -> '\x3d'
  | Returndatacopy -> '\x3e'
  | Extcodehash -> '\x3f'
  | Blockhash -> '\x40'
  | Coinbase -> '\x41'
  | Timestamp -> '\x42'
  | Number -> '\x43'
  | Prevrandao -> '\x44'
  | Gaslimit -> '\x45'
  | Chainid -> '\x46'
  | Selfbalance -> '\x47'
  | Basefee -> '\x48'
  | Blobhash -> '\x49'
  | Blobbasefee -> '\x4a'
  | Pop -> '\x50'
  | Mload -> '\x51'
  | Mstore -> '\x52'
  | Mstore8 -> '\x53'
  | Sload -> '\x54'
  | Sstore -> '\x55'
  | Jump -> '\x56'
  | Jumpi -> '\x57'
  | Pc -> '\x58'
  | Msize -> '\x59'
  | Gas -> '\x5a'
  | Jumpdest -> '\x5b'
  | Tload -> '\x5c'
  | Tstore -> '\x5d'
  | Mcopy -> '\x5e'
  | Push (n, _) when n >= 0 && n <= 32 -> 0x5f + n |> Char.chr
  | Push _ -> failwith "invalid 'push' arrity"
  | Dup n when n > 0 && n <= 16 -> 0x7f + n |> Char.chr
  | Dup _ -> failwith "invalid 'dup' arrity"
  | Swap n when n > 0 && n <= 16 -> 0x8f + n |> Char.chr
  | Swap _ -> failwith "invalid 'swap' arrity"
  | Log n when n > 0 && n <= 4 -> 0x9f + n |> Char.chr
  | Log _ -> failwith "invalid 'log' arrity"
  | Create -> '\xf0'
  | Call -> '\xf1'
  | Callcode -> '\xf2'
  | Return -> '\xf3'
  | Delegatecall -> '\xf4'
  | Create2 -> '\xf5'
  | Staticcall -> '\xfa'
  | Revert -> '\xfd'
  | Invalid -> '\xfe'
  | Selfdestruct -> '\xff'
