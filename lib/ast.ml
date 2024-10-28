type expr =
  | Value of Z.t
  | Ref of string
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

type arg = Immediate of int | Expr of expr
type stmt = Label of string | Inst of string * arg list | Data of string
type t = stmt list
