type unop =
  | Negate

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual

let unop_to_string (op: unop) : string =
  match op with
  | Negate -> "-"

let binop_to_string (op: binop) : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | NotEqual -> "!="
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
