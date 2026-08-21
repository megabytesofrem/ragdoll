(** Region IDs correspond to sections over a presheaf.*)
type region_id = string

type t =
  | U1 | U8 | U16 | U32 | U64
  | I8 | I16 | I32 | I64
  | F32 | F64
  | String
  | Char
  | Unit
  | Named of string
  | Pointer of { target: t; region: region_id }

let rec to_string (ty: t) : string =
  match ty with
  | U1 -> "u1"
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | F32 -> "f32"
  | F64 -> "f64"
  | String -> "string"
  | Char -> "char"
  | Unit -> "unit"
  | Named name -> name
  | Pointer { target; region } ->
      Printf.sprintf "*%s ~%s" (to_string target) region