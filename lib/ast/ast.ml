(* Bring BinaryOp and UnaryOp into scope *)
module BinaryOp = Operator.BinaryOp
module UnaryOp = Operator.UnaryOp

(* Bring Types into scope *)
module Ty = Types

type literal =
  | Uint of int64
  | Int of int64
  | Float of float
  | String of string
  | Char of char

module Pattern = struct
  type t =
    | Wildcard
    | Literal of literal
    | Variable of string
    | Constructor of string * t list
    | Tuple of t list
    | List of t list
    | Or of t * t
    | And of t * t

  let rec to_string (pat: t) : string =
    match pat with
    | Wildcard -> "_"
    | Literal lit -> (
        match lit with
        | Uint v -> Printf.sprintf "%Lu" v
        | Int v -> Printf.sprintf "%Ld" v
        | Float v -> Printf.sprintf "%f" v
        | String v -> Printf.sprintf "\"%s\"" v
        | Char v -> Printf.sprintf "'%c'" v
      )
    | Variable name -> name
    | Constructor (name, args) ->
        let args_str = String.concat ", " (List.map to_string args) in
        Printf.sprintf "%s(%s)" name args_str
    | Tuple elems ->
        let elems_str = String.concat ", " (List.map to_string elems) in
        Printf.sprintf "(%s)" elems_str
    | List elems ->
        let elems_str = String.concat ", " (List.map to_string elems) in
        Printf.sprintf "[%s]" elems_str
    | Or (p1, p2) ->
        Printf.sprintf "(%s | %s)" (to_string p1) (to_string p2)
    | And (p1, p2) ->
        Printf.sprintf "(%s & %s)" (to_string p1) (to_string p2)
end

module Expr = struct
  type t =
    | Literal of literal
    | Variable of string
    | BinaryOp of { left: t; op: BinaryOp.t; right: t }
    | UnaryOp of { op: UnaryOp.t; expr: t }
    | Assign of { target: t; value: t }
    | Constructor of { name: string; args: t list }
    | Call of { fn: t; args: t list }
    | Index of { target: t; index: t }
    | Match of t * (Pattern.t * t) list
    | Restrict of { expr: t; source: Ty.region_id; target: Ty.region_id }
    | Extend of { expr: t; source: Ty.region_id; target: Ty.region_id }

  let literal_to_string (lit: literal) : string =
    match lit with
    | Uint v -> Printf.sprintf "%Lu" v
    | Int v -> Printf.sprintf "%Ld" v
    | Float v -> Printf.sprintf "%f" v
    | String v -> Printf.sprintf "\"%s\"" v
    | Char v -> Printf.sprintf "'%c'" v

  let rec to_string (expr: t) : string =
    match expr with
    | Literal lit -> literal_to_string lit
    | Variable name -> name
    | BinaryOp { left; op; right } ->
        Printf.sprintf "(%s %s %s)" (to_string left) (BinaryOp.to_string op) (to_string right)
    | UnaryOp { op; expr } ->
        Printf.sprintf "(%s%s)" (UnaryOp.to_string op) (to_string expr)
    | Assign { target; value } ->
        Printf.sprintf "(%s = %s)" (to_string target) (to_string value)
    | Constructor { name; args } ->
        let args_str = String.concat ", " (List.map to_string args) in
        Printf.sprintf "%s(%s)" name args_str
    | Call { fn; args } ->
        let args_str = String.concat ", " (List.map to_string args) in
        Printf.sprintf "%s(%s)" (to_string fn) args_str
    | Index { target; index } ->
        Printf.sprintf "%s[%s]" (to_string target) (to_string index)
    | Match (expr, branches) ->
        let branches_str =
          branches |> List.map (fun (pat, branch_expr) ->
                        Printf.sprintf "| %s => %s" (Pattern.to_string pat) 
                                                    (to_string branch_expr))
                   |>  String.concat " "
        in
        Printf.sprintf "match %s %s" (to_string expr) branches_str
    | Restrict { expr; source; target } ->
        Printf.sprintf "restrict %s : ~%s -> ~%s" (to_string expr) source target
    | Extend { expr; source; target } ->
        Printf.sprintf "extend %s : ~%s -> ~%s" (to_string expr) source target
end

module Stmt = struct
  type t =
    | Let of { name: string; ty: Ty.t option; value: Expr.t }
    | Expr of Expr.t
    | For of { var: string; iterable: Expr.t; body: t list }
    | While of { condition: Expr.t; body: t list }

  let rec to_string (stmt: t) : string =
    match stmt with
    | Let { name; ty; value } ->
        let ty_str = match ty with
          | Some ty -> Printf.sprintf " : %s" (Ty.to_string ty)
          | None -> ""
        in
        Printf.sprintf "let %s%s = %s;" name ty_str (Expr.to_string value)
    | Expr expr ->
        Printf.sprintf "%s;" (Expr.to_string expr)
    | For { var; iterable; body } ->
        let body_str = String.concat "\n" (List.map to_string body) in
        Printf.sprintf "for %s in %s {\n%s\n}" var (Expr.to_string iterable) body_str
    | While { condition; body } ->
        let body_str = String.concat "\n" (List.map to_string body) in
        Printf.sprintf "while (%s) {\n%s\n}" (Expr.to_string condition) body_str
end

type top_level =
  | Stmt of Stmt.t