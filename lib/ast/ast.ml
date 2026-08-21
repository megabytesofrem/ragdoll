(* Bring Types into scope *)
module Ty = Types

(* Aliases for parsing *)
type unop = Operator.unop
type binop = Operator.binop

type literal =
  | Uint of int64
  | Int of int64
  | Float of float
  | String of string
  | Char of char

type pattern =
  | PWildcard
  | PLiteral of literal
  | PVariable of string
  | PConstructor of string * pattern list
  | PTuple of pattern list
  | PList of pattern list
  | POr of pattern * pattern
  | PAnd of pattern * pattern

type expr =
  | ELiteral of literal
  | EVariable of string
  | EBinaryOp of { left: expr; op: binop; right: expr }
  | EUnaryOp of { op: unop; expr: expr }
  | EAssign of { target: expr; value: expr }
  | EConstructor of { name: string; args: expr list }
  | ECall of { fn: expr; args: expr list }
  | EIndex of { target: expr; index: expr }
  | EMatch of expr * (pattern * expr) list
  | ERestrict of { expr: expr; source: Ty.region_id; target: Ty.region_id }
  | EExtend of { expr: expr; source: Ty.region_id; target: Ty.region_id }

type stmt =
  | SLet of { name: string; ty: Ty.t option; value: expr }
  | SExpr of expr
  | SFor of { var: string; iterable: expr; body: stmt list }
  | SWhile of { condition: expr; body: stmt list }

type top_level =
  | TopStmt of stmt

let literal_to_string (lit: literal) : string =
  match lit with
  | Uint v -> Printf.sprintf "%Lu" v
  | Int v -> Printf.sprintf "%Ld" v
  | Float v -> Printf.sprintf "%f" v
  | String v -> Printf.sprintf "\"%s\"" v
  | Char v -> Printf.sprintf "'%c'" v

let rec pattern_to_string (pat: pattern) : string =
  match pat with
  | PWildcard -> "_"
  | PLiteral lit -> literal_to_string lit
  | PVariable name -> name
  | PConstructor (name, args) ->
      let args_str = String.concat ", " (List.map pattern_to_string args) in
      Printf.sprintf "%s(%s)" name args_str
  | PTuple elems ->
      let elems_str = String.concat ", " (List.map pattern_to_string elems) in
      Printf.sprintf "(%s)" elems_str
  | PList elems ->
      let elems_str = String.concat ", " (List.map pattern_to_string elems) in
      Printf.sprintf "[%s]" elems_str
  | POr (p1, p2) ->
      Printf.sprintf "(%s | %s)" (pattern_to_string p1) (pattern_to_string p2)
  | PAnd (p1, p2) ->
      Printf.sprintf "(%s & %s)" (pattern_to_string p1) (pattern_to_string p2)

let rec expr_to_string (node: expr) : string =
  match node with
  | ELiteral lit -> literal_to_string lit
  | EVariable name -> name
  | EBinaryOp { left; op; right } ->
      Printf.sprintf "(%s %s %s)" (expr_to_string left) (Operator.binop_to_string op) (expr_to_string right)
  | EUnaryOp { op; expr } ->
      Printf.sprintf "(%s%s)" (Operator.unop_to_string op) (expr_to_string expr)
  | EAssign { target; value } ->
      Printf.sprintf "(%s = %s)" (expr_to_string target) (expr_to_string value)
  | EConstructor { name; args } ->
      let args_str = String.concat ", " (List.map expr_to_string args) in
      Printf.sprintf "%s(%s)" name args_str
  | ECall { fn; args } ->
      let args_str = String.concat ", " (List.map expr_to_string args) in
      Printf.sprintf "%s(%s)" (expr_to_string fn) args_str
  | EIndex { target; index } ->
      Printf.sprintf "%s[%s]" (expr_to_string target) (expr_to_string index)
  | EMatch (expr, branches) ->
      let branches_str =
        branches |> List.map (fun (pat, branch_expr) ->
                      Printf.sprintf "| %s => %s" (pattern_to_string pat)
                                                  (expr_to_string branch_expr))
                 |>  String.concat " "
      in
      Printf.sprintf "match %s %s" (expr_to_string expr) branches_str
  | ERestrict { expr; source; target } ->
      Printf.sprintf "restrict %s : ~%s -> ~%s" (expr_to_string expr) source target
  | EExtend { expr; source; target } ->
      Printf.sprintf "extend %s : ~%s -> ~%s" (expr_to_string expr) source target

let rec stmt_to_string (node: stmt) : string =
  match node with
  | SLet { name; ty; value } ->
      let ty_str = match ty with
        | Some ty -> Printf.sprintf " : %s" (Ty.to_string ty)
        | None -> ""
      in
      Printf.sprintf "let %s%s = %s;" name ty_str (expr_to_string value)
  | SExpr expr ->
      Printf.sprintf "%s;" (expr_to_string expr)
  | SFor { var; iterable; body } ->
      let body_str = String.concat "\n" (List.map stmt_to_string body) in
      Printf.sprintf "for %s in %s {\n%s\n}" var (expr_to_string iterable) body_str
  | SWhile { condition; body } ->
      let body_str = String.concat "\n" (List.map stmt_to_string body) in
      Printf.sprintf "while (%s) {\n%s\n}" (expr_to_string condition) body_str