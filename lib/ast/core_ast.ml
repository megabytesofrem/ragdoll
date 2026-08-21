(* Core AST definitions for the Ragdoll language *)

(* Bring BinaryOp and UnaryOp into scope *)
module BinaryOp = Operator.BinaryOp
module UnaryOp = Operator.UnaryOp

(* Bring Types into scope *)
module Ty = Types

(** A debrujin index: the number of local binders between a variables use site,
    and its binding site, counting outwards starting from 0. *)
type debrujin = int

(* Modules prefixed by C belong to the core_ast, and are lowered during passes *)

module CVar = struct
  type t =
    | Local of debrujin
    | Global of string

  let to_string (var: t) : string =
    match var with
    | Local idx -> Printf.sprintf "Local(%d)" idx
    | Global name -> Printf.sprintf "Global(%s)" name
end

module CLiteral = struct
  type t =
    | Uint of int64
    | Int of int64
    | Float of float
    | String of string
    | Char of char

  let to_string (lit: t) : string =
    match lit with
    | Uint v -> Printf.sprintf "%Lu" v
    | Int v -> Printf.sprintf "%Ld" v
    | Float v -> Printf.sprintf "%f" v
    | String v -> Printf.sprintf "\"%s\"" v
    | Char v -> Printf.sprintf "'%c'" v
end

module CPattern = struct
  type t =
    | Wildcard
    | Literal of CLiteral.t
    | Variable of CVar.t
    | Constructor of string * t list
    | Tuple of t list
    | List of t list
    | Or of t * t
    | And of t * t

  let rec to_string (pat: t) : string =
    match pat with
    | Wildcard -> "_"
    | Literal lit -> CLiteral.to_string lit
    | Variable var -> CVar.to_string var
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

module CExpr = struct
  type t =
    | Literal of CLiteral.t
    | Var of CVar.t
    | BinaryOp of { op: BinaryOp.t; left: t; right: t }
    | UnaryOp of { op: UnaryOp.t; expr: t }
    | Assign of { target: t; value: t }
    | Constructor of { name: string; args: t list }
    | Call of { fn: t; args: t list }
    | Index of { target: t; index: t }
    | Match of t * (CPattern.t * t) list
    | Restrict of { expr: t; source: Ty.region_id; target: Ty.region_id }
    | Extend of { expr: t; source: Ty.region_id; target: Ty.region_id }

  let rec to_string (expr: t) : string =
    match expr with
    | Literal lit -> CLiteral.to_string lit
    | Var var -> CVar.to_string var
    | BinaryOp { op; left; right } ->
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
                        Printf.sprintf "| %s => %s" (CPattern.to_string pat) 
                                                    (to_string branch_expr))
                   |>  String.concat " "
        in
        Printf.sprintf "match %s %s" (to_string expr) branches_str
    | Restrict { expr; source; target } ->
        Printf.sprintf "restrict %s from %s to %s" (to_string expr) source target
    | Extend { expr; source; target } ->
        Printf.sprintf "extend %s from %s to %s" (to_string expr) source target
end