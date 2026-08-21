(* Core AST definitions for the Ragdoll language *)

(* Bring Types into scope *)
module Ty = Types

(** A debrujin index: the number of local binders between a variables use site,
    and its binding site, counting outwards starting from 0. *)
type debrujin = int

type cvar =
  | Local of debrujin
  | Global of string

type cliteral =
  | Uint of int64
  | Int of int64
  | Float of float
  | String of string
  | Char of char

type cpattern =
  | PWildcard
  | PLiteral of cliteral
  | PVariable of cvar
  | PConstructor of string * cpattern list
  | PTuple of cpattern list
  | PList of cpattern list
  | POr of cpattern * cpattern
  | PAnd of cpattern * cpattern

type cexpr =
  | ELiteral of cliteral
  | EVar of cvar
  | EBinaryOp of { op: Operator.binop; left: cexpr; right: cexpr }
  | EUnaryOp of { op: Operator.unop; expr: cexpr }
  | EAssign of { target: cexpr; value: cexpr }
  | EConstructor of { name: string; args: cexpr list }
  | ECall of { fn: cexpr; args: cexpr list }
  | EIndex of { target: cexpr; index: cexpr }
  | EMatch of cexpr * (cpattern * cexpr) list
  | ERestrict of { expr: cexpr; source: Ty.region_id; target: Ty.region_id }
  | EExtend of { expr: cexpr; source: Ty.region_id; target: Ty.region_id }

let cvar_to_string (var: cvar) : string =
  match var with
  | Local idx -> Printf.sprintf "Local(%d)" idx
  | Global name -> Printf.sprintf "Global(%s)" name

let cliteral_to_string (lit: cliteral) : string =
  match lit with
  | Uint v -> Printf.sprintf "%Lu" v
  | Int v -> Printf.sprintf "%Ld" v
  | Float v -> Printf.sprintf "%f" v
  | String v -> Printf.sprintf "\"%s\"" v
  | Char v -> Printf.sprintf "'%c'" v

let rec cpattern_to_string (pat: cpattern) : string =
  match pat with
  | PWildcard -> "_"
  | PLiteral lit -> cliteral_to_string lit
  | PVariable var -> cvar_to_string var
  | PConstructor (name, args) ->
      let args_str = String.concat ", " (List.map cpattern_to_string args) in
      Printf.sprintf "%s(%s)" name args_str
  | PTuple elems ->
      let elems_str = String.concat ", " (List.map cpattern_to_string elems) in
      Printf.sprintf "(%s)" elems_str
  | PList elems ->
      let elems_str = String.concat ", " (List.map cpattern_to_string elems) in
      Printf.sprintf "[%s]" elems_str
  | POr (p1, p2) ->
      Printf.sprintf "(%s | %s)" (cpattern_to_string p1) (cpattern_to_string p2)
  | PAnd (p1, p2) ->
      Printf.sprintf "(%s & %s)" (cpattern_to_string p1) (cpattern_to_string p2)

let rec cexpr_to_string (node: cexpr) : string =
  match node with
  | ELiteral lit -> cliteral_to_string lit
  | EVar var -> cvar_to_string var
  | EBinaryOp { op; left; right } ->
      Printf.sprintf "(%s %s %s)" (cexpr_to_string left) (Operator.binop_to_string op) (cexpr_to_string right)
  | EUnaryOp { op; expr } ->
      Printf.sprintf "(%s%s)" (Operator.unop_to_string op) (cexpr_to_string expr)
  | EAssign { target; value } ->
      Printf.sprintf "(%s = %s)" (cexpr_to_string target) (cexpr_to_string value)
  | EConstructor { name; args } ->
      let args_str = String.concat ", " (List.map cexpr_to_string args) in
      Printf.sprintf "%s(%s)" name args_str
  | ECall { fn; args } ->
      let args_str = String.concat ", " (List.map cexpr_to_string args) in
      Printf.sprintf "%s(%s)" (cexpr_to_string fn) args_str
  | EIndex { target; index } ->
      Printf.sprintf "%s[%s]" (cexpr_to_string target) (cexpr_to_string index)
  | EMatch (expr, branches) ->
      let branches_str =
        branches |> List.map (fun (pat, branch_expr) ->
                      Printf.sprintf "| %s => %s" (cpattern_to_string pat)
                                                  (cexpr_to_string branch_expr))
                 |>  String.concat " "
      in
      Printf.sprintf "match %s %s" (cexpr_to_string expr) branches_str
  | ERestrict { expr; source; target } ->
      Printf.sprintf "restrict %s from %s to %s" (cexpr_to_string expr) source target
  | EExtend { expr; source; target } ->
      Printf.sprintf "extend %s from %s to %s" (cexpr_to_string expr) source target