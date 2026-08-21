(*
 * Parser for the Ragdoll programming language.
 *)

open Result.Syntax

type parser_state = {
  tokens: Lexer.Token.t array;
  mutable position: int;
}

type precedence = int

type parse_error =
  | ExpectedIdent of { line: int; column: int }
  | ExpectedType of { line: int; column: int }

  | UnexpectedToken of {
      expected: Lexer.Token.token_kind;
      found: Lexer.Token.token_kind;
      line: int;
      column: int;
    }
  | UnexpectedTokens of {
      expected: Lexer.Token.token_kind list;
      found: Lexer.Token.token_kind;
      line: int;
      column: int;
    }
  | UnexpectedEndOfInput

type 'a parse_result = ('a, parse_error) result

let error_to_string (err: parse_error) : string =
  match err with
  | ExpectedIdent { line; column } ->
      Printf.sprintf "Expected identifier at line %d, column %d" line column
  | ExpectedType { line; column } ->
      Printf.sprintf "Expected type at line %d, column %d" line column
  | UnexpectedToken { expected; found; line; column } ->
      Printf.sprintf "Unexpected token at line %d, column %d: expected %s, found %s"
        line column
        (Lexer.tokenkind_to_string expected)
        (Lexer.tokenkind_to_string found)
  | UnexpectedTokens { expected; found; line; column } ->
      let expected_str = String.concat ", " (List.map Lexer.tokenkind_to_string expected) in
      Printf.sprintf "Unexpected token at line %d, column %d: expected one of [%s], found %s"
        line column
        expected_str
        (Lexer.tokenkind_to_string found)
  | UnexpectedEndOfInput ->
      "Unexpected end of input"

let precedence_for_token (token: Lexer.Token.token_kind) : precedence =
  let open Lexer.Token in

  match token with
  | Equal -> 10
  | LParen -> 100
  | Plus | Minus -> 40
  | Star | Slash | Percent -> 50
  | NotEqual | Less | LessEqual
  | Greater | GreaterEqual -> 50
  | _ -> 0

let next_higher (prec: precedence) : precedence =
  match prec with
  | 0 -> 10
  | 10 -> 20
  | 20 -> 40
  | 40 -> 50
  | _ -> prec + 10

let create_parser (tokens: Lexer.Token.t list) : parser_state =
  { tokens = Array.of_list tokens; position = 0 }

let peek (state: parser_state) : Lexer.Token.t option =
  if state.position < Array.length state.tokens then
    Some state.tokens.(state.position)
  else
    None

let next (state: parser_state) : Lexer.Token.t option =
  match peek state with
  | Some _ as tok ->
      state.position <- state.position + 1;
      tok
  | None -> None

let expect (state: parser_state) (expected_kind: Lexer.Token.token_kind) :
    Lexer.Token.t parse_result =
  match next state with
  | Some token when token.kind = expected_kind -> Ok token
  | Some token ->
      Error
        (UnexpectedToken
           { expected = expected_kind;
             found = token.kind;
             line = token.line;
             column = token.column })
  | None -> Error UnexpectedEndOfInput

(*************************************************
 * Expression parsing -- pratt precedence climbing 
 *************************************************)

let token_to_binop (token: Lexer.Token.token_kind) : Ast.binop option =
  let open Ast in
  let open Lexer.Token in

  match token with
  | Plus -> Some Add
  | Minus -> Some Sub
  | Star -> Some Mul
  | Slash -> Some Div
  | Percent -> Some Mod
  | Equal -> Some Equal
  | NotEqual -> Some NotEqual
  | Less -> Some Less
  | LessEqual -> Some LessEqual
  | Greater -> Some Greater
  | GreaterEqual -> Some GreaterEqual
  | _ -> None

let token_to_unop (token: Lexer.Token.token_kind) : Ast.unop option =
  let open Ast in
  let open Lexer.Token in
  
  match token with
  | Minus -> Some Negate
  | _ -> None

let parse_literal (state: parser_state) : Ast.literal parse_result =
  let open Lexer in

  let valid_tokens = [
    Token.Int 0L;
    Token.Float 0.0;
    Token.String "a string";
    Token.Char 'a';
  ] in
  match next state with
  | Some token -> (
      match token.kind with
      | Token.Int v    -> Ok (Ast.Int v)
      | Token.Float v  -> Ok (Ast.Float v)
      | Token.String v -> Ok (Ast.String v)
      | Token.Char v   -> Ok (Ast.Char v)
      | _ ->
          Error
            (UnexpectedTokens
               { expected = valid_tokens;
                 found = token.kind;
                 line = token.line;
                 column = token.column }))
  | None -> Error UnexpectedEndOfInput


(* Parse an expression, yielding a (Ast.expr, Parser.error) result pair *)
let rec parse_expr (state: parser_state) : Ast.expr parse_result =
  parse_expr_impl state 0

and parse_expr_impl (state: parser_state) (prec: precedence) : Ast.expr parse_result =
  let* lhs = parse_prefix state in
  let rec loop lhs =
    match peek state with
    | Some token when precedence_for_token token.kind > prec ->
        let* lhs' = parse_infix state lhs prec in
        loop lhs'
    | _ -> Ok lhs
  in
  loop lhs

and parse_unary (state: parser_state) : Ast.expr parse_result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error UnexpectedEndOfInput
  in
  let op = match token_to_unop token.kind with
    | Some op -> op
    | None -> failwith "Expected unary operator"
  in
  let* expr = parse_expr_impl state 80 in
  Ok (Ast.EUnaryOp { op; expr })

and parse_prefix (state: parser_state) : Ast.expr parse_result =
  let open Lexer in

  match peek state with
  | Some token -> (
      match token.kind with
      | Token.Int _ | Token.Float _ | Token.String _
      | Token.Char _ ->
          parse_value state
      | Token.Ident name ->
          let _ = next state in
          Ok (Ast.EVariable name)
      | Token.Minus ->
          parse_unary state
      | _ ->
          Error
            (UnexpectedToken
               { expected = token.kind;
                 found = token.kind;
                 line = token.line;
                 column = token.column }))
  | None -> Error UnexpectedEndOfInput

and parse_value (state: parser_state) : Ast.expr parse_result =
  let open Lexer in

  match peek state with
  | Some token -> (
      match token.kind with
      | Token.Int _ | Token.Float _ | Token.String _
      | Token.Char _ -> 
        let* lit = parse_literal state in
        Ok (Ast.ELiteral lit)
      | Token.Minus -> 
          parse_unary state
      | Token.Ident name ->
          let _ = next state in
          Ok (Ast.EVariable name)
      | Token.LParen ->
          let _ = next state in
          let* expr = parse_expr_impl state 0 in
          let* _ = expect state Token.RParen in
          Ok expr
      | Token.Restrict ->
          parse_restrict state
      | Token.Extend ->
          parse_extend state
      | _ -> failwith "parse_value not implemented for this token kind")
  | None -> Error UnexpectedEndOfInput

and parse_infix (state: parser_state) (lhs: Ast.expr) (prec: precedence) : Ast.expr parse_result =
  let open Lexer in
  
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error UnexpectedEndOfInput
  in
  match token.kind with
  | Token.LParen ->
      let* args = parse_argument_list state in
      Ok (Ast.ECall { fn = lhs; args })
  | Token.Equal ->
      let* rhs = parse_expr_impl state (next_higher prec) in
      Ok (Ast.EAssign { target = lhs; value = rhs })

  (* Parse binary operator *)
  | _ when token_to_binop token.kind <> None ->
      let op = match token_to_binop token.kind with
        | Some op -> op
        | None -> failwith "Expected binary operator"
      in
      let* rhs = parse_expr_impl state (next_higher prec) in
      Ok (Ast.EBinaryOp { left = lhs; op; right = rhs })

  (* Unexpected token *)
  | _ -> Error (UnexpectedToken { expected = token.kind; found = token.kind; line = token.line; column = token.column })

and parse_argument_list (state: parser_state) : (Ast.expr list) parse_result =
  let rec aux acc =
    match peek state with
    | Some token when token.kind = Lexer.Token.RParen ->
        let _ = next state in
        Ok (List.rev acc)
    | Some _ ->
        let* expr = parse_expr_impl state 0 in
        let acc = expr :: acc in
        (match peek state with
        | Some token when token.kind = Lexer.Token.Comma ->
            let _ = next state in
            aux acc
        | Some token when token.kind = Lexer.Token.RParen ->
            let _ = next state in
            Ok (List.rev acc)
        | Some token ->
            Error
              (UnexpectedToken
                 { expected = Lexer.Token.Comma;
                   found = token.kind;
                   line = token.line;
                   column = token.column })
        | None -> Error UnexpectedEndOfInput)
    | None -> Error UnexpectedEndOfInput
  in
  aux []

(* ************************************************* *)

and parse_ty (state: parser_state) : Ast.Ty.t parse_result =
  let open Lexer in
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error UnexpectedEndOfInput
  in
  match token.kind with
  | Token.Int _     -> Ok Ast.Ty.I32
  | Token.Float _   -> Ok Ast.Ty.F32
  | Token.String _  -> Ok Ast.Ty.String
  | Token.Char _    -> Ok Ast.Ty.Char
  | Token.Ident name -> Ok (Ast.Ty.Named name)
  | Token.Star -> 
    (* *T in u *)
    let* inner_ty = parse_ty state in
    let* _ = expect state Token.In in
    let* region_id = parse_region_id state in
    Ok (Ast.Ty.Pointer { target = inner_ty; region = region_id })
  | _ -> Error (ExpectedType { line = token.line; column = token.column })

and parse_ident (state: parser_state) : string parse_result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error UnexpectedEndOfInput
  in
  match token.kind with
  | Lexer.Token.Ident id -> Ok id
  | _ -> Error (ExpectedIdent { line = token.line; column = token.column })

and parse_region_id (state: parser_state) : string parse_result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error UnexpectedEndOfInput
  in
  match token.kind with
  | Lexer.Token.Ident id -> Ok id
  | _ -> Error (ExpectedIdent { line = token.line; column = token.column })

and parse_reference (state: parser_state) : Ast.expr parse_result =
  let* _ = expect state Lexer.Token.Ampersand in
  let* ident = parse_ident state in
  Ok (Ast.EVariable ("&" ^ ident))

and parse_arrow (state: parser_state) : (Ast.Ty.region_id * Ast.Ty.region_id) parse_result =
  (* ~a -> ~b *)
  let* _ = expect state Lexer.Token.Tilde in
  let* from_region = parse_region_id state in
  let* _ = expect state Lexer.Token.Arrow in
  let* _ = expect state Lexer.Token.Tilde in
  let* to_region = parse_region_id state in
  Ok (from_region, to_region)

and parse_restrict (state: parser_state) : Ast.expr parse_result =
  let* _ = expect state Lexer.Token.Restrict in
  let* from_exp = parse_expr_impl state 0 in

  (* ~a -> ~b *)
  let* arr = parse_arrow state in
  let (from_region, to_region) = arr in
  Ok (Ast.ERestrict { expr = from_exp; source = from_region; target = to_region })

and parse_extend (state: parser_state) : Ast.expr parse_result =
  let* _ = expect state Lexer.Token.Extend in
  let* from_exp = parse_expr_impl state 0 in

  (* ~a -> ~b *)
  let* arr = parse_arrow state in
  let (from_region, to_region) = arr in
  Ok (Ast.EExtend { expr = from_exp; source = from_region; target = to_region })

(* *************************************************
 * Statement parsing
 ************************************************** *)

and parse_block (state: parser_state) : Ast.stmt list parse_result =
  let* _ = expect state Lexer.Token.LBrace in
  let rec aux acc =
    match peek state with
    | Some token when token.kind = Lexer.Token.RBrace ->
        let _ = next state in
        Ok (List.rev acc)
    | Some _ ->
        let* stmt = parse_stmt state in
        aux (stmt :: acc)
    | None -> Error UnexpectedEndOfInput
  in
  aux []

and parse_let (state: parser_state) : Ast.stmt parse_result =
  let* _ = expect state Lexer.Token.Let in
  let* name = parse_ident state in
  let* ty = match peek state with
    | Some token when token.kind = Lexer.Token.Colon ->
        let _ = next state in
        let* ty = parse_ty state in
        Ok (Some ty)
    | _ -> Ok None
  in
  let* _ = expect state Lexer.Token.Equal in
  let* value = parse_expr_impl state 0 in
  Ok (Ast.SLet { name; ty; value })

and parse_for (state: parser_state) : Ast.stmt parse_result =
  (* for var in iterable { .. }*)
  let* _ = expect state Lexer.Token.For in
  let* var = parse_ident state in
  let* _ = expect state Lexer.Token.In in
  let* iterable = parse_expr_impl state 0 in
  let* _ = expect state Lexer.Token.LBrace in
  let* body = parse_block state in
  Ok (Ast.SFor { var; iterable; body })

and parse_while (state: parser_state) : Ast.stmt parse_result =
  (* while condition { .. } *)
  let* _ = expect state Lexer.Token.While in
  let* condition = parse_expr_impl state 0 in
  let* _ = expect state Lexer.Token.LBrace in
  let* body = parse_block state in
  Ok (Ast.SWhile { condition; body })

and parse_stmt (state: parser_state) : Ast.stmt parse_result =
  match peek state with
  | Some token -> (
      match token.kind with
      | Lexer.Token.Let -> parse_let state
      | Lexer.Token.For -> parse_for state
      | Lexer.Token.While -> parse_while state
      | _ ->
          (* Expression in statement place *)
          let* expr = parse_expr_impl state 0 in
          Ok (Ast.SExpr expr))
  | None -> Error (UnexpectedEndOfInput)