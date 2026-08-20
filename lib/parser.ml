(*
 * Parser for the Ragdoll programming language.
 *)

open Result.Syntax

module Parser = struct
  type state = {
    tokens: Lexer.Token.t array;
    mutable position: int;
  }

  type prec = int

  type error =
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
end

let error_to_string (err: Parser.error) : string =
  match err with
  | Parser.ExpectedIdent { line; column } ->
      Printf.sprintf "Expected identifier at line %d, column %d" line column
  | Parser.ExpectedType { line; column } ->
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

let precedence_for_token (token: Lexer.Token.token_kind) : Parser.prec =
  match token with
  | Lexer.Token.Equal -> 10
  | Lexer.Token.LParen -> 100
  | Lexer.Token.Plus | Lexer.Token.Minus -> 40
  | Lexer.Token.Star | Lexer.Token.Slash | Lexer.Token.Percent -> 50
  | Lexer.Token.NotEqual | Lexer.Token.Less | Lexer.Token.LessEqual
  | Lexer.Token.Greater | Lexer.Token.GreaterEqual -> 50
  | _ -> 0

let next_higher (prec: Parser.prec) : Parser.prec =
  match prec with
  | 0 -> 10
  | 10 -> 20
  | 20 -> 40
  | 40 -> 50
  | _ -> prec + 10

let create_parser (tokens: Lexer.Token.t list) : Parser.state =
  { tokens = Array.of_list tokens; position = 0 }

let peek (state: Parser.state) : Lexer.Token.t option =
  if state.position < Array.length state.tokens then
    Some state.tokens.(state.position)
  else
    None

let next (state: Parser.state) : Lexer.Token.t option =
  match peek state with
  | Some _ as tok ->
      state.position <- state.position + 1;
      tok
  | None -> None

let expect (state: Parser.state) (expected_kind: Lexer.Token.token_kind) :
    (Lexer.Token.t, Parser.error) result =
  match next state with
  | Some token when token.kind = expected_kind -> Ok token
  | Some token ->
      Error
        (Parser.UnexpectedToken
           { expected = expected_kind;
             found = token.kind;
             line = token.line;
             column = token.column })
  | None -> Error Parser.UnexpectedEndOfInput

(*************************************************
 * Expression parsing -- pratt precedence climbing 
 *************************************************)

let token_to_binop (token: Lexer.Token.token_kind) : Ast.BinaryOp.t option =
  let open Ast.BinaryOp in
  match token with
  | Lexer.Token.Plus -> Some Add
  | Lexer.Token.Minus -> Some Subtract
  | Lexer.Token.Star -> Some Multiply
  | Lexer.Token.Slash -> Some Divide
  | Lexer.Token.Percent -> Some Modulo
  | Lexer.Token.Equal -> Some Equal
  | Lexer.Token.NotEqual -> Some NotEqual
  | Lexer.Token.Less -> Some Less
  | Lexer.Token.LessEqual -> Some LessEqual
  | Lexer.Token.Greater -> Some Greater
  | Lexer.Token.GreaterEqual -> Some GreaterEqual
  | _ -> None

let token_to_unaryop (token: Lexer.Token.token_kind) : Ast.UnaryOp.t option =
  let open Ast.UnaryOp in
  match token with
  | Lexer.Token.Minus -> Some Negate
  | _ -> None

let parse_literal (state: Parser.state) : (Ast.literal, Parser.error) result =
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
            (Parser.UnexpectedTokens
               { expected = valid_tokens;
                 found = token.kind;
                 line = token.line;
                 column = token.column }))
  | None -> Error Parser.UnexpectedEndOfInput


(* Parse an expression, yielding a (Ast.Expr.t, Parser.error) result pair *)
let rec parse_expr (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  parse_expr_impl state 0

and parse_expr_impl (state: Parser.state) (prec: Parser.prec) : (Ast.Expr.t, Parser.error) result =
  let* lhs = parse_prefix state in
  let rec loop lhs =
    match peek state with
    | Some token when precedence_for_token token.kind > prec ->
        let* lhs' = parse_infix state lhs prec in
        loop lhs'
    | _ -> Ok lhs
  in
  loop lhs

and parse_unary (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error Parser.UnexpectedEndOfInput
  in
  let op = match token_to_unaryop token.kind with
    | Some op -> op
    | None -> failwith "Expected unary operator"
  in
  let* expr = parse_expr_impl state 80 in
  Ok (Ast.Expr.UnaryOp { op; expr })

and parse_prefix (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let open Lexer in

  match peek state with
  | Some token -> (
      match token.kind with
      | Token.Int _ | Token.Float _ | Token.String _
      | Token.Char _ ->
          parse_value state
      | Token.Ident name ->
          let _ = next state in
          Ok (Ast.Expr.Variable name)
      | Token.Minus ->
          parse_unary state
      | _ ->
          Error
            (Parser.UnexpectedToken
               { expected = token.kind;
                 found = token.kind;
                 line = token.line;
                 column = token.column }))
  | None -> Error Parser.UnexpectedEndOfInput

and parse_value (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let open Lexer in

  match peek state with
  | Some token -> (
      match token.kind with
      | Token.Int _ | Token.Float _ | Token.String _
      | Token.Char _ -> 
        let* lit = parse_literal state in
        Ok (Ast.Expr.Literal lit)
      | Token.Minus -> 
          parse_unary state
      | Token.Ident name ->
          let _ = next state in
          Ok (Ast.Expr.Variable name)
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
  | None -> Error Parser.UnexpectedEndOfInput

and parse_infix (state: Parser.state) (lhs: Ast.Expr.t) (prec: Parser.prec) : (Ast.Expr.t, Parser.error) result =
  let open Lexer in
  
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error Parser.UnexpectedEndOfInput
  in
  match token.kind with
  | Token.LParen ->
      let* args = parse_argument_list state in
      Ok (Ast.Expr.Call { fn = lhs; args })
  | Token.Equal ->
      let* rhs = parse_expr_impl state (next_higher prec) in
      Ok (Ast.Expr.Assign { target = lhs; value = rhs })

  (* Parse binary operator *)
  | _ when token_to_binop token.kind <> None ->
      let op = match token_to_binop token.kind with
        | Some op -> op
        | None -> failwith "Expected binary operator"
      in
      let* rhs = parse_expr_impl state (next_higher prec) in
      Ok (Ast.Expr.BinaryOp { left = lhs; op; right = rhs })

  (* Unexpected token *)
  | _ -> Error (Parser.UnexpectedToken { expected = token.kind; found = token.kind; line = token.line; column = token.column })

and parse_argument_list (state: Parser.state) : (Ast.Expr.t list, Parser.error) result =
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
              (Parser.UnexpectedToken
                 { expected = Lexer.Token.Comma;
                   found = token.kind;
                   line = token.line;
                   column = token.column })
        | None -> Error Parser.UnexpectedEndOfInput)
    | None -> Error Parser.UnexpectedEndOfInput
  in
  aux []

(* ************************************************* *)

and parse_ty (state: Parser.state) : (Ast.Ty.t, Parser.error) result =
  let open Lexer in
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error Parser.UnexpectedEndOfInput
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
  | _ -> Error (Parser.ExpectedType { line = token.line; column = token.column })

and parse_ident (state: Parser.state) : (string, Parser.error) result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error Parser.UnexpectedEndOfInput
  in
  match token.kind with
  | Lexer.Token.Ident id -> Ok id
  | _ -> Error (Parser.ExpectedIdent { line = token.line; column = token.column })

and parse_region_id (state: Parser.state) : (string, Parser.error) result =
  let* token = match next state with
    | Some t -> Ok t
    | None -> Error Parser.UnexpectedEndOfInput
  in
  match token.kind with
  | Lexer.Token.Ident id -> Ok id
  | _ -> Error (Parser.ExpectedIdent { line = token.line; column = token.column })

and parse_reference (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let* _ = expect state Lexer.Token.Ampersand in
  let* ident = parse_ident state in
  Ok (Ast.Expr.Variable ("&" ^ ident))

and parse_arrow (state: Parser.state) : ((Ast.region_id * Ast.region_id), Parser.error) result =
  (* ~a -> ~b *)
  let* _ = expect state Lexer.Token.Tilde in
  let* from_region = parse_region_id state in
  let* _ = expect state Lexer.Token.Arrow in
  let* _ = expect state Lexer.Token.Tilde in
  let* to_region = parse_region_id state in
  Ok (from_region, to_region)

and parse_restrict (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let* _ = expect state Lexer.Token.Restrict in
  let* from_exp = parse_expr_impl state 0 in

  (* ~a -> ~b *)
  let* arr = parse_arrow state in
  let (from_region, to_region) = arr in
  Ok (Ast.Expr.Restrict { expr = from_exp; source = from_region; target = to_region })

and parse_extend (state: Parser.state) : (Ast.Expr.t, Parser.error) result =
  let* _ = expect state Lexer.Token.Extend in
  let* from_exp = parse_expr_impl state 0 in

  (* ~a -> ~b *)
  let* arr = parse_arrow state in
  let (from_region, to_region) = arr in
  Ok (Ast.Expr.Extend { expr = from_exp; source = from_region; target = to_region })

(* *************************************************
 * Statement parsing
 ************************************************** *)

and parse_block (state: Parser.state) : (Ast.Stmt.t list, Parser.error) result =
  let* _ = expect state Lexer.Token.LBrace in
  let rec aux acc =
    match peek state with
    | Some token when token.kind = Lexer.Token.RBrace ->
        let _ = next state in
        Ok (List.rev acc)
    | Some _ ->
        let* stmt = parse_stmt state in
        aux (stmt :: acc)
    | None -> Error Parser.UnexpectedEndOfInput
  in
  aux []

and parse_let (state: Parser.state) : (Ast.Stmt.t, Parser.error) result =
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
  Ok (Ast.Stmt.Let { name; ty; value })

and parse_for (state: Parser.state) : (Ast.Stmt.t, Parser.error) result =
  (* for var in iterable { .. }*)
  let* _ = expect state Lexer.Token.For in
  let* var = parse_ident state in
  let* _ = expect state Lexer.Token.In in
  let* iterable = parse_expr_impl state 0 in
  let* _ = expect state Lexer.Token.LBrace in
  let* body = parse_block state in
  Ok (Ast.Stmt.For { var; iterable; body })

and parse_while (state: Parser.state) : (Ast.Stmt.t, Parser.error) result =
  (* while condition { .. } *)
  let* _ = expect state Lexer.Token.While in
  let* condition = parse_expr_impl state 0 in
  let* _ = expect state Lexer.Token.LBrace in
  let* body = parse_block state in
  Ok (Ast.Stmt.While { condition; body })

and parse_stmt (state: Parser.state) : (Ast.Stmt.t, Parser.error) result =
  match peek state with
  | Some token -> (
      match token.kind with
      | Lexer.Token.Let -> parse_let state
      | Lexer.Token.For -> parse_for state
      | Lexer.Token.While -> parse_while state
      | _ ->
          (* Expression in statement place *)
          let* expr = parse_expr_impl state 0 in
          Ok (Ast.Stmt.Expr expr))
  | None -> Error Parser.UnexpectedEndOfInput