(*
 * Lexer for the Ragdoll programming language.
 *)

module Token = struct
  type token_kind =
    | EOF
    | Int of int64
    | Float of float
    | String of string
    | Char of char
    | Ident of string

    (* Symbols *)
    | Plus
    | Minus
    | Star
    | Slash
    | Percent
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    | Equal
    | Dot
    | DotDot
    | Comma
    | Colon
    | Semicolon
    | Ampersand
    | Apostrophe
    | Tilde
    | DoubleEqual
    | NotEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Arrow     (* -> *)
    | FatArrow  (* => *)

    (* Keywords *)
    | Let
    | In
    | If
    | Else
    | While
    | For
    | Return
    | Break
    | Def
    | Struct
    | Enum
    | Topology
    | Restrict (* restrict a a_to_b*) 
    | Extend   (* extend a a_to_b*)

    (* Reserved types *)
    | TyU1
    | TyU8
    | TyU16
    | TyU32
    | TyU64
    | TyI8
    | TyI16
    | TyI32
    | TyI64
    | TyString
    | TyChar
    | TyUnit

  type t = {
    kind: token_kind;
    line: int;
    column: int;
  }
end

(* Lexer state *)
type lexer_state = {
  input: string;
  mutable position: int;
  mutable line: int;
  mutable column: int;
}

let tokenkind_to_string kind =
  match kind with
  | Token.EOF -> "EOF"
  | Token.Int _ -> "Int"
  | Token.Float _ -> "Float"
  | Token.String _ -> "String"
  | Token.Char _ -> "Char"
  | Token.Ident _ -> "Ident"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.LParen -> "LParen"
  | Token.RParen -> "RParen"
  | Token.LBrace -> "LBrace"
  | Token.RBrace -> "RBrace"
  | Token.LBracket -> "LBracket"
  | Token.RBracket -> "RBracket"
  | Token.Equal -> "Equal"
  | Token.Dot -> "Dot"
  | Token.DotDot -> "DotDot"
  | Token.Comma -> "Comma"
  | Token.Colon -> "Colon"
  | Token.Semicolon -> "Semicolon"
  | Token.Ampersand -> "Ampersand"
  | Token.Apostrophe -> "Apostrophe"
  | Token.Tilde -> "Tilde"
  | Token.DoubleEqual -> "DoubleEqual"
  | Token.NotEqual -> "NotEqual"
  | Token.Less -> "Less"
  | Token.LessEqual -> "LessEqual"
  | Token.Greater -> "Greater"
  | Token.GreaterEqual -> "GreaterEqual"
  | Token.Arrow -> "Arrow"
  | Token.FatArrow -> "FatArrow"
  | Token.Let -> "Let"
  | Token.In -> "In"
  | Token.If -> "If"
  | Token.Else -> "Else"
  | Token.While -> "While"
  | Token.For -> "For"
  | Token.Return -> "Return"
  | Token.Break -> "Break"
  | Token.Def -> "Def"
  | Token.Struct -> "Struct"
  | Token.Enum -> "Enum"
  | Token.Topology -> "Topology"
  | Token.Restrict -> "Restrict"
  | Token.Extend -> "Extend"
  | Token.TyU1 -> "TyU1"
  | Token.TyU8 -> "TyU8"
  | Token.TyU16 -> "TyU16"
  | Token.TyU32 -> "TyU32"
  | Token.TyU64 -> "TyU64"
  | Token.TyI8 -> "TyI8"
  | Token.TyI16 -> "TyI16"
  | Token.TyI32 -> "TyI32"
  | Token.TyI64 -> "TyI64"
  | Token.TyString -> "TyString"
  | Token.TyChar -> "TyChar"
  | Token.TyUnit -> "TyUnit"

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

(* ******************************************* *)
(* Lexer implementation *)
(* ******************************************* *)

let create_lexer input =
  { input; position = 0; line = 1; column = 1 }

let create_token kind line column =
  { Token.kind; Token.line = line; Token.column = column }

let advance state =
  if state.position < String.length state.input then
    let c = state.input.[state.position] in
    state.position <- state.position + 1;
    if c = '\n' then (
      state.line <- state.line + 1;
      state.column <- 1
    ) else
      state.column <- state.column + 1

let skip_line_comment state =
  while state.position < String.length state.input && state.input.[state.position] <> '\n' do
    advance state
  done

let skip_block_comment state =
  let rec loop i = 
    if i >= String.length state.input then
      Error "Unterminated block comment"
    
    (* if we see a */, we have a terminator *)
    else if state.input.[i] = '*' && i + 1 < String.length state.input && state.input.[i + 1] = '/' then (
      state.position <- i + 2;
      Ok ()
    ) else
      loop (i + 1)
  in
  loop state.position

let lex_integer state : (Token.t, string) result =
  let start_pos = state.position in
  let start_line = state.line in
  let start_column = state.column in

  (* consume digits *)
  while state.position < String.length state.input && is_digit state.input.[state.position] do
    advance state
  done;

  (* extract the number string and return a token *)
  let num_str = String.sub state.input start_pos (state.position - start_pos) in
  Ok (create_token (Token.Int (Int64.of_string num_str)) start_line start_column)

let lex_float state : (Token.t, string) result =
  let start_pos = state.position in
  let start_line = state.line in
  let start_column = state.column in

  (* consume digits before the dot *)
  while state.position < String.length state.input && is_digit state.input.[state.position] do
    advance state
  done;

  (* expect a dot *)
  if state.position < String.length state.input && state.input.[state.position] = '.' then (
    advance state; (* consume the dot *)

    (* consume digits after the dot *)
    while state.position < String.length state.input && is_digit state.input.[state.position] do
      advance state
    done;

    (* extract the float string and return a token *)
    let float_str = String.sub state.input start_pos (state.position - start_pos) in
    Ok (create_token (Token.Float (float_of_string float_str)) start_line start_column)
  ) else
    Error "Expected '.' for float literal"

let lex_numeric state : (Token.t, string) result =
  let start_pos = state.position in
  let start_line = state.line in
  let start_column = state.column in

  (* consume digits before the dot *)
  while state.position < String.length state.input && is_digit state.input.[state.position] do
    advance state
  done;

  (* check if we have a dot for float *)
  if state.position < String.length state.input && state.input.[state.position] = '.' then (
    advance state; (* consume the dot *)

    (* consume digits after the dot *)
    while state.position < String.length state.input && is_digit state.input.[state.position] do
      advance state
    done;

    (* extract the float string and return a token *)
    let float_str = String.sub state.input start_pos (state.position - start_pos) in
    Ok (create_token (Token.Float (float_of_string float_str)) start_line start_column)
  ) else (
    (* no dot, treat as integer *)
    let int_str = String.sub state.input start_pos (state.position - start_pos) in
    Ok (create_token (Token.Int (Int64.of_string int_str)) start_line start_column)
  )

let lex_char state : (Token.t, string) result =
  let start_line = state.line in
  let start_column = state.column in

  advance state; (* skip the opening quote *)
  if state.position >= String.length state.input then
    Error "Unterminated character literal"
  else
    let c = state.input.[state.position] in
    advance state; (* consume the character *)

    if state.position < String.length state.input && state.input.[state.position] = '\'' then (
      advance state; (* skip closing quote *)
      Ok (create_token (Token.Char c) start_line start_column)
    ) else
      Error "Expected closing single quote for char literal"

let lex_string state : (Token.t, string) result =
  let start_pos = state.position in
  advance state; (* skip the opening quote *)

  while state.position < String.length state.input && state.input.[state.position] <> '"' do
    advance state;
  done;

  if state.position >= String.length state.input then
    Error "Unterminated string literal"
  else
    let str_val = String.sub state.input (start_pos + 1) (state.position - start_pos - 1) in
    advance state; (* skip the closing quote *)
    Ok (create_token (Token.String str_val) state.line state.column)

let lex_ident_or_keyword state : (Token.t, string) result =
  let start_pos = state.position in

  while state.position < String.length state.input && (is_alpha state.input.[state.position] || is_digit state.input.[state.position]) do
    advance state;
  done;

  let ident_str = String.sub state.input start_pos (state.position - start_pos) in
  let kind =
    match ident_str with
    | "let" -> Token.Let
    | "in" -> Token.In
    | "if" -> Token.If
    | "else" -> Token.Else
    | "while" -> Token.While
    | "for" -> Token.For
    | "def" -> Token.Def
    | "struct" -> Token.Struct
    | "enum" -> Token.Enum
    | "topology" -> Token.Topology
    | "return" -> Token.Return
    | "break" -> Token.Break
    | "restrict" -> Token.Restrict
    | "extend" -> Token.Extend

    (* reserved types *)
    | "u1" -> Token.TyU1
    | "u8" -> Token.TyU8
    | "u16" -> Token.TyU16
    | "u32" -> Token.TyU32
    | "u64" -> Token.TyU64
    | "i8" -> Token.TyI8
    | "i16" -> Token.TyI16
    | "i32" -> Token.TyI32
    | "i64" -> Token.TyI64
    | "string" -> Token.TyString
    | "char" -> Token.TyChar
    | "unit" -> Token.TyUnit

    (* lex identifier *)
    | _ -> Token.Ident ident_str
  in
  Ok (create_token kind state.line state.column)

let emit_single_token state kind =
  advance state;
  Ok (create_token kind state.line state.column)

let emit_double_token state kind =
  advance state;
  advance state;
  Ok (create_token kind state.line state.column)

let rec lex state : (Token.t, string) result =
  if state.position >= String.length state.input then
    (* EOF reached *)
    Ok (create_token Token.EOF state.line state.column)
  else
    let c = state.input.[state.position] in
    match c with
    | ' ' | '\t' | '\r' | '\n' ->
        advance state;
        lex state
    | '+' -> emit_single_token state Token.Plus
    | '-' -> if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '>' then
              emit_double_token state Token.Arrow
            else
              emit_single_token state Token.Minus
    | '*' -> emit_single_token state Token.Star
    | '/' -> if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '/' then (
              (* line comment: skip it *)
              skip_line_comment state;
              lex state
            ) else if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '*' then (
              (* block comment: skip it *)
              match skip_block_comment state with
              | Ok () -> lex state
              | Error msg -> Error msg
            ) else
              emit_single_token state Token.Slash
    | '%' -> emit_single_token state Token.Percent
    | '(' -> emit_single_token state Token.LParen
    | ')' -> emit_single_token state Token.RParen
    | '{' -> emit_single_token state Token.LBrace
    | '}' -> emit_single_token state Token.RBrace
    | '[' -> emit_single_token state Token.LBracket
    | ']' -> emit_single_token state Token.RBracket
    | '.' ->
      if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '.' then
        emit_double_token state Token.DotDot
      else
        emit_single_token state Token.Dot
    | ',' -> emit_single_token state Token.Comma
    | ':' -> emit_single_token state Token.Colon
    | ';' -> emit_single_token state Token.Semicolon
    | '&' -> emit_single_token state Token.Ampersand
    | '~' -> emit_single_token state Token.Tilde
    | '=' ->
        if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '=' then
          emit_double_token state Token.DoubleEqual
        else if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '>' then
          emit_double_token state Token.FatArrow
        else
          emit_single_token state Token.Equal
    | '!' ->
        if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '=' then
          emit_double_token state Token.NotEqual
        else
          Error "Unexpected character: !"
    | '<' ->
        if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '=' then
          emit_double_token state Token.LessEqual
        else
          emit_single_token state Token.Less
    | '>' ->
        if state.position + 1 < String.length state.input && state.input.[state.position + 1] = '=' then
          emit_double_token state Token.GreaterEqual
        else
          emit_single_token state Token.Greater

    | '0' .. '9' -> lex_numeric state
    | '\'' -> lex_char state
    | '"'  -> lex_string state

    (* identifier or reserved keyword *)
    | _ when is_alpha c -> lex_ident_or_keyword state

    (* unexpected character *)
    | _ -> Error (Printf.sprintf "Unexpected character: %c" c)

let do_lex (input: string) : (Token.t list, string) result =
  let state = create_lexer input in
  let rec aux acc =
    match lex state with
    | Ok token ->
        if token.kind = Token.EOF then
          Ok (List.rev (token :: acc))
        else
          aux (token :: acc)
    | Error msg -> Error msg
  in
  aux []