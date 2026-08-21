open Ragdoll

let () =
  let input = "42 + 3 * f(4, 3.14)" in
  match Lexer.do_lex input with
  | Ok tokens -> (
    let parser = Parser.create_parser tokens in
    let result = Parser.parse_expr parser in
    match result with
    | Ok ast -> Printf.printf "Parsed AST: %s\n" (Ast.expr_to_string ast)
    | Error err -> Printf.printf "Parser error: %s\n" (Parser.error_to_string err)
  )
  | Error msg ->
      prerr_endline msg

