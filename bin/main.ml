open Ragdoll

let () =
  let input = "//let x : i32 = 42" in
  match Lexer.do_lex input with
  | Ok tokens ->
      List.iter
        (fun (token : Lexer.Token.t) ->
          Printf.printf "Token: kind=%s, line=%d, column=%d\n"
            (Lexer.tokenkind_to_string token.kind)
            token.line
            token.column)
        tokens
  | Error msg ->
      prerr_endline msg

