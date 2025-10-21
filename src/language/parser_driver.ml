module Report = Diagnostics.Report
module I = Parser.MenhirInterpreter

let parser_producer =
  { Error.Located.phase= `Parser; module_path= Some "language.parser" }

let positions_of_token token =
  let span = Token.span token in
  let startp = Positions.to_lexing span.start in
  let endp = Positions.to_lexing span.stop in
  (startp, endp)

let to_parser_token token =
  if Token.is_trivia token then None
  else
    let startp, endp = positions_of_token token in
    match Token.kind token with
    | Token.Eof ->
        Some (Parser.EOF, startp, endp)
    | kind ->
        Some (Parser.ANY kind, startp, endp)

let add_parse_error diagnostics message =
  let span = Positions.point_span Positions.unknown_point in
  let diag = Diagnostics.make `Error parser_producer span message in
  Report.add diag diagnostics

let rec loop checkpoint tokens =
  match checkpoint with
  | I.InputNeeded _ -> (
      match tokens with
      | (token, startp, endp) :: rest ->
          let checkpoint = I.offer checkpoint (token, startp, endp) in
          loop checkpoint rest
      | [] ->
          let pos = Lexing.dummy_pos in
          let checkpoint = I.offer checkpoint (Parser.EOF, pos, pos) in
          loop checkpoint [])
  | I.Shifting _ | I.AboutToReduce _ ->
      loop (I.resume checkpoint) tokens
  | I.Accepted ast ->
      ast
  | I.HandlingError _ ->
      failwith "unexpected parser error"
  | I.Rejected ->
      failwith "input rejected"

let parse stream =
  let base_diagnostics = Token_stream.diagnostics stream in
  let diagnostics = ref base_diagnostics in
  let raw_tokens = Array.to_list (Token_stream.tokens stream) in
  let tokens = raw_tokens |> List.filter_map to_parser_token in
  let start_pos = Lexing.dummy_pos in
  let checkpoint = Parser.Incremental.program start_pos in
  let ast =
    try loop checkpoint tokens
    with Failure message ->
      diagnostics := add_parse_error !diagnostics message
      ; Ast.empty
  in
  (ast, !diagnostics)
