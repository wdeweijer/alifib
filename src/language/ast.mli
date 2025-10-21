type token = Token.kind
type program = { tokens: token list }

val empty : program
val of_tokens : token list -> program
