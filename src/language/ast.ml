type token = Token.kind
type program = { tokens: token list }

let empty = { tokens= [] }
let of_tokens tokens = { tokens }
