%{
%}

%token EOF
%token <Token.kind> ANY

%start <Ast.program> program

%%

program:
  | tokens EOF { Ast.of_tokens (List.rev $1) }

tokens:
  | /* empty */ { [] }
  | tokens ANY { $2 :: $1 }
