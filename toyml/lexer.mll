{
let reservedWords = [
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
]
}

rule main = parse
  (* skip space and newline *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parse.LT }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
  { let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with
    _ -> Parser.ID id
  }
| eof { exit 0 }