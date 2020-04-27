{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
]
}

let space = [' ' '\009' '\012' '\n']

rule main = parse
| space+ { main lexbuf }
| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.MULT }
| "/" { Parser.DIV }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "(*" { comments 0 lexbuf }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comments level = parse
| "*)"
    { if level = 0 then main lexbuf
      else comments (level-1) lexbuf
    }
| "(*" { comments (level+1) lexbuf }
| _ { comments level lexbuf }
| eof
    { print_endline "Error: comments not closed";
      raise End_of_file
    }