// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | LARROW
  | PLUS
  | MINUS
  | EQ
  | LT
  | GT
  | LEQ
  | GEQ
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COLON
  | SEMICOLON
  | COMMA
  | DBLQUOTE
  | FLOAT of (System.Single)
  | INT of (System.Int32)
  | BOOL of (System.Boolean)
  | TYPE_SPEC of (string)
  | IDENT of (string)
  | DEF
  | RETURN
  | VAR
  | IF
  | THEN
  | ELSE
  | IDENTIFIER
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LARROW
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_EQ
    | TOKEN_LT
    | TOKEN_GT
    | TOKEN_LEQ
    | TOKEN_GEQ
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_COLON
    | TOKEN_SEMICOLON
    | TOKEN_COMMA
    | TOKEN_DBLQUOTE
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_BOOL
    | TOKEN_TYPE_SPEC
    | TOKEN_IDENT
    | TOKEN_DEF
    | TOKEN_RETURN
    | TOKEN_VAR
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_IDENTIFIER
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_program
    | NONTERM_fun_decl_list
    | NONTERM_fun_decl
    | NONTERM_stmt_list
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( CuddlyUmbrella.Compiler.Ast.Program ) 
