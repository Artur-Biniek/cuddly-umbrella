module CuddlyUmbrella.Compiler.ParserUtils

open Parser


let private keywords_table =
    [
        "def", DEF;
        "return", RETURN;

    ] |> Map.ofList


let private punctuations_table =
    [
        ",", COMMA;
        ":", COLON;
        ";", SEMICOLON;
        "(", LPAREN;
        ")", RPAREN;
        "{", LBRACE;
        "}", RBRACE;
        "\"", DBLQUOTE;

    ] |> Map.ofList


let private operators_table =
    [
        "+", PLUS;
        "-", MINUS;
        "=", EQ;
        "<", LT;
        ">", GT;
        "<=", LEQ;
        ">=", GEQ;
        "<-", LARROW;

    ] |> Map.ofList


let private lookupLexemeInTokenTable (table : Map<string, Parser.token>) lexeme =
    match table.TryFind(lexeme) with
    | Some(token) -> token   
    | None -> failwith ("Lexeme not found in the table: " + lexeme)   

let keywordOrIdentifierToken lexeme =
    match keywords_table.TryFind(lexeme) with   
    | Some(token) -> token   
    | None -> IDENT(lexeme)

let punctuationToken = lookupLexemeInTokenTable punctuations_table

let operatorToken = lookupLexemeInTokenTable operators_table