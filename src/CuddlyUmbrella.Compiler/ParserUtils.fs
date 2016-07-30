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

    ] |> Map.ofList

let keywordOrIdentifierToken lexeme =
    match keywords_table.TryFind(lexeme) with   
    | Some(token) -> token   
    | None -> IDENT(lexeme)

let punctuationToken lexeme =
    match punctuations_table.TryFind(lexeme) with   
    | Some(token) -> token   
    | None -> failwith ("uknown punctuation: " + lexeme)