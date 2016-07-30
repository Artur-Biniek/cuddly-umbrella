module CuddlyUmbrella.Compiler.Tests.ParserTests

open CuddlyUmbrella.Compiler
open Microsoft.FSharp.Text.Lexing
open NUnit.Framework
open Parser


let stringToTokens input =
    let lexbuf = LexBuffer<_>.FromString input
    let tokens = Lexer.getAllTokens lexbuf
    tokens


[<Test>]
[<TestCaseSource("correctIdentifierTestCases")>]
let ``tokenizes identifiers correctly`` input = stringToTokens input


[<Test>]
let ``ignores spaces, tabs, and line endings``() =
    let tokens = stringToTokens "             \t   \t  \t   \t\t\r\n\t   \n"
    let areEqual = (tokens = [EOF]);

    Assert.That(areEqual)


[<Test>]
[<TestCaseSource("correctPunctuationsTestCases")>]
let ``tokenizes punctuations correctly`` input = stringToTokens input


[<Test>]
[<TestCaseSource("correctOperatorsTestCases")>]
let ``tokenizes operators correctly`` input = stringToTokens input


let correctIdentifierTestCases = 
    [
        // just small letters
        "id",
        [IDENT("id")];

        // just capital letters
        "IDENTIFIER",
        [IDENT("IDENTIFIER")];

        // mix of small and upper with multiple whitespaces
        "aBcDeFgHi          JkLmNoP",
        [IDENT("aBcDeFgHi"); IDENT("JkLmNoP")];

        // mix of small and uppder and digits with multiple whitespaces
        "aBcDeFgHi1          JkLmNoP23  r098765",
        [IDENT("aBcDeFgHi1"); IDENT("JkLmNoP23"); IDENT("r098765")];

        // few identifiers in a row
        "ab cdef",
        [IDENT("ab"); IDENT("cdef")];

        // few more identifires in a row
        "ab cdef ghijklmno12345",
        [IDENT("ab"); IDENT("cdef"); IDENT("ghijklmno12345")];

    ] |> List.map (fun (a, b) -> TestCaseData(a).Returns(b @ [EOF]))


let correctPunctuationsTestCases = 
    [
        // recognizes commas
        ", , ,            ,  ,",
        [COMMA; COMMA; COMMA; COMMA; COMMA];

        // recognizes colons
        ":  : :      :   : :",
        [COLON; COLON; COLON; COLON; COLON; COLON];

        // recognizes semicolons
        ";     ; ;",
        [SEMICOLON; SEMICOLON; SEMICOLON];

        // recognizes left and right parens
        "( )) (( )",
        [LPAREN; RPAREN; RPAREN; LPAREN; LPAREN; RPAREN];

        // recognizes left and right braces
        "{{ } }   { {}",
        [LBRACE; LBRACE; RBRACE; RBRACE; LBRACE; LBRACE; RBRACE];

        // recognizes double quotes
        "\" \"\" \"    \"",
        [DBLQUOTE; DBLQUOTE; DBLQUOTE; DBLQUOTE; DBLQUOTE];

        // mixed punctiations
        ",:;(){}\"",
        [COMMA; COLON; SEMICOLON; LPAREN; RPAREN; LBRACE; RBRACE; DBLQUOTE]

    ] |> List.map (fun (a, b) -> TestCaseData(a).Returns(b @ [EOF]))


let correctOperatorsTestCases = 
    [
        // recognizes pluses
        "+  +   + ++",
        [PLUS; PLUS; PLUS; PLUS; PLUS];

        // recognizes minuses
        "-- - -",
        [MINUS; MINUS; MINUS; MINUS];

        // recognizes equals
        "= == =",
        [EQ; EQ; EQ; EQ];

        // recognizes less than
        "< << <",
        [LT; LT; LT; LT]

        // recognizes less than
        "<= <=<= <=<=",
        [LEQ; LEQ; LEQ; LEQ; LEQ]

        // recognizes greater than
        "   >>   > >  ",
        [GT; GT; GT; GT]

        // recognizes greater than
        ">=>=>= >=",
        [GEQ; GEQ; GEQ; GEQ]

        // mixed operators
        "-+=<=<>>=",
        [MINUS; PLUS; EQ; LEQ; LT; GT; GEQ]


    ] |> List.map (fun (a, b) -> TestCaseData(a).Returns(b @ [EOF]))
