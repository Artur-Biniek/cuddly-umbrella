module CuddlyUmbrella.Compiler.Tests.ParserTests

open Microsoft.FSharp.Text.Lexing
open NUnit.Framework
open CuddlyUmbrella.Compiler
open Parser

[<Test>]
[<TestCaseSource("correctIdentifierTestCases")>]
let ``tokenizes identifiers correctly`` input =
    let lexbuf = LexBuffer<_>.FromString input
    let tokens = Lexer.getAllTokens lexbuf
    tokens

[<Test>]
let ``ignores spaces, tabs, and line endings``() =
    let lexbuf = LexBuffer<_>.FromString "             \t   \t  \t   \t\t\r\n\t   \n"
    let tokens = Lexer.getAllTokens lexbuf
    let areEqual = (tokens = [EOF]);
    Assert.That(areEqual)

[<Test>]
[<TestCaseSource("correctPunctuationsTestCases")>]
let ``tokenizes punctuations correctly`` input =
    let lexbuf = LexBuffer<_>.FromString input
    let tokens = Lexer.getAllTokens lexbuf
    tokens

let correctIdentifierTestCases = 
    [
        // just small letters
        "id",
        [IDENT("id")];

        // just capital letters
        "IDENTIFIER",
        [IDENT("IDENTIFIER")];

        // mix of small and uppder with multiple whitespaces
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

    ] |> List.map (fun (a, b) -> TestCaseData(a).Returns(b @ [EOF]))

