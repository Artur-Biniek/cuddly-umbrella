module CuddlyUmbrella.Compiler.Tests.ParserTests

open Microsoft.FSharp.Text.Lexing
open NUnit.Framework
open CuddlyUmbrella.Compiler
open Parser

[<Test>]
[<TestCaseSource("testCases")>]
let ``should tokenize identifiers correctly`` input =
    let lexbuf = LexBuffer<_>.FromString input
    let tokens = Lexer.getAllTokens lexbuf
    tokens

let testCases =
    ["id",
        [IDENT("id"); EOF];

     "ab cdef",
        [IDENT("ab"); IDENT("cdef"); EOF];

     "ab cdef ghijklmno12345",
        [IDENT("ab"); IDENT("cdef"); IDENT("ghijklmno12345"); EOF];
    
    ] |> List.map (fun (a, b) -> TestCaseData(a).Returns(b))

