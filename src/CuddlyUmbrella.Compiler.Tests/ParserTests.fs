module CuddlyUmbrella.Compiler.Tests.ParserTests

open CuddlyUmbrella.Compiler
open CuddlyUmbrella.Compiler.Ast
open Microsoft.FSharp.Text.Lexing
open NUnit.Framework
open Parser


let stringToAst input =
    let lexbuf = LexBuffer<_>.FromString input
    let ast = Parser.program Lexer.tokenize lexbuf
    ast

let createTestData lst =
    lst |> List.map (fun (str, par:Program) -> new TestCaseData(str, par))

[<Test>]
let ``parses empty string to empty program ast``() =
    let ast = "" |> stringToAst
    let (functions, statements) = ast

    Assert.True(List.isEmpty functions)
    Assert.True(List.isEmpty statements)


[<Test>]
[<TestCaseSource("variableDefinitionsTestData")>]
let ``parses variable declaratinos to ast``(rawString, expectedParsed) =

    let ast = rawString |> stringToAst
    
    Assert.True((expectedParsed = ast))


let variableDefinitionsTestData =
    [
        "var a: int;",
        ([], [VariableDeclaration("a", Int)]);

        "var b: int;",
        ([], [VariableDeclaration("b", Int)]);

        "var c: string;",
        ([], [VariableDeclaration("c", String)]);

        "var d: bool;",
        ([], [VariableDeclaration("d", Bool)]);

        "var e: float;",
        ([], [VariableDeclaration("e", Float)]);


    ] |> createTestData

