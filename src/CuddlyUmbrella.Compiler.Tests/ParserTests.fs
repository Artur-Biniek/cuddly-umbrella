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
        ([], [VariableDeclaration("a", Int, None)]);

        "var b: int;",
        ([], [VariableDeclaration("b", Int, None)]);

        "var c: string;",
        ([], [VariableDeclaration("c", String, None)]);

        "var d: bool;",
        ([], [VariableDeclaration("d", Bool, None)]);

        "var e: float;",
        ([], [VariableDeclaration("e", Float, None)]);

        "var f: int <- 12;",
        ([], [VariableDeclaration("f", Int, Some(LiteralExpression(IntLiteral(12))))])

        "var g: float <- 23.34;",
        ([], [VariableDeclaration("g", Float, Some(LiteralExpression(FloatLiteral(23.34f))))])

        "var h: bool <- true;",
        ([], [VariableDeclaration("h", Bool, Some(LiteralExpression(BoolLiteral(true))))])

        "var i: bool <- false;",
        ([], [VariableDeclaration("i", Bool, Some(LiteralExpression(BoolLiteral(false))))])

        "var j: string <- \"Hello\";",
        ([], [VariableDeclaration("j", String, Some(LiteralExpression(StringLiteral("Hello"))))])

        "var k: string <- \"Compiler\";",
        ([], [VariableDeclaration("k", String, Some(LiteralExpression(StringLiteral("Compiler"))))])

        "var l: string <- \"World!\";",
        ([], [VariableDeclaration("l", String, Some(LiteralExpression(StringLiteral("World!"))))])


    ] |> createTestData

