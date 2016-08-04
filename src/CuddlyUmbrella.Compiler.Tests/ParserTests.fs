module CuddlyUmbrella.Compiler.Tests.ParserTests

open CuddlyUmbrella.Compiler
open CuddlyUmbrella.Compiler.Ast
open Microsoft.FSharp.Text.Lexing
open NUnit.Framework


let stringToAst input =
    let lexbuf = LexBuffer<_>.FromString input
    let ast = Parser.program Lexer.tokenize lexbuf
    ast

let stringToExpression input =
    let lexbuf = LexBuffer<_>.FromString input
    let expression = Parser.expression Lexer.tokenize lexbuf
    expression

let createProgramTestData lst =
    lst |> List.map (fun (str, par:Program) -> new TestCaseData(str, par))

let createExpressionTestData lst =
    lst |> List.map (fun (str, par:Expression) -> new TestCaseData(str, par))

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

[<Test>]
[<TestCaseSource("expressionsTestData")>]
let ``parses expressions to ast``(rawString, expectedParsed) =

    let ast = rawString |> stringToExpression
    
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
        ([], [VariableDeclaration("f", Int, Some(LiteralExpression(IntLiteral(12))))]);

        "var g: float <- 23.34;",
        ([], [VariableDeclaration("g", Float, Some(LiteralExpression(FloatLiteral(23.34f))))]);

        "var h: bool <- true;",
        ([], [VariableDeclaration("h", Bool, Some(LiteralExpression(BoolLiteral(true))))]);

        "var i: bool <- false;",
        ([], [VariableDeclaration("i", Bool, Some(LiteralExpression(BoolLiteral(false))))]);

        "var j: string <- \"Hello\";",
        ([], [VariableDeclaration("j", String, Some(LiteralExpression(StringLiteral("Hello"))))]);

        "var k: string <- \"Compiler\";",
        ([], [VariableDeclaration("k", String, Some(LiteralExpression(StringLiteral("Compiler"))))]);

        "var l: string <- \"World!\";",
        ([], [VariableDeclaration("l", String, Some(LiteralExpression(StringLiteral("World!"))))]);

        "var m: int <- intvar;",
         ([], [VariableDeclaration("m", Int, Some(IdentifierExpression("intvar")))]);

        "var n: float <- fun();",
         ([], [VariableDeclaration("n", Float, Some(FunctionCallExpression("fun", [])))]);

        "var o: bool <- fun2(1, b);",
         (
            [], 
            [
                VariableDeclaration(
                    "o", 
                    Bool, 
                    Some(
                        FunctionCallExpression(
                            "fun2", 
                            [
                                LiteralExpression(IntLiteral(1));
                                IdentifierExpression("b")
                            ]
                        )
                    )
                )
            ]
         );

         "var o: bool <- fun3(1, 4 / z);",
         (
            [], 
            [
                VariableDeclaration(
                    "o", 
                    Bool, 
                    Some(
                        FunctionCallExpression(
                            "fun3", 
                            [
                                LiteralExpression(IntLiteral(1));
                                BinaryExpression(
                                    LiteralExpression(IntLiteral(4)),
                                    Divide,
                                    IdentifierExpression("z")    
                                )
                            ]
                        )
                    )
                )
            ]
         );

         "var o: bool <- fun4(1, nested(6 +4) / z);",
         (
            [], 
            [
                VariableDeclaration(
                    "o", 
                    Bool, 
                    Some(
                        FunctionCallExpression(
                            "fun4", 
                            [
                                LiteralExpression(IntLiteral(1));
                                BinaryExpression(
                                    FunctionCallExpression(
                                        "nested",
                                        [
                                            BinaryExpression(
                                                LiteralExpression(IntLiteral(6)),
                                                Add,
                                                LiteralExpression(IntLiteral(4))
                                            )
                                        ]
                                    ),
                                    Divide,
                                    IdentifierExpression("z")    
                                )
                            ]
                        )
                    )
                )
            ]
         );

    ] |> createProgramTestData

let expressionsTestData =
    [
        "4",
        LiteralExpression(IntLiteral(4));

        "123.0",
        LiteralExpression(FloatLiteral(123.0f));

        "\"that is some string!\"",
        LiteralExpression(StringLiteral("that is some string!"));

        "true",
        LiteralExpression(BoolLiteral(true));

        "false",
        LiteralExpression(BoolLiteral(false));

        "sum(1, giveTwo(), variableThree)",
        FunctionCallExpression(
            "sum", 
            [
                LiteralExpression(IntLiteral(1)); 
                FunctionCallExpression(
                    "giveTwo", 
                    []
                ); 
                IdentifierExpression("variableThree")
            ]
        );

        "3 + 4",
        BinaryExpression(
            LiteralExpression(IntLiteral(3)), 
            Add, 
            LiteralExpression(IntLiteral(4))
        );

        "12.0 -89.0",
        BinaryExpression(
            LiteralExpression(FloatLiteral(12.0f)), 
            Substract, 
            LiteralExpression(FloatLiteral(89.0f))
        );

        "5 * 6",
        BinaryExpression(
            LiteralExpression(IntLiteral(5)), 
            Multiply, 
            LiteralExpression(IntLiteral(6))
        );

         "100 / 20",
        BinaryExpression(
            LiteralExpression(IntLiteral(100)), 
            Divide, 
            LiteralExpression(IntLiteral(20))
        );

        "345 + 10 * 30",
        BinaryExpression(
            LiteralExpression(IntLiteral(345)), 
            Add, 
            BinaryExpression(
                LiteralExpression(IntLiteral(10)), 
                Multiply, 
                LiteralExpression(IntLiteral(30))
            )
        );

        "987 - 65 / 43",
        BinaryExpression(
            LiteralExpression(IntLiteral(987)), 
            Substract, 
            BinaryExpression(
                LiteralExpression(IntLiteral(65)), 
                Divide, 
                LiteralExpression(IntLiteral(43))
            )
        );

        "10 * 30 + 40",
        BinaryExpression(
            BinaryExpression(
                LiteralExpression(IntLiteral(10)),
                Multiply, 
                LiteralExpression(IntLiteral(30))
            ),
            Add, 
            LiteralExpression(IntLiteral(40))
        );

        "10 / 30 - 40",
        BinaryExpression(
            BinaryExpression(
                LiteralExpression(IntLiteral(10)), 
                Divide, 
                LiteralExpression(IntLiteral(30))
            ), 
            Substract, 
            LiteralExpression(IntLiteral(40))
        );

        "0 + (9 - 8)",
        BinaryExpression(
            LiteralExpression(IntLiteral(0)),
            Add,
            BinaryExpression(
                LiteralExpression(IntLiteral (9)),
                Substract,
                LiteralExpression(IntLiteral(8))
            )
        );

        "(0 + 9) - 8",
        BinaryExpression(
            BinaryExpression(
                LiteralExpression(IntLiteral (0)),
                Add,
                LiteralExpression(IntLiteral(9))
            ),           
            Substract,
            LiteralExpression(IntLiteral(8))
        );

        "((1 + 2) - ( 3 * 4))",
        BinaryExpression(
            BinaryExpression(
                LiteralExpression(IntLiteral(1)),
                Add,
                LiteralExpression(IntLiteral(2))
            ),
            Substract,
            BinaryExpression(
                LiteralExpression(IntLiteral(3)),
                Multiply,
                LiteralExpression(IntLiteral(4))
            )
        );


        "1 + (2 + (3 + (4 + 5)))",
        BinaryExpression(
            LiteralExpression(IntLiteral(1)),
            Add,
            BinaryExpression(
                LiteralExpression(IntLiteral(2)),
                Add,
                BinaryExpression(
                    LiteralExpression(IntLiteral(3)),
                    Add,
                    BinaryExpression(
                        LiteralExpression(IntLiteral(4)),
                        Add,
                        LiteralExpression(IntLiteral(5))
                    )
                )
            )
        )
    ] |> createExpressionTestData

