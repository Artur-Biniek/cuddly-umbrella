module CuddlyUmbrella.Compiler.Tests.AstTests

open NUnit.Framework
open CuddlyUmbrella.Compiler

[<Test>]
let ``program should be list of function definitions followed by main code``() =
    let functions = List.empty<Ast.FunctionDefinition>
    let statements = List.empty<Ast.Statement>
    let program: Ast.Program = (functions, statements)

    let (a: Ast.FunctionDefinition list, b: Ast.Statement list) = program

    Assert.That(a = functions && b = statements)
    
