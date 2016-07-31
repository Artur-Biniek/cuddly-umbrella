module CuddlyUmbrella.Compiler.Ast

type Program = FunctionDefinition list * Statement list

and FunctionDefinition = FunctionDefinition of Identifier

and Statement =
    | VariableDeclaration of Identifier * TypeSpec

and Identifier = string

and TypeSpec =
    | Void
    | Int
    | Float
    | String
    | Bool


