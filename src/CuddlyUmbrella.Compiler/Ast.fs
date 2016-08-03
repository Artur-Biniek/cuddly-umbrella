module CuddlyUmbrella.Compiler.Ast

type Program = FunctionDefinition list * Statement list

and FunctionDefinition = FunctionDefinition of Identifier

and Statement =
    | VariableDeclaration of Identifier * TypeSpec * Expression option

and Identifier = string

and TypeSpec =
    | Void
    | Int
    | Float
    | String
    | Bool

 and Expression =
    | IdentExpression of Identifier
    | LiteralExpression of Literal

and Literal =
    | IntLiteral of System.Int32
    | FloatLiteral of System.Single
    | StringLiteral of System.String
    | BoolLiteral of System.Boolean