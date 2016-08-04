module CuddlyUmbrella.Compiler.Ast

type Program = FunctionDefinition list * Statement list

and FunctionDefinition = FunctionDefinition of Identifier

and Statement =
    | VariableDeclaration of Identifier * TypeSpec * Expression option

    override this.ToString() = sprintf "%A" this  

and Identifier = string

and TypeSpec =
    | Void
    | Int
    | Float
    | String
    | Bool

and Expression =
    | IdentifierExpression of Identifier
    | LiteralExpression of Literal
    | FunctionCallExpression of Identifier * Expression list
    | BinaryExpression of Expression * BinaryOperator * Expression

    override this.ToString() = sprintf "%A" this       

and Literal =
    | IntLiteral of System.Int32
    | FloatLiteral of System.Single
    | StringLiteral of System.String
    | BoolLiteral of System.Boolean

and BinaryOperator =
    | Add
    | Substract
    | Multiply
    | Divide
