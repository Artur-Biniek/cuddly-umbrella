module CuddlyUmbrella.Compiler.Ast

type Program = FunctionDefinition list * Statement list

and FunctionDefinition = FunctionDefinition of Identifier

and Statement = Identifier

and Identifier = string


