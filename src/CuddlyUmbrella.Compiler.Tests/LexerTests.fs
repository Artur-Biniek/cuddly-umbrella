module CuddlyUmbrella.Compiler.Tests.ParserTests

open CuddlyUmbrella.Compiler
open Microsoft.FSharp.Text.Lexing
open NUnit.Framework
open Parser


let stringToTokens input =
    let lexbuf = LexBuffer<_>.FromString input
    let tokens = Lexer.getAllTokens lexbuf
    tokens

let mapToSimpleTestData (input: (string * Parser.token list) list) = 
    input |> List.map (fun (a, b) -> TestCaseData(a).Returns(b @ [EOF]))


[<Test>]
[<TestCaseSource("correctIdentifierTestCases")>]
let ``tokenizes identifiers correctly`` input = stringToTokens input

[<Test>]
let ``ignores spaces, tabs, and line endings``() =
    let tokens = stringToTokens "             \t   \t  \t   \t\t\r\n\t   \n"
    let areEqual = (tokens = [EOF]);
    Assert.That(areEqual)

[<Test>]
[<TestCaseSource("correctPunctuationsTestCases")>]
let ``tokenizes punctuations correctly`` input = stringToTokens input

[<Test>]
[<TestCaseSource("correctOperatorsTestCases")>]
let ``tokenizes operators correctly`` input = stringToTokens input

[<Test>]
[<TestCaseSource("correctTypesTestCases")>]
let ``tokenizes type specificators correctly`` input = stringToTokens input

[<Test>]
[<TestCaseSource("correctKeywordsTestCases")>]
let ``tokenizes keywords correctly`` input = stringToTokens input

[<Test>]
[<TestCaseSource("correctLiteralsTestCases")>]
let ``tokenizes literals correctly`` input = stringToTokens input

[<Test>]
[<TestCaseSource("correctMixedTestCases")>]
let ``tokenizes mixed parts correctly`` input = stringToTokens input


let correctIdentifierTestCases = 
    [
        // just small letters
        "id",
        [IDENT("id")];

        // just capital letters
        "IDENTIFIER",
        [IDENT("IDENTIFIER")];

        // mix of small and upper with multiple whitespaces
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

    ] |> mapToSimpleTestData

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

        // recognizes left and right braces
        "{{ } }   { {}",
        [LBRACE; LBRACE; RBRACE; RBRACE; LBRACE; LBRACE; RBRACE];

        // recognizes double quotes
        "\" \"\" \"    \"",
        [DBLQUOTE; DBLQUOTE; DBLQUOTE; DBLQUOTE; DBLQUOTE];

        // mixed punctiations
        ",:;(){}\"",
        [COMMA; COLON; SEMICOLON; LPAREN; RPAREN; LBRACE; RBRACE; DBLQUOTE]

    ] |> mapToSimpleTestData

let correctOperatorsTestCases = 
    [
        // recognizes pluses
        "+  +   + ++",
        [PLUS; PLUS; PLUS; PLUS; PLUS];

        // recognizes minuses
        "-- - -",
        [MINUS; MINUS; MINUS; MINUS];

        // recognizes equals
        "= == =",
        [EQ; EQ; EQ; EQ];

        // recognizes less than
        "< << <",
        [LT; LT; LT; LT]

        // recognizes less than or equal
        "<= <=<= <=<=",
        [LEQ; LEQ; LEQ; LEQ; LEQ]

        // recognizes greater than
        "   >>   > >  ",
        [GT; GT; GT; GT]

        // recognizes greater than or equal
        ">=>=>= >=",
        [GEQ; GEQ; GEQ; GEQ]

        // recognizes left arror (assignment)
        "<- <- <-<-",
        [LARROW; LARROW; LARROW; LARROW]

        // mixed operators
        "-+=<=<>>=<<-",
        [MINUS; PLUS; EQ; LEQ; LT; GT; GEQ; LT; LARROW]

    ] |> mapToSimpleTestData

let correctTypesTestCases =
    [
        // recognizes void
        "void void",
        [TYPE_SPEC("void"); TYPE_SPEC("void")];

        // recognizes int
        "int int int",
        [TYPE_SPEC("int"); TYPE_SPEC("int"); TYPE_SPEC("int")];

        // recognizes float
        "float     float",
        [TYPE_SPEC("float"); TYPE_SPEC("float")];

        // recognizes bool
        "bool   bool",
        [TYPE_SPEC("bool"); TYPE_SPEC("bool")];

        // recognizes string
        "string string             string",
        [TYPE_SPEC("string"); TYPE_SPEC("string"); TYPE_SPEC("string")];

        // mixed types and identifiers
        "void voidvoid int intvoidint boolintvoid bool floatstring float string",
        [TYPE_SPEC("void"); IDENT("voidvoid"); TYPE_SPEC("int"); IDENT("intvoidint"); IDENT("boolintvoid"); 
         TYPE_SPEC("bool"); IDENT("floatstring"); TYPE_SPEC("float"); TYPE_SPEC("string")];

    ] |> mapToSimpleTestData

let correctKeywordsTestCases =
    [
        // recognizes def keyword
        "def def    def",
        [DEF; DEF; DEF];

        // recognizes return keyword
        "return      return  return",
        [RETURN; RETURN; RETURN];

        // recognizes var keyword
        "var               var  ",
        [VAR; VAR]

        // recognizes if keyword
        "if   if   if",
        [IF; IF; IF];

        // recognizes then keyword
        "then then        then",
        [THEN; THEN; THEN];

        // recognizes else keyword
        "   else  else ",
        [ELSE; ELSE];

        // mixed keywords
        "def return var if then else",
        [DEF; RETURN; VAR; IF; THEN; ELSE];

        // mixed not keywords
        "Def reTurn vAr IF thEN ElsE",
        [IDENT("Def"); IDENT("reTurn"); IDENT("vAr"); IDENT("IF"); IDENT("thEN"); IDENT("ElsE")]

    ] |> mapToSimpleTestData


let correctLiteralsTestCases =
    [
        // recognizes bools
        "true false false true",
        [BOOL(true); BOOL(false); BOOL(false); BOOL(true)];

        // but only smallercase bools are literals
        "True False truE FLASE",
        [IDENT("True"); IDENT("False"); IDENT("truE"); IDENT("FLASE")];

        // recognizes ints
        "0 1   2 34 89",
        [INT(0); INT(1); INT(2); INT(34); INT(89)];

        // does not include minus in integer literal
        "-4 -45",
        [MINUS; INT(4); MINUS; INT(45)];

        // recognizes floats
        "1.0 14.56    87.234",
        [FLOAT(1.0f); FLOAT(14.56f); FLOAT(87.234f)];

        // does not include minus in float literals
        "-123.456",
        [MINUS; FLOAT(123.456f)]

    ] |> mapToSimpleTestData

let correctMixedTestCases =
    [
        "if i = j then return;",
        [IF; IDENT("i"); EQ; IDENT("j"); THEN; RETURN; SEMICOLON];

        "def factorial(x: int):int { return x; }",
        [DEF; IDENT("factorial"); LPAREN; IDENT("x"); COLON; TYPE_SPEC("int"); RPAREN; COLON; TYPE_SPEC("int"); 
         LBRACE; RETURN; IDENT("x"); SEMICOLON; RBRACE;]

    ] |> mapToSimpleTestData