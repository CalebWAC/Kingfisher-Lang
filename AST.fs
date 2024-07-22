module AST 

type Program = Statement list

and Statement = 
    | Expression of Expr
    | Binding of Binding
    | TypeDeclaration of TypeDeclaration

and Identifier = string

and Binding = 
    | ImmutableBinding of ImmutableBinding
    | MutableBinding of MutableBinding
    | FunctionDeclaration of FunctionDeclaration
    | EntityBinding of EntityBinding
    | SystemDeclaration of SystemDeclaration
    | Reassignment of Reassignment

and ImmutableBinding = Identifier * Type option * Expr list
and MutableBinding = Identifier * Type option * Expr list
and Reassignment = Identifier * BinaryArithmeticOperator option * Expr list

and Parameter = Identifier option * Identifier * Type option 
and FunctionDeclaration = Identifier * Parameter list * Expr list

and EntityBinding = Identifier * Identifier list
and SystemDeclaration = Identifier list * SystemClassification * Expr list

and SystemClassification = 
    | Awake
    | Start
    | Update 
    | End

and TypeDeclaration =
    | RecordDeclaration of RecordDeclaration
    | UnionDeclaration of UnionDeclaration
    | ComponentDeclaration of ComponentDeclaration
    | TypeAlias of TypeAlias
    | Extension of Extension

and RecordDeclaration = Identifier * bool list * Identifier list * Type list
and ComponentDeclaration = Identifier * Identifier list * Type list
and TypeAlias = Identifier * Type
and Extension = Identifier * Statement list

and UnionDeclaration = Identifier * UnionCase list
and UnionCase = 
    | Single of Identifier
    | Multiple of Identifier * Type list

and Type = TypeKeyWord of CollectionType option

and TypeKeyWord = 
    | I8
    | I16
    | Int
    | I64
    | Float
    | Float64
    | String
    | Bool
    | Rune

and CollectionType = 
    | Array
    | Set

and BinaryOperator =
    | BinaryArithmeticOperator of BinaryArithmeticOperator
    | BinaryLogicalOperator of BinaryLogicalOperator
    | BinaryComparisonOperator of BinaryComparisonOperator

and BinaryArithmeticOperator =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Exp

and BinaryLogicalOperator =
    | And
    | Or

and BinaryComparisonOperator =
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessEqual
    | GreaterEqual

and UnaryOperator =
    | Not
    | Negative

and Expr =
    | IfExpr of IfExpr
    | ForExpr of ForExpr
    | WhileExpr of WhileExpr
    | MatchExpr of MatchExpr
    | Expression of Expression

and IfExpr = IfExpress * IfExpress option * Else option
and IfExpress = Expression * LetStatement option * Expression option * Statement list
and LetStatement = Identifier * Expression
and Else = Statement list

and ForExpr = Identifier option * Identifier * Expr * Expression option * Statement list
and WhileExpr = Expr * Statement list
and MatchExpr = Expr * Expression list * Statement list

and Expression = 
    | BinaryExpr of BinaryExpr
    | UnaryExpr of char * Expression // Change this to operator
    | IdentifierExpr of Identifier
    | FunctionCallExpr of Expression * Expression list
    | ArrayExpr of Identifier * Expression
    | DataAccessExpr of Identifier * Identifier
    | ComponentAccessExpr of Identifier * Identifier
    | RangeExpr of Expression * RangeType * Expression
    | LiteralExpr of Literal

and RangeType = 
    | Exclusive
    | Inclusive
    | ExclusiveStep of Expression
    | InclusiveStep of Expression

and BinaryExpr =
    | BinaryLogicalExpr of Expression * BinaryLogicalOperator * Expression
    | BinaryComparisonExpr of Expression * BinaryComparisonOperator * Expression
    | BinaryArithmeticOperator of Expression * BinaryArithmeticOperator * Expr

and Literal = 
    | IntLiteral of int
    | FloatLiteral of float
    | BoolLiteral of bool
    | StringLiteral of string
    | RuneLiteral of char
    | VoidLiteral
    | CollectionLiteral of CollectionLiteral
    | RecordLiteral of Identifier list * Expression list
    | TupleLiteral of Expression list

and CollectionLiteral =
    | ArrayLiteral of ArrayType * Expr list
    | MapLiteral of Expr list * Expr list

and ArrayType = 
    | Array
    | Set