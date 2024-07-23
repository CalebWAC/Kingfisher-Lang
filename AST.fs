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

and ImmutableBinding = (Identifier * Type option) * Expr list
and MutableBinding = (Identifier * Type option) * Expr list
and Reassignment = Identifier (* * BinaryArithmeticOperator option *) * Expr list

and Parameter =
    | Unspecified of Identifier
    | Specified of (Identifier option * Identifier) * Type option 
and FunctionDeclaration = (Identifier * Parameter list) * Expression list

and EntityBinding = Identifier * Identifier list
and SystemDeclaration = (Identifier list * SystemClassification option) * Expr list // change

and SystemClassification = string // change
    (* | Awake
    | Start
    | Update 
    | End *)

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

and Type = TypeKeyWord * CollectionType option

and TypeKeyWord = string // fix

and CollectionType = string //fix

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

and IfExpr = (IfExpress * IfExpress list option) * Expression list  // change Expression to statement
and IfExpress = (IfCondition * Expression option) * Expression list // change!
and IfCondition =
    | Expr of Expression
    | LetStatement of Identifier  * Expression

and ForExpr = (((Identifier option * Identifier) * Expression) * Expression option) * Expression list // change to statement
and WhileExpr = Expression * Expression list // change to statement
and MatchExpr = Identifier * (Identifier * Identifier) list // change first two to expression, last to statement

and Expression = 
    | BinaryLogicalExpr of (Expression * BinaryLogicalOperator) * Expression
    | BinaryComparisonExpr of (Expression * BinaryComparisonOperator) * Expression
    | BinaryArithmeticExpr of (Expression * BinaryArithmeticOperator) * Expression
    | UnaryExpr of UnaryOperator * Expression
    | IdentifierExpr of Identifier
    | FunctionCallExpr of Identifier * Expression list
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

and Literal = 
    | IntLiteral of int
    | FloatLiteral of float
    | BoolLiteral of bool
    | StringLiteral of string
    | RuneLiteral of char
    | VoidLiteral
    | CollectionLiteral of CollectionLiteral
    | RecordLiteral of string option * (Identifier * Expression) list
    | TupleLiteral of Expression list

and CollectionLiteral =
    | ArrayLiteral of ArrayType * Expression list
    | MapLiteral of (Expression * Expression) list

and ArrayType = string option // change to array/set