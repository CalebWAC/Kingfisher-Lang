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
    | Reassignment of Reassignment

and ImmutableBinding = (Identifier * Type option) * Expression
and MutableBinding = (Identifier * Type option) * Expression
and Reassignment = (Expression * BinaryArithmeticOperator option) * Expression

and Parameter =
    | Unspecified of Identifier
    | Specified of (Identifier option * Identifier) * Type option 
and FunctionDeclaration = ((Identifier * Parameter list) * Type option) * Statement list

and EntityBinding = Identifier * (Identifier * Expression option) list
and SystemDeclaration = (Identifier list * SystemClassification) * Statement list

and SystemClassification =
    | Awake
    | Start
    | Update 
    | End

and TypeDeclaration =
    | RecordDeclaration of RecordDeclaration
    | UnionDeclaration of UnionDeclaration
    | ComponentDeclaration of ComponentDeclaration
    | SystemDeclaration of SystemDeclaration
    | TypeAlias of TypeAlias
    | Extension of Extension

and RecordDeclaration = Identifier * ((string option * Identifier) * Type) list
and ComponentDeclaration = Identifier * (Identifier * Type) list
and TypeAlias = Identifier * Type
and Extension = Identifier * Statement list

and UnionDeclaration = Identifier * UnionCase list
and UnionCase = 
    | Single of Identifier
    | Multiple of Identifier * Type list

and Type = TypeKeyWord * CollectionType option

and TypeKeyWord =
    | Float
    | Int
    | Bool
    | String
    | Rune
    | Void
    | Map
    | Custom of string
    | Option of TypeKeyWord
    | Any

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
    
and BinaryComparisonOperator =
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessEqual
    | GreaterEqual

and BinaryLogicalOperator =
    | And
    | Or
    
and BinaryShortLogicalOperator =
    | ShortAnd
    | ShortOr

and UnaryOperator =
    | Not
    | Negative

and Expr =
    | IfExpr of IfExpr
    | ForExpr of ForExpr
    | WhileExpr of WhileExpr
    | MatchExpr of MatchExpr
    | Expression of Expression

and IfExpr = (IfExpress * IfExpress list option) * Statement list option  
and IfExpress = (IfCondition * Expression option) * Statement list
and IfCondition =
    | Expr of Expression
    | LetStatement of Identifier

and ForExpr = (((Identifier option * ForVar) * Expression) * Expression option) * Statement list
and ForVar =
    | Identifier of Identifier
    | MapDestructuring of Identifier * Identifier

and WhileExpr = Expression * Statement list 
and MatchExpr = Identifier * ((Expression * Expression option) * Statement) list

and Expression = 
    | BinaryLogicalExpr of Expression * (BinaryLogicalOperator * Expression) list
    | BinaryComparisonExpr of ((Expression * BinaryComparisonOperator) * Expression) * (BinaryShortLogicalOperator * Expression) list
    | BinaryArithmeticExpr of Expression * (BinaryArithmeticOperator * Expression) list
    | UnaryExpr of UnaryOperator * Expression
    | IdentifierExpr of Identifier
    | FunctionCallExpr of Identifier * (Identifier option * Expression) list
    | ArrayExpr of Identifier * Expression
    | DataAccessExpr of Identifier * Expression
    | ComponentAccessExpr of Identifier * Identifier
    | RangeExpr of (Expression * RangeType) * Expression
    | Parenthesis of Expression
    | LiteralExpr of Literal
    | Lambda of Identifier list * Statement list

and RangeType = 
    | Exclusive
    | Inclusive
    | ExclusiveStep of Expression
    | InclusiveStep of Expression    

and Literal = 
    | IntLiteral of int
    | FloatLiteral of float
    | DoubleLiteral of double
    | BoolLiteral of bool
    | StringLiteral of string
    | RuneLiteral of char
    | VoidLiteral of string
    | CollectionLiteral of CollectionLiteral
    | RecordLiteral of string option * (Identifier * Expression) list
    | TupleLiteral of Expression list

and CollectionLiteral =
    | ArrayLiteral of CollectionType * ArrayLiteralForm
    | MapLiteral of (Expression * Expression) list
    
and ArrayLiteralForm =
    | Standard of Expression list
    | Comprehension of ((ForVar * Expression) * Expression option) * Expression