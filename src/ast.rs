// expression     → literal
//                | unary
//                | binary
//                | grouping ;
// literal        → NUMBER | STRING | "true" | "false" | "nil" ;
// grouping       → "(" expression ")" ;
// unary          → ( "-" | "!" ) expression ;
// binary         → expression operator expression ;
// operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
//                | "+"  | "-"  | "*" | "/" ;

// program -> [statements]
// statement ->
//      | expression
//      | function def
//      | if
//      | if-else

use crate::token::Literal;

pub(crate) type AstStatementList = Vec<AstStatement>;

pub(crate) enum BinaryOp {
    And,
    Or,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub(crate) enum UnaryOp {
    Negate,
}

pub(crate) enum AstValue {
    Str(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

pub(crate) enum AstExpression {
    Literal {
        value: AstValue,
    },
    Unary {
        op: UnaryOp,
        expr: Box<AstExpression>,
    },
    Binary {
        op: BinaryOp,
        lhs_expr: Box<AstExpression>,
        rhs_expr: Box<AstExpression>,
    },
    Group {
        expr: Box<AstExpression>,
    },
}

pub(crate) enum AstStatement {
    Expr(AstExpression),
}
