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

use crate::token::TokenKind;

#[derive(Debug)]
pub(crate) enum BinaryOp {
    And,
    Or,
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    EqualEqual,
}

impl BinaryOp {
    pub(crate) fn from_token_kind(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::And => Some(Self::And),
            TokenKind::Or => Some(Self::Or),
            TokenKind::Plus => Some(Self::Plus),
            TokenKind::Minus => Some(Self::Minus),
            TokenKind::Star => Some(Self::Star),
            TokenKind::Slash => Some(Self::Slash),
            TokenKind::Less => Some(Self::Less),
            TokenKind::LessEqual => Some(Self::LessEqual),
            TokenKind::Greater => Some(Self::Greater),
            TokenKind::GreaterEqual => Some(Self::GreaterEqual),
            TokenKind::EqualEqual => Some(Self::EqualEqual),
            _ => None,
        }
    }

    fn dump(&self) -> String {
        match self {
            Self::And => "&&".into(),
            Self::Or => "||".into(),
            Self::Plus => "+".into(),
            Self::Minus => "-".into(),
            Self::Star => "*".into(),
            Self::Slash => "/".into(),
            Self::Less => "<".into(),
            Self::LessEqual => "<=".into(),
            Self::Greater => ">".into(),
            Self::GreaterEqual => ">=".into(),
            Self::EqualEqual => "==".into(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum UnaryOp {
    Minus,
    Bang,
}

impl UnaryOp {
    fn dump(&self) -> String {
        match self {
            Self::Minus => "-".into(),
            Self::Bang => "!".into(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AstValue {
    Str(String),
    Number(f64),
    Boolean(bool),
    Nil,
}

impl AstValue {
    fn dump(&self) -> String {
        match self {
            Self::Str(s) => s.clone(),
            Self::Number(v) => format!("{:?}", v),
            Self::Boolean(v) => format!("{:?}", v),
            Self::Nil => String::from("nil"),
        }
    }
}

#[derive(Debug)]
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

impl AstExpression {
    fn dump(&self) -> String {
        match self {
            Self::Literal { value } => value.dump(),
            Self::Unary { op, expr } => format!("({} {})", op.dump(), expr.dump()),
            Self::Binary {
                op,
                lhs_expr,
                rhs_expr,
            } => format!("({} {} {})", op.dump(), lhs_expr.dump(), rhs_expr.dump()),
            Self::Group { expr } => format!("(group {})", expr.dump()),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AstStatement {
    Expr(AstExpression),
}

impl AstStatement {
    fn dump(&self) -> String {
        match self {
            Self::Expr(expr) => expr.dump(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct AstStatementList(pub(crate) Vec<AstStatement>);

impl AstStatementList {
    pub(crate) fn dump(&self) -> String {
        self.0
            .iter()
            .map(|stmt| stmt.dump())
            .collect::<Vec<_>>()
            .join("")
    }
}
