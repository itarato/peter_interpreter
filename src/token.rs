#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TokenKind {
    Identifier,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Number,
    Dot,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    String,
    Eof,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Str(String),
    Num(f64),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
    pub(crate) literal: Option<Literal>,
}

impl<'a> Token<'a> {
    pub(crate) fn new(kind: TokenKind, lexeme: &'a str) -> Self {
        Self {
            kind,
            lexeme,
            literal: None,
        }
    }

    pub(crate) fn new_with_literal(kind: TokenKind, lexeme: &'a str, literal: Literal) -> Self {
        Self {
            kind,
            lexeme,
            literal: Some(literal),
        }
    }
}
