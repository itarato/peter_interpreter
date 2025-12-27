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
    Bang,
    Equal,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    String,
    Eof,
    ScanningError(usize),
}

impl TokenKind {
    fn to_upper_snake_case(&self) -> &str {
        match self {
            Self::Identifier => "IDENTIFIER",
            Self::And => "AND",
            Self::Class => "CLASS",
            Self::Else => "ELSE",
            Self::False => "FALSE",
            Self::For => "FOR",
            Self::Fun => "FUN",
            Self::If => "IF",
            Self::Nil => "NIL",
            Self::Or => "OR",
            Self::Return => "RETURN",
            Self::Super => "SUPER",
            Self::This => "THIS",
            Self::True => "TRUE",
            Self::Var => "VAR",
            Self::While => "WHILE",
            Self::Number => "NUMBER",
            Self::Dot => "DOT",
            Self::LeftParen => "LEFT_PAREN",
            Self::RightParen => "RIGHT_PAREN",
            Self::LeftBrace => "LEFT_BRACE",
            Self::RightBrace => "RIGHT_BRACE",
            Self::Semicolon => "SEMICOLON",
            Self::Comma => "COMMA",
            Self::Plus => "PLUS",
            Self::Minus => "MINUS",
            Self::Star => "STAR",
            Self::Bang => "BANG",
            Self::Equal => "EQUAL",
            Self::BangEqual => "BANG_EQUAL",
            Self::EqualEqual => "EQUAL_EQUAL",
            Self::LessEqual => "LESS_EQUAL",
            Self::GreaterEqual => "GREATER_EQUAL",
            Self::Less => "LESS",
            Self::Greater => "GREATER",
            Self::Slash => "SLASH",
            Self::String => "STRING",
            Self::Eof => "EOF",
            // --
            Self::ScanningError(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Literal {
    Str(String),
    Num(f64),
}

impl Literal {
    fn to_string_short(&self) -> String {
        match self {
            Self::Str(s) => s.clone(),
            Self::Num(n) => n.to_string(),
        }
    }
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

    pub(crate) fn dump_short(&self) {
        match self.kind {
            TokenKind::ScanningError(line) => {
                println!("[line {}] Unexpected character: {}", line + 1, self.lexeme)
            }
            _ => println!(
                "{} {} {}",
                self.kind.to_upper_snake_case(),
                self.lexeme,
                self.literal
                    .as_ref()
                    .map(|v| v.to_string_short())
                    .unwrap_or("null".into())
            ),
        }
    }
}
