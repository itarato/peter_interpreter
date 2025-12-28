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
    Number(f64),
    Dot,
    Print,
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
    String(String),
    Eof,
    UnexpectedCharacterError(usize),
    UnterminatedStringError(usize),
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
            Self::Print => "PRINT",
            Self::Eof => "EOF",
            // --
            Self::UnexpectedCharacterError(_) => unimplemented!(),
            Self::UnterminatedStringError(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn new(kind: TokenKind, lexeme: &'a str) -> Self {
        Self { kind, lexeme }
    }

    pub(crate) fn is_error(&self) -> bool {
        match self.kind {
            TokenKind::UnexpectedCharacterError(_) => true,
            TokenKind::UnterminatedStringError(_) => true,
            _ => false,
        }
    }

    pub(crate) fn dump_short(&self) {
        match self.kind {
            TokenKind::UnexpectedCharacterError(line) => {
                eprintln!(
                    "[line {}] Error: Unexpected character: {}",
                    line + 1,
                    self.lexeme
                )
            }
            TokenKind::UnterminatedStringError(line) => {
                eprintln!("[line {}] Error: Unterminated string.", line + 1,)
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
