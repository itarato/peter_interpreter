use crate::{
    common::{Error, string_token_to_literal},
    str_reader::StrReader,
    token::{Literal, Token, TokenKind},
};

pub(crate) struct Scanner<'a> {
    reader: StrReader<'a>,
}

impl<'a> Scanner<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            reader: StrReader::new(source),
        }
    }

    pub(crate) fn scan(&mut self) -> Result<Vec<Token<'a>>, Error> {
        let mut tokens = vec![];

        loop {
            self.reader.drop_while(|c| c.is_whitespace());

            match self.reader.peek1().clone() {
                Some(c) => match c {
                    '(' => tokens.push(Token::new(TokenKind::LeftParen, self.reader.pop(1))),
                    ')' => tokens.push(Token::new(TokenKind::RightParen, self.reader.pop(1))),
                    '{' => tokens.push(Token::new(TokenKind::LeftBrace, self.reader.pop(1))),
                    '}' => tokens.push(Token::new(TokenKind::RightBrace, self.reader.pop(1))),
                    ';' => tokens.push(Token::new(TokenKind::Semicolon, self.reader.pop(1))),
                    ',' => tokens.push(Token::new(TokenKind::Comma, self.reader.pop(1))),
                    '+' => tokens.push(Token::new(TokenKind::Plus, self.reader.pop(1))),
                    '-' => tokens.push(Token::new(TokenKind::Minus, self.reader.pop(1))),
                    '*' => tokens.push(Token::new(TokenKind::Star, self.reader.pop(1))),
                    '/' => tokens.push(Token::new(TokenKind::Slash, self.reader.pop(1))),
                    '.' => tokens.push(Token::new(TokenKind::Dot, self.reader.pop(1))),
                    '!' => {
                        if let Some("!=") = self.reader.peek(2) {
                            tokens.push(Token::new(TokenKind::BangEqual, self.reader.pop(2)));
                        } else {
                            tokens.push(Token::new(TokenKind::Bang, self.reader.pop(1)));
                        }
                    }
                    '=' => {
                        if let Some("==") = self.reader.peek(2) {
                            tokens.push(Token::new(TokenKind::EqualEqual, self.reader.pop(2)));
                        } else {
                            tokens.push(Token::new(TokenKind::Equal, self.reader.pop(1)));
                        }
                    }
                    '<' => {
                        if let Some("<=") = self.reader.peek(2) {
                            tokens.push(Token::new(TokenKind::LessEqual, self.reader.pop(2)));
                        } else {
                            tokens.push(Token::new(TokenKind::Less, self.reader.pop(1)));
                        }
                    }
                    '>' => {
                        if let Some(">=") = self.reader.peek(2) {
                            tokens.push(Token::new(TokenKind::GreaterEqual, self.reader.pop(2)));
                        } else {
                            tokens.push(Token::new(TokenKind::Greater, self.reader.pop(1)));
                        }
                    }
                    '"' => {
                        if let Some(s) = self.reader.pop_until(1, |c| c == '"') {
                            tokens.push(Token::new_with_literal(
                                TokenKind::String,
                                s,
                                Literal::Str(string_token_to_literal(s)),
                            ));
                        } else {
                            return Err(format!(
                                "Incomplete string token at pos {}",
                                self.reader.pos
                            )
                            .into());
                        }
                    }
                    'a'..='z' | '_' => {
                        let raw = self
                            .reader
                            .pop_while(|c| c.is_ascii_alphanumeric() || c == '_');

                        match raw {
                            "and" => tokens.push(Token::new(TokenKind::And, raw)),
                            "class" => tokens.push(Token::new(TokenKind::Class, raw)),
                            "else" => tokens.push(Token::new(TokenKind::Else, raw)),
                            "false" => tokens.push(Token::new(TokenKind::False, raw)),
                            "for" => tokens.push(Token::new(TokenKind::For, raw)),
                            "fun" => tokens.push(Token::new(TokenKind::Fun, raw)),
                            "if" => tokens.push(Token::new(TokenKind::If, raw)),
                            "nil" => tokens.push(Token::new(TokenKind::Nil, raw)),
                            "or" => tokens.push(Token::new(TokenKind::Or, raw)),
                            "return" => tokens.push(Token::new(TokenKind::Return, raw)),
                            "super" => tokens.push(Token::new(TokenKind::Super, raw)),
                            "this" => tokens.push(Token::new(TokenKind::This, raw)),
                            "true" => tokens.push(Token::new(TokenKind::True, raw)),
                            "var" => tokens.push(Token::new(TokenKind::Var, raw)),
                            "while" => tokens.push(Token::new(TokenKind::While, raw)),
                            other => tokens.push(Token::new(TokenKind::Identifier, other)),
                        }
                    }
                    '1'..='9' => {
                        let raw = self.reader.pop_while(|c| c.is_ascii_digit() || c == '.');
                        match raw.parse::<f64>() {
                            Ok(v) => tokens.push(Token::new_with_literal(
                                TokenKind::Number,
                                raw,
                                Literal::Num(v),
                            )),
                            Err(err) => {
                                return Err(format!(
                                    "Invalid number format <{}> at pos {} (error: {})",
                                    raw, self.reader.pos, err
                                )
                                .into());
                            }
                        }
                    }
                    other => {
                        return Err(format!(
                            "Unrecognized char <{}> at pos {}",
                            other, self.reader.pos
                        )
                        .into());
                    }
                },
                None => break,
            }
        }

        tokens.push(Token::new(crate::token::TokenKind::Eof, ""));

        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use crate::{scanner::Scanner, token::TokenKind};

    #[test]
    fn test_empty() {
        assert_eq!(vec![TokenKind::Eof], tokenize(""));
    }

    #[test]
    fn test_whitespaces() {
        assert_eq!(vec![TokenKind::Eof], tokenize("   "));
        assert_eq!(vec![TokenKind::Eof], tokenize("   \n   \t\r\n "));
    }

    #[test]
    fn test_basic_one_char_tokens() {
        assert_eq!(
            vec![TokenKind::LeftParen, TokenKind::RightParen, TokenKind::Eof],
            tokenize(" (\t)\n")
        );
    }

    #[test]
    fn test_signs() {
        assert_eq!(
            vec![
                TokenKind::BangEqual,
                TokenKind::EqualEqual,
                TokenKind::Equal,
                TokenKind::Less,
                TokenKind::LessEqual,
                TokenKind::Eof
            ],
            tokenize("!====<<=")
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            vec![
                TokenKind::Identifier,
                TokenKind::Equal,
                TokenKind::String,
                TokenKind::Eof
            ],
            tokenize("x = \"abc\"")
        );
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            vec![
                TokenKind::Var,
                TokenKind::Identifier,
                TokenKind::Equal,
                TokenKind::Nil,
                TokenKind::Eof
            ],
            tokenize("var x = nil")
        );
    }

    #[test]
    fn test_numbers() {
        assert_eq!(
            vec![
                TokenKind::Number,
                TokenKind::Number,
                TokenKind::Dot,
                TokenKind::Number,
                TokenKind::Number,
                TokenKind::Eof
            ],
            tokenize("12 12.34 .12 12.")
        );
    }

    fn tokenize<'a>(source: &str) -> Vec<TokenKind> {
        Scanner::new(source)
            .scan()
            .unwrap()
            .iter()
            .map(|token| token.kind)
            .collect()
    }
}
