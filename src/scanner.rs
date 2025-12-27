use crate::{common::Error, str_reader::StrReader, token::Token, token::TokenKind};

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
            self.reader.drop_until(|c| c.is_whitespace());

            match self.reader.peek1().clone() {
                Some(c) => match c {
                    '(' => tokens.push(Token::new(TokenKind::LeftParen, self.reader.pop(1))),
                    ')' => tokens.push(Token::new(TokenKind::RightParen, self.reader.pop(1))),
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

    fn tokenize<'a>(source: &str) -> Vec<TokenKind> {
        Scanner::new(source)
            .scan()
            .unwrap()
            .iter()
            .map(|token| token.kind)
            .collect()
    }
}
