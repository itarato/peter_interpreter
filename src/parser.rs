use crate::{
    ast::{AstExpression, AstStatement, AstStatementList},
    common::{Error, Reader},
    token::{Token, TokenKind},
};

pub(crate) struct Parser<'a> {
    reader: Reader<'a, Token<'a>>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(tokens: &'a [Token]) -> Self {
        Self {
            reader: Reader::new(tokens),
        }
    }

    pub(crate) fn parse(mut self) -> Result<AstStatementList, Error> {
        let mut statements = vec![];

        loop {
            let node = match self.reader.peek() {
                Some(token) => match token.kind {
                    TokenKind::String
                    | TokenKind::Number
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Nil
                    | TokenKind::Minus
                    | TokenKind::Bang
                    | TokenKind::LeftParen => AstStatement::Expr(self.parse_expression()?),
                    _ => unimplemented!("Token {:?} not implemented yet for parsing", token),
                },
                None => break,
            };

            statements.push(node);
        }

        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<AstExpression, Error> {
        match self.reader.peek().unwrap().kind {
            TokenKind::LeftParen => {
                self.pop_and_assert(TokenKind::LeftParen)?;
                let expr = self.parse_expression()?;
                self.pop_and_assert(TokenKind::RightParen)?;

                Ok(AstExpression::Group {
                    expr: Box::new(expr),
                })
            }
            other => unimplemented!(
                "Token {:?} not implemented yet for expression parsing",
                other
            ),
        }
    }

    fn pop_and_assert(&mut self, kind_expected: TokenKind) -> Result<(), Error> {
        match self.reader.pop() {
            Some(token) => {
                if token.kind == kind_expected {
                    Ok(())
                } else {
                    Err(format!(
                        "Unexpected token. Expected: {:?}. Actual: {:?}.",
                        kind_expected, token
                    )
                    .into())
                }
            }
            None => Err(format!("Not enoughg tokens. Expected: {:?}.", kind_expected).into()),
        }
    }
}
