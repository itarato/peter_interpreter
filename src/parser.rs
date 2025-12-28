use crate::{
    ast::{AstExpression, AstStatement, AstStatementList, AstValue, BinaryOp, UnaryOp},
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
                    TokenKind::String(_)
                    | TokenKind::Number(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Nil
                    | TokenKind::Minus
                    | TokenKind::Bang
                    | TokenKind::LeftParen => AstStatement::Expr(self.parse_expression()?),

                    TokenKind::Eof => break,

                    _ => unimplemented!("Token {:?} not implemented yet for parsing", token),
                },
                None => break,
            };

            statements.push(node);
        }

        Ok(AstStatementList(statements))
    }

    fn parse_expression(&mut self) -> Result<AstExpression, Error> {
        let mut expr: AstExpression = self.parse_single_expression_unit()?;

        loop {
            match self.reader.peek() {
                Some(token) => match BinaryOp::from_token_kind(&token.kind) {
                    Some(op) => {
                        self.pop_and_assert(&token.kind)?;
                        let rhs = self.parse_single_expression_unit()?;
                        expr = AstExpression::Binary {
                            op,
                            lhs_expr: Box::new(expr),
                            rhs_expr: Box::new(rhs),
                        };
                        // TODO: Precedence.
                    }
                    None => return Ok(expr),
                },
                None => return Ok(expr),
            }
        }
    }

    fn parse_single_expression_unit(&mut self) -> Result<AstExpression, Error> {
        match &self.reader.pop().unwrap().kind {
            TokenKind::LeftParen => {
                let expr = self.parse_expression()?;
                self.pop_and_assert(&TokenKind::RightParen)?;

                Ok(AstExpression::Group {
                    expr: Box::new(expr),
                })
            }
            TokenKind::Bang => {
                let expr = self.parse_single_expression_unit()?;
                Ok(AstExpression::Unary {
                    op: UnaryOp::Bang,
                    expr: Box::new(expr),
                })
            }
            TokenKind::Minus => {
                let expr = self.parse_single_expression_unit()?;
                Ok(AstExpression::Unary {
                    op: UnaryOp::Minus,
                    expr: Box::new(expr),
                })
            }
            TokenKind::String(s) => Ok(AstExpression::Literal {
                value: AstValue::Str(s.clone()),
            }),
            TokenKind::Number(n) => Ok(AstExpression::Literal {
                value: AstValue::Number(*n),
            }),
            TokenKind::True => Ok(AstExpression::Literal {
                value: AstValue::Boolean(true),
            }),
            TokenKind::False => Ok(AstExpression::Literal {
                value: AstValue::Boolean(false),
            }),
            TokenKind::Nil => Ok(AstExpression::Literal {
                value: AstValue::Nil,
            }),

            other => unimplemented!(
                "Token {:?} not implemented yet for expression parsing",
                other
            ),
        }
    }

    fn pop_and_assert(&mut self, kind_expected: &TokenKind) -> Result<(), Error> {
        match self.reader.pop() {
            Some(token) => {
                if &token.kind == kind_expected {
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

#[cfg(test)]
mod test {
    use crate::{ast::AstStatementList, parser::Parser, scanner::Scanner};

    #[test]
    fn test_empty() {
        assert_eq!("".to_string(), parse("").dump());
    }

    #[test]
    fn test_single_literal() {
        assert_eq!("12.0".to_string(), parse("12").dump());
        assert_eq!("true".to_string(), parse("true").dump());
        assert_eq!("hi".to_string(), parse("\"hi\"").dump());
    }

    #[test]
    fn test_unary_op() {
        assert_eq!("(! true)".to_string(), parse("!true").dump());
        assert_eq!("(! 1.0)".to_string(), parse("!1").dump());
        assert_eq!("(- 9.12)".to_string(), parse("- 9.12").dump());
    }

    #[test]
    fn test_binary_op() {
        assert_eq!("(+ 1.0 2.0)".to_string(), parse("1 + 2").dump());
        assert_eq!("(> 1.0 2.0)".to_string(), parse("1 > 2").dump());
        assert_eq!("(<= 1.0 2.0)".to_string(), parse("1 <= 2").dump());
        assert_eq!("(== 1.0 2.0)".to_string(), parse("1 == 2").dump());
    }

    #[test]
    fn test_group() {
        assert_eq!("(group 2.0)".to_string(), parse("(2)").dump());
        assert_eq!("(group (> 1.0 2.0))".to_string(), parse("(1 > 2)").dump());
    }

    #[test]
    fn test_multiple_ops() {
        assert_eq!("(+ (+ 1.0 2.0) 3.0)".to_string(), parse("1 + 2 + 3").dump());
        assert_eq!(
            "(+ (+ 1.0 2.0) (- 3.0))".to_string(),
            parse("1 + 2 + -3").dump()
        );
    }

    fn parse(source: &str) -> AstStatementList {
        let tokens = Scanner::new(source).scan().unwrap();
        Parser::new(&tokens[..]).parse().unwrap()
    }
}
