use crate::{ast::AstStatementList, common::Error};

pub(crate) struct Interpreter {
    statements: AstStatementList,
}

impl Interpreter {
    pub(crate) fn new(statements: AstStatementList) -> Self {
        Self { statements }
    }

    pub(crate) fn evaluate(&mut self) -> Result<(), Error> {
        Ok(())
    }
}
