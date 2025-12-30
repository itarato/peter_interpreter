use crate::{
    ast::{AstStatementList, AstValue},
    common::Error,
    vm::VM,
};

pub(crate) struct Interpreter<'a> {
    statements: AstStatementList,
    vm: VM<'a>,
}

impl<'a> Interpreter<'a> {
    pub(crate) fn new(statements: AstStatementList) -> Self {
        Self {
            statements,
            vm: VM::new(),
        }
    }

    pub(crate) fn evaluate(&'a mut self) -> Result<Option<AstValue>, Error> {
        self.statements.eval(&mut self.vm)
    }
}
