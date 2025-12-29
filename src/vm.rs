use std::collections::HashMap;

use crate::ast::AstValue;

pub(crate) struct Scope {
    vars: HashMap<String, AstValue>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }
}

pub(crate) struct VM {
    scope_stack: Vec<Scope>,
}

impl VM {
    pub(crate) fn new() -> Self {
        Self {
            scope_stack: vec![Scope::new()],
        }
    }

    pub(crate) fn load_variable(&self, name: &str) -> Option<&AstValue> {
        self.scope_stack
            .last()
            .expect("Missing top stack")
            .vars
            .get(name)
    }

    pub(crate) fn store_variable(&mut self, name: String, value: AstValue) {
        self.scope_stack
            .last_mut()
            .expect("Missing top stack")
            .vars
            .insert(name, value);
    }
}
