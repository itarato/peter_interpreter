use std::collections::HashMap;

use crate::{
    ast::{AstFn, AstValue},
    common::Error,
};

pub(crate) struct Scope<'a> {
    vars: HashMap<String, AstValue>,
    functions: HashMap<String, &'a AstFn>,
    is_soft: bool,
}

impl<'a> Scope<'a> {
    fn new_soft() -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_soft: true,
        }
    }
}

pub(crate) struct VM<'a> {
    scope_stack: Vec<Scope<'a>>,
}

impl<'a> VM<'a> {
    pub(crate) fn new() -> Self {
        Self {
            scope_stack: vec![Scope::new_soft()],
        }
    }

    pub(crate) fn load_variable(&self, name: &str) -> Option<&AstValue> {
        for scope in self.scope_stack.iter().rev() {
            if scope.vars.contains_key(name) {
                return scope.vars.get(name);
            }

            if !scope.is_soft {
                break;
            }
        }

        None
    }

    pub(crate) fn establish_variable(&mut self, name: String, value: AstValue) {
        self.scope_stack
            .last_mut()
            .expect("Missing top stack")
            .vars
            .insert(name, value);
    }

    pub(crate) fn update_variable(&mut self, name: String, value: AstValue) -> Result<(), Error> {
        for scope in self.scope_stack.iter_mut().rev() {
            if scope.vars.contains_key(&name) {
                *scope.vars.get_mut(&name).unwrap() = value;
                return Ok(());
            }

            if !scope.is_soft {
                break;
            }
        }

        Err(format!("Error: variable not found in any scope: {}", name).into())
    }

    pub(crate) fn push_scope(&mut self) {
        self.scope_stack.push(Scope::new_soft());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scope_stack.pop().unwrap();
    }

    pub(crate) fn establish_fn(&mut self, fn_def: &'a AstFn) {
        self.scope_stack
            .last_mut()
            .unwrap()
            .functions
            .insert(fn_def.name.clone(), fn_def);
    }

    pub(crate) fn load_fn(&mut self, name: &str) -> Option<&&'a AstFn> {
        for scope in self.scope_stack.iter().rev() {
            if scope.functions.contains_key(name) {
                return scope.functions.get(name);
            }

            if !scope.is_soft {
                break;
            }
        }

        None
    }
}
