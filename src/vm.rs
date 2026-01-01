use std::{collections::HashMap, rc::Rc, usize};

use crate::{
    ast::{AstExpression, AstFn, AstValue},
    common::Error,
};

pub(crate) struct Scope {
    vars: HashMap<String, AstValue>,
    functions: HashMap<String, Rc<AstFn>>,
    is_local_scope: bool,
}

impl Scope {
    fn new_local_scope() -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_local_scope: true,
        }
    }

    fn new_function_scope() -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_local_scope: false,
        }
    }
}

pub(crate) struct VM {
    scope_stack: Vec<Scope>,
}

impl VM {
    pub(crate) fn new() -> Self {
        Self {
            scope_stack: vec![Scope::new_local_scope()],
        }
    }

    pub(crate) fn load_variable(&self, name: &str) -> Option<&AstValue> {
        for scope in self.scope_stack.iter().rev() {
            if scope.vars.contains_key(name) {
                return scope.vars.get(name);
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
        }

        Err(format!("Error: variable not found in any scope: {}", name).into())
    }

    pub(crate) fn push_local_scope(&mut self) {
        self.scope_stack.push(Scope::new_local_scope());
    }

    pub(crate) fn push_function_scope(&mut self) {
        self.scope_stack.push(Scope::new_function_scope());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.scope_stack.pop().unwrap();
    }

    pub(crate) fn establish_fn(&mut self, fn_def: Rc<AstFn>) {
        self.scope_stack
            .last_mut()
            .unwrap()
            .functions
            .insert(fn_def.name.clone(), fn_def.clone());

        self.scope_stack.last_mut().unwrap().vars.insert(
            fn_def.name.clone(),
            AstValue::FnRef {
                function: fn_def.clone(),
                is_return: false,
            },
        );
    }

    pub(crate) fn eval_internal_fn(
        &mut self,
        name: &str,
        args: &Vec<AstExpression>,
    ) -> Result<AstValue, Error> {
        match name {
            "clock" => {
                if args.len() != 0 {
                    Err(format!(
                            "Err: Incorrect number of arguments for the method {}. Expected 0. Got: {}.",
                            name,
                            args.len()
                        ).into())
                } else {
                    Ok(AstValue::Number {
                        value: std::time::SystemTime::now()
                            .duration_since(std::time::UNIX_EPOCH)
                            .unwrap()
                            .as_secs_f64(),
                        line: usize::MAX,
                        is_return: false,
                    })
                }
            }
            _ => Err(format!("Error: Function <{}> not found.", name).into()),
        }
    }

    pub(crate) fn is_in_function_scope(&self) -> bool {
        self.scope_stack.iter().any(|scope| !scope.is_local_scope)
    }
}
