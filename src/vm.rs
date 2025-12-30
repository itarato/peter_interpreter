use std::{collections::HashMap, usize};

use crate::{
    ast::{AstExpression, AstFn, AstValue},
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

    pub(crate) fn eval_fn(
        &mut self,
        name: &str,
        args: &Vec<AstExpression>,
    ) -> Result<AstValue, Error> {
        let mut it = self.scope_stack.iter().rev();

        let fn_def = loop {
            match it.next() {
                Some(scope) => {
                    if scope.functions.contains_key(name) {
                        break scope.functions.get(name);
                    }

                    if !scope.is_soft {
                        break None;
                    }
                }
                None => break None,
            }
        };

        match fn_def {
            Some(fn_def) => {
                return fn_def.eval(self, args);
            }
            None => match name {
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
                                .as_millis() as f64,
                            line: usize::MAX,
                        })
                    }
                }
                _ => Err(format!("Error: Function <{}> not found.", name).into()),
            },
        }
    }
}
