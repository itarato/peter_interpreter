use crate::{
    ast::{AstClass, AstExpression, AstFn, AstValue},
    common::Error,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc, u64, usize};

#[derive(Debug)]
struct VarData {
    value: AstValue,
    // Auto increment id. Marking creation order.
    id: u64,
}

#[derive(Debug)]
pub(crate) struct Scope {
    vars: HashMap<String, VarData>,
    functions: HashMap<
        String,
        (
            Rc<RefCell<Scope>>,
            Rc<AstFn>,
            u64, /* Declaration ID. */
        ),
    >,
    is_local_scope: bool,
    parent: Option<Rc<RefCell<Scope>>>,
    child_scope_max_allowed_var_id: u64,
    classes: HashMap<String, Rc<AstClass>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_local_scope: true,
            parent: None,
            child_scope_max_allowed_var_id: u64::MAX,
            classes: HashMap::new(),
        }
    }

    fn new_fn_scope(scope_barrier: u64) -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_local_scope: false,
            parent: None,
            child_scope_max_allowed_var_id: scope_barrier,
            classes: HashMap::new(),
        }
    }

    pub(crate) fn new_fn_scope_with_parent(scope_barrier: u64, parent: Rc<RefCell<Scope>>) -> Self {
        Self {
            vars: HashMap::new(),
            functions: HashMap::new(),
            is_local_scope: false,
            parent: Some(parent),
            child_scope_max_allowed_var_id: scope_barrier,
            classes: HashMap::new(),
        }
    }
}

struct ScopeIter {
    scope: Option<Rc<RefCell<Scope>>>,
}

impl Iterator for ScopeIter {
    type Item = Rc<RefCell<Scope>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.scope.is_none() {
            None
        } else {
            let current = self.scope.clone();

            self.scope = {
                let inner = self.scope.as_ref().unwrap().borrow();
                inner.parent.clone()
            };

            current
        }
    }
}

pub(crate) struct VM {
    scopes: Vec<Rc<RefCell<Scope>>>,
    id_provider: u64,
}

impl VM {
    pub(crate) fn new() -> Self {
        Self {
            scopes: vec![Rc::new(RefCell::new(Scope::new()))],
            id_provider: 0,
        }
    }

    pub(crate) fn get_unique_id(&mut self) -> u64 {
        let id = self.id_provider;
        self.id_provider += 1;
        id
    }

    pub(crate) fn current_scope(&self) -> &Rc<RefCell<Scope>> {
        self.scopes.last().unwrap()
    }

    fn scope_iter(&self) -> ScopeIter {
        ScopeIter {
            scope: Some(self.current_scope().clone()),
        }
    }

    fn make_scope_iter(scope: &Rc<RefCell<Scope>>) -> ScopeIter {
        ScopeIter {
            scope: Some(scope.clone()),
        }
    }

    pub(crate) fn load_variable(&self, name: &str) -> Option<AstValue> {
        self.load_variable_from_scope(name, &self.current_scope())
    }

    pub(crate) fn load_variable_from_scope(
        &self,
        name: &str,
        scope: &Rc<RefCell<Scope>>,
    ) -> Option<AstValue> {
        let mut max_allowed_var_id = u64::MAX;

        for scope in Self::make_scope_iter(scope) {
            let scope_ref = scope.borrow();

            if scope_ref.vars.contains_key(name) {
                let var_data = scope_ref.vars.get(name).unwrap();
                if var_data.id > max_allowed_var_id {
                    continue;
                }

                return Some(var_data.value.clone());
            }

            max_allowed_var_id = max_allowed_var_id.min(scope_ref.child_scope_max_allowed_var_id);
        }

        None
    }

    pub(crate) fn declare_variable(&mut self, name: String, value: AstValue) {
        let id = self.get_unique_id();
        self.current_scope()
            .borrow_mut()
            .vars
            .insert(name, VarData { value, id });
    }

    pub(crate) fn declare_instance_variable(
        &mut self,
        scope: &Rc<RefCell<Scope>>,
        name: String,
        value: AstValue,
    ) {
        let id = self.get_unique_id();
        scope.borrow_mut().vars.insert(name, VarData { value, id });
    }

    pub(crate) fn update_variable(&mut self, name: String, value: AstValue) -> Result<(), Error> {
        let mut max_allowed_var_id = u64::MAX;

        for scope in self.scope_iter() {
            let mut scope_ref_mut = scope.borrow_mut();

            if scope_ref_mut.vars.contains_key(&name) {
                let var_data = scope_ref_mut.vars.get_mut(&name).unwrap();
                if var_data.id > max_allowed_var_id {
                    continue;
                }

                var_data.value = value;
                return Ok(());
            }

            max_allowed_var_id =
                max_allowed_var_id.min(scope_ref_mut.child_scope_max_allowed_var_id);
        }

        Err(format!("Error: variable not found in any scope: {}", name).into())
    }

    pub(crate) fn push_local_scope(&mut self) {
        let mut new_scope = Scope::new();
        new_scope.parent = Some(self.current_scope().clone());

        self.scopes.pop();
        self.scopes.push(Rc::new(RefCell::new(new_scope)));
    }

    pub(crate) fn push_function_scope(&mut self, scope: Rc<RefCell<Scope>>, scope_barrier: u64) {
        let mut new_scope = Scope::new_fn_scope(scope_barrier);
        new_scope.parent = Some(scope);

        self.scopes.push(Rc::new(RefCell::new(new_scope)));
    }

    pub(crate) fn pop_local_scope(&mut self) {
        let new_scope = {
            let inner = self.current_scope().borrow();
            inner.parent.clone().unwrap()
        };

        self.scopes.pop();
        self.scopes.push(new_scope);
    }

    pub(crate) fn pop_function_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn establish_fn(&mut self, fn_def: Rc<AstFn>) {
        let id = self.get_unique_id();

        self.current_scope().borrow_mut().functions.insert(
            fn_def.name.clone(),
            (self.current_scope().clone(), fn_def.clone(), id),
        );

        self.current_scope().borrow_mut().vars.insert(
            fn_def.name.clone(),
            VarData {
                value: AstValue::FnRef {
                    function: fn_def.clone(),
                    is_return: false,
                    scope: self.current_scope().clone(),
                    scope_barrier: id,
                },
                id,
            },
        );
    }

    pub(crate) fn establish_class(&mut self, class_def: Rc<AstClass>) {
        let id = self.get_unique_id();

        // TODO: Review is fn-scope is appropriate. Likely it is - and `fn-` should be renamed to `hard-` or something.
        let mut class_scope = Scope::new_fn_scope(id);
        class_scope.parent = Some(self.current_scope().clone());

        self.current_scope()
            .borrow_mut()
            .classes
            .insert(class_def.name.clone(), class_def.clone());

        self.current_scope().borrow_mut().vars.insert(
            class_def.name.clone(),
            VarData {
                value: AstValue::ClassRef {
                    class: class_def.clone(),
                    is_return: false,
                    scope: Rc::new(RefCell::new(class_scope)),
                },
                id,
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
        self.scope_iter()
            .any(|scope| !scope.borrow().is_local_scope)
    }
}
