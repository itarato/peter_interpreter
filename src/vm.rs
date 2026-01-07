use log::debug;

use crate::{
    ast::{AstClass, AstExpression, AstFn, AstValue},
    common::Error,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc, u64, usize};

static mut SCOPE_COUNTER: u64 = 0;
fn get_new_scope_id() -> u64 {
    unsafe {
        let id = SCOPE_COUNTER;
        SCOPE_COUNTER += 1;
        id
    }
}

#[derive(Debug)]
pub(crate) enum ScopeKind {
    Local,
    Function(u64),
    Class(u64),
    Instance,
}

impl ScopeKind {
    fn child_scope_max_allowed_var_id(&self) -> u64 {
        match self {
            Self::Class(v) | ScopeKind::Function(v) => *v,
            _ => u64::MAX,
        }
    }

    fn is_local(&self) -> bool {
        match self {
            Self::Local => true,
            _ => false,
        }
    }

    fn is_instance(&self) -> bool {
        match self {
            ScopeKind::Instance => true,
            _ => false,
        }
    }

    fn is_class(&self) -> bool {
        match self {
            ScopeKind::Class(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct VarData {
    value: AstValue,
    // Auto increment id. Marking creation order.
    id: u64,
}

#[derive(Debug)]
pub(crate) struct Scope {
    id: u64,
    vars: HashMap<String, VarData>,
    functions: HashMap<
        String,
        (
            Rc<RefCell<Scope>>,
            Rc<AstFn>,
            u64, /* Declaration ID. */
        ),
    >,
    kind: ScopeKind,
    pub(crate) parent: Option<Rc<RefCell<Scope>>>,
    classes: HashMap<String, Rc<AstClass>>,
}

impl Scope {
    pub(crate) fn new(kind: ScopeKind) -> Self {
        Self {
            id: get_new_scope_id(),
            vars: HashMap::new(),
            functions: HashMap::new(),
            kind,
            parent: None,
            classes: HashMap::new(),
        }
    }

    pub(crate) fn dump_scope_content(&self) {
        self.dump_scope_content_on_level(0);
    }

    fn dump_scope_content_on_level(&self, level: usize) {
        debug!(
            "[Scope #{} (id={}) (kind={:?})][Vars: {:?}]",
            level,
            self.id,
            self.kind,
            self.vars
                .iter()
                .map(|(k, v)| format!("{}({})", k, v.id))
                .collect::<Vec<_>>(),
        );
        if let Some(parent) = &self.parent {
            parent.borrow().dump_scope_content_on_level(level + 1);
        }
    }

    pub(crate) fn with_parent(mut self, parent_scope: Rc<RefCell<Self>>) -> Self {
        self.parent = Some(parent_scope);
        self
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
            scopes: vec![Rc::new(RefCell::new(Scope::new(ScopeKind::Local)))],
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
        self.load_variable_from_scope(name, &self.current_scope(), false)
    }

    pub(crate) fn load_variable_from_scope(
        &self,
        name: &str,
        scope: &Rc<RefCell<Scope>>,
        limit_to_class_scope: bool,
    ) -> Option<AstValue> {
        debug!("LOAD VARIABLE: {}", name);
        scope.borrow().dump_scope_content();

        let mut max_allowed_var_id = u64::MAX;

        for scope in Self::make_scope_iter(scope) {
            let scope_ref = scope.borrow();

            if scope_ref.vars.contains_key(name) {
                let var_data = scope_ref.vars.get(name).unwrap();
                if var_data.id > max_allowed_var_id && !scope_ref.kind.is_instance() {
                    continue;
                }

                return Some(var_data.value.clone());
            }

            // Instance variables are only seeing down to class, and not lower.
            // We cannot fully remove the below class scope since class functions do see
            // that level too.
            if limit_to_class_scope && scope_ref.kind.is_class() {
                break;
            }

            max_allowed_var_id =
                max_allowed_var_id.min(scope_ref.kind.child_scope_max_allowed_var_id());
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
                max_allowed_var_id.min(scope_ref_mut.kind.child_scope_max_allowed_var_id());
        }

        Err(format!("Error: variable not found in any scope: {}", name).into())
    }

    pub(crate) fn push_local_scope(&mut self) {
        let mut new_scope = Scope::new(ScopeKind::Local);
        new_scope.parent = Some(self.current_scope().clone());

        self.scopes.pop();
        self.scopes.push(Rc::new(RefCell::new(new_scope)));
    }

    pub(crate) fn push_function_scope(&mut self, scope: Rc<RefCell<Scope>>, scope_barrier: u64) {
        let mut new_scope = Scope::new(ScopeKind::Function(scope_barrier));
        new_scope.parent = Some(scope);

        self.scopes.push(Rc::new(RefCell::new(new_scope)));
    }

    pub(crate) fn pop_local_scope(&mut self) -> Rc<RefCell<Scope>> {
        let removed_scope = self.current_scope().clone();

        let new_scope = {
            let inner = self.current_scope().borrow();
            inner.parent.clone().unwrap()
        };

        self.scopes.pop();
        self.scopes.push(new_scope);

        removed_scope
    }

    pub(crate) fn pop_function_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn use_scope(&mut self, scope: Rc<RefCell<Scope>>) {
        self.scopes.push(scope);
    }

    pub(crate) fn remove_scope(&mut self) {
        self.scopes.pop();
    }

    pub(crate) fn establish_fn(&mut self, fn_def: Rc<AstFn>) {
        self.establish_fn_in_scope(fn_def, &self.current_scope().clone());
    }

    pub(crate) fn establish_fn_in_scope(&mut self, fn_def: Rc<AstFn>, scope: &Rc<RefCell<Scope>>) {
        let id = self.get_unique_id();

        scope
            .borrow_mut()
            .functions
            .insert(fn_def.name.clone(), (scope.clone(), fn_def.clone(), id));

        scope.borrow_mut().vars.insert(
            fn_def.name.clone(),
            VarData {
                value: AstValue::FnRef {
                    function: fn_def.clone(),
                    is_return: false,
                    scope: scope.clone(),
                    scope_barrier: id,
                    instance_scope: None,
                },
                id,
            },
        );
    }

    pub(crate) fn establish_class(&mut self, class_def: Rc<AstClass>) {
        let id = self.get_unique_id();

        // TODO: Review is fn-scope is appropriate. Likely it is - and `fn-` should be renamed to `hard-` or something.
        let mut class_scope = Scope::new(ScopeKind::Class(id));
        class_scope.parent = Some(self.current_scope().clone());

        self.current_scope()
            .borrow_mut()
            .classes
            .insert(class_def.name.clone(), class_def.clone());

        let scope = Rc::new(RefCell::new(class_scope));

        self.current_scope().borrow_mut().vars.insert(
            class_def.name.clone(),
            VarData {
                value: AstValue::ClassRef {
                    class: class_def.clone(),
                    is_return: false,
                    scope: scope.clone(),
                },
                id,
            },
        );

        for function in &class_def.functions {
            self.establish_fn_in_scope(function.clone(), &scope);
        }
    }

    pub(crate) fn load_class(&self, name: &str) -> Option<Rc<AstClass>> {
        for scope in self.scope_iter() {
            if scope.borrow().classes.contains_key(name) {
                return scope.borrow().classes.get(name).cloned();
            }
        }

        None
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
            .any(|scope| !scope.borrow().kind.is_local())
    }
}
