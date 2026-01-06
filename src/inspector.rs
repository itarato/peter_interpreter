use std::collections::HashSet;

use log::debug;

use crate::common::Error;

const SCOPE_FLAG_FN: u8 = 0b0001;
const SCOPE_FLAG_CLASS: u8 = 0b0010;
const SCOPE_FLAG_INIT_FN: u8 = 0b0101;

pub(crate) struct Inspector {
    static_scope_level: Vec<u8>,
    declared_scope_vars: Vec<HashSet<String>>,
}

impl Inspector {
    pub(crate) fn new() -> Self {
        Self {
            static_scope_level: vec![],
            declared_scope_vars: vec![HashSet::new()],
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.static_scope_level.push(0);
        self.declared_scope_vars.push(HashSet::new());
    }

    pub(crate) fn leave_scope(&mut self) {
        self.static_scope_level.pop();
        self.declared_scope_vars.pop();
    }

    pub(crate) fn enter_init_fn_scope(&mut self) {
        self.static_scope_level.push(SCOPE_FLAG_INIT_FN);
    }

    pub(crate) fn enter_fn_scope(&mut self) {
        self.static_scope_level.push(SCOPE_FLAG_FN);
    }

    pub(crate) fn leave_fn_scope(&mut self) {
        self.static_scope_level.pop();
    }

    pub(crate) fn enter_class_scope(&mut self) {
        self.static_scope_level.push(SCOPE_FLAG_CLASS);
    }

    pub(crate) fn leave_class_scope(&mut self) {
        self.static_scope_level.pop();
    }

    pub(crate) fn is_global_scope(&self) -> bool {
        self.static_scope_level.is_empty()
    }

    pub(crate) fn is_fn_scope(&self) -> bool {
        self.static_scope_level
            .iter()
            .any(|scope_flag| (scope_flag & SCOPE_FLAG_FN) > 0)
    }

    pub(crate) fn is_class_scope(&self) -> bool {
        self.static_scope_level
            .iter()
            .any(|scope_flag| (scope_flag & SCOPE_FLAG_CLASS) > 0)
    }

    pub(crate) fn is_class_init_fn_scope(&self) -> bool {
        debug!("PARSE SCOPE: {:?}", self.static_scope_level);
        for i in (0..self.static_scope_level.len()).rev() {
            if (self.static_scope_level[i] & SCOPE_FLAG_FN) > 0 {
                return self.static_scope_level.len() >= 2
                    && (self.static_scope_level[i] & SCOPE_FLAG_INIT_FN) == SCOPE_FLAG_INIT_FN
                    && (self.static_scope_level[i - 2] & SCOPE_FLAG_CLASS) > 0;
            }
        }

        false
    }

    pub(crate) fn declare_variable(&mut self, name: String) -> Result<(), Error> {
        if !self.is_global_scope() && self.declared_scope_vars.last().unwrap().contains(&name) {
            return Err(format!("Error: Variable {} already declared.", &name).into());
        }

        self.declared_scope_vars.last_mut().unwrap().insert(name);
        Ok(())
    }
}
