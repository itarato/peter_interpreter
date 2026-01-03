pub(crate) struct Inspector {
    static_scope_level: u64,
    declared_scope_vars: Vec<Vec<String>>,
}

impl Inspector {
    pub(crate) fn new() -> Self {
        Self {
            static_scope_level: 0,
            declared_scope_vars: vec![vec![]],
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.static_scope_level += 1;
        self.declared_scope_vars.push(vec![]);
    }

    pub(crate) fn leave_scope(&mut self) {
        self.static_scope_level -= 1;
        self.declared_scope_vars.pop();
    }

    pub(crate) fn is_global_scope(&self) -> bool {
        self.static_scope_level == 0
    }
}
