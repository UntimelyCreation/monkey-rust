use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Rc<SymbolTable>>,
    store: HashMap<String, Rc<Symbol>>,
    pub num_definitions: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> Self {
        Self {
            outer: Some(Rc::new(outer)),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: &str) -> Rc<Symbol> {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope,
            index: self.num_definitions,
        });
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }

    pub fn define_builtin(&mut self, index: usize, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope: SymbolScope::Builtin,
            index,
        });
        self.store.insert(name.to_string(), symbol.clone());
        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.get(name) {
            Some(symbol) => Some(symbol).cloned(),
            None => match &self.outer {
                Some(outer) => outer.resolve(name),
                None => None,
            },
        }
    }
}
