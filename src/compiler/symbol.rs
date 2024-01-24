use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    Global,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    store: HashMap<String, Rc<Symbol>>,
    num_definitions: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.clone(),
            scope: SymbolScope::Global,
            index: self.num_definitions,
        });
        self.store.insert(name.clone(), symbol.clone());
        self.num_definitions += 1;
        symbol.clone()
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        self.store.get(name).cloned()
    }
}
