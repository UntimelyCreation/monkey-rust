use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
    store: HashMap<String, Rc<Symbol>>,
    pub num_definitions: usize,
    pub free_symbols: Vec<Rc<Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> Self {
        Self {
            outer: Some(Rc::new(RefCell::new(outer))),
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
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

    pub fn define_free(&mut self, original: Rc<Symbol>) -> Rc<Symbol> {
        self.free_symbols.push(original.clone());

        let symbol = Rc::new(Symbol {
            name: (*original.name).to_string(),
            index: self.free_symbols.len() - 1,
            scope: SymbolScope::Free,
        });
        self.store.insert(original.name.to_string(), symbol.clone());

        symbol
    }

    pub fn define_function(&mut self, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope: SymbolScope::Function,
            index: 0,
        });
        self.store.insert(name.to_string(), symbol.clone());
        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.get(name) {
            Some(symbol) => Some(symbol).cloned(),
            None => match &self.outer {
                Some(outer) => {
                    let obj = outer.as_ref().borrow_mut().resolve(name)?;

                    if obj.scope == SymbolScope::Global || obj.scope == SymbolScope::Builtin {
                        return Some(obj);
                    }

                    let free = self.define_free(obj);
                    Some(free)
                }
                None => None,
            },
        }
    }
}
