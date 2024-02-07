#[cfg(test)]
mod tests {
    use crate::compiler::symbol::{Symbol, SymbolScope, SymbolTable};

    #[test]
    fn test_define() {
        let expected_a = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::Global,
            index: 0,
        };
        let expected_b = Symbol {
            name: "b".to_string(),
            scope: SymbolScope::Global,
            index: 1,
        };
        let expected_c = Symbol {
            name: "c".to_string(),
            scope: SymbolScope::Local,
            index: 0,
        };
        let expected_d = Symbol {
            name: "d".to_string(),
            scope: SymbolScope::Local,
            index: 1,
        };
        let expected_e = Symbol {
            name: "e".to_string(),
            scope: SymbolScope::Local,
            index: 0,
        };
        let expected_f = Symbol {
            name: "f".to_string(),
            scope: SymbolScope::Local,
            index: 1,
        };

        let mut global = SymbolTable::new();
        let a = global.define("a".to_string());
        let b = global.define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        let c = first_local.define("c".to_string());
        let d = first_local.define("d".to_string());

        let mut second_local = SymbolTable::new_enclosed(first_local);
        let e = second_local.define("e".to_string());
        let f = second_local.define("f".to_string());

        assert_eq!(*a, expected_a);
        assert_eq!(*b, expected_b);

        assert_eq!(*c, expected_c);
        assert_eq!(*d, expected_d);

        assert_eq!(*e, expected_e);
        assert_eq!(*f, expected_f);
    }

    #[test]
    fn test_resolve_global() {
        let expected_symbols = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ];

        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        for expected in expected_symbols.into_iter() {
            let actual = global
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
    }

    #[test]
    fn test_resolve_local() {
        let expected_symbols = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: SymbolScope::Global,
                index: 1,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "d".to_owned(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ];

        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut local = SymbolTable::new_enclosed(global);
        local.define("c".to_string());
        local.define("d".to_string());

        for expected in expected_symbols.into_iter() {
            let actual = local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let expected_symbols_first = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: SymbolScope::Global,
                index: 1,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "d".to_owned(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ];

        let expected_symbols_second = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".to_owned(),
                scope: SymbolScope::Global,
                index: 1,
            },
            Symbol {
                name: "e".to_owned(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "f".to_owned(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ];

        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());
        first_local.define("d".to_string());

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        for expected in expected_symbols_first.into_iter() {
            let actual = first_local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }

        for expected in expected_symbols_second.into_iter() {
            let actual = second_local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
    }
}
