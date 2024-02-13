#[cfg(test)]
mod tests {
    use std::rc::Rc;

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
        let a = global.define("a");
        let b = global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        let c = first_local.define("c");
        let d = first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local);
        let e = second_local.define("e");
        let f = second_local.define("f");

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
        global.define("a");
        global.define("b");

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
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(global);
        local.define("c");
        local.define("d");

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
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

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

    #[test]
    fn test_define_resolve_builtins() {
        let expected_symbols = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Builtin,
                index: 0,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Builtin,
                index: 1,
            },
            Symbol {
                name: "e".to_owned(),
                scope: SymbolScope::Builtin,
                index: 2,
            },
            Symbol {
                name: "f".to_owned(),
                scope: SymbolScope::Builtin,
                index: 3,
            },
        ];

        let mut global = SymbolTable::new();
        for (i, symbol) in expected_symbols.iter().enumerate() {
            global.define_builtin(i, &symbol.name);
        }

        let first_local = SymbolTable::new_enclosed(global.clone());
        let second_local = SymbolTable::new_enclosed(first_local.clone());

        for table in [global, first_local, second_local].iter_mut() {
            for expected in expected_symbols.iter() {
                let actual = table
                    .resolve(&expected.name)
                    .expect("no symbol found in table");
                assert_eq!(expected.clone(), *actual);
            }
        }
    }

    #[test]
    fn test_resolve_free() {
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
        let expected_free_symbols_first = [];

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
                name: "c".to_owned(),
                scope: SymbolScope::Free,
                index: 0,
            },
            Symbol {
                name: "d".to_owned(),
                scope: SymbolScope::Free,
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
        let expected_free_symbols_second = [
            Rc::new(Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Local,
                index: 0,
            }),
            Rc::new(Symbol {
                name: "d".to_owned(),
                scope: SymbolScope::Local,
                index: 1,
            }),
        ];

        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        for expected in expected_symbols_first.into_iter() {
            let actual = first_local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
        assert_eq!(first_local.free_symbols, expected_free_symbols_first);

        for expected in expected_symbols_second.into_iter() {
            let actual = second_local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
        assert_eq!(second_local.free_symbols, expected_free_symbols_second);
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let expected_symbols = [
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Free,
                index: 0,
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
        let expected_unresolvable = ["b", "d"];

        let mut global = SymbolTable::new();
        global.define("a");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        for expected in expected_symbols.into_iter() {
            let actual = second_local
                .resolve(&expected.name)
                .expect("no symbol found in table");
            assert_eq!(expected, *actual);
        }
        for name in expected_unresolvable.into_iter() {
            assert_eq!(second_local.resolve(name), None);
        }
    }
}
