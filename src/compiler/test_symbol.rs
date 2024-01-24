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

        let mut global = SymbolTable::new();
        let a = global.define("a".to_string());
        let b = global.define("b".to_string());

        assert_eq!(*a, expected_a);
        assert_eq!(*b, expected_b);
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
}
