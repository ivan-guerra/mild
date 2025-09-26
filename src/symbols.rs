use crate::object::{Object, Symbol, SymbolType};
use anyhow::bail;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

#[derive(Debug)]
pub struct GlobalSymbol {
    pub filename: String,
    pub symbol: Symbol,
}

impl Display for GlobalSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} defined in {}", self.symbol.name, self.filename)
    }
}

pub type GlobalSymbolTable = HashMap<String, GlobalSymbol>;

pub fn collect_global_symbols(objects: &[Object]) -> anyhow::Result<GlobalSymbolTable> {
    let mut gsymtab: GlobalSymbolTable = HashMap::new();
    let mut undefined_symbols: HashSet<&str> = HashSet::new();

    for object in objects {
        for symbol in &object.symtab {
            match symbol.symtype {
                SymbolType::Undefined => {
                    if !gsymtab.contains_key(&symbol.name) && !symbol.is_common_blk() {
                        undefined_symbols.insert(&symbol.name);
                    }
                }
                SymbolType::Defined => {
                    undefined_symbols.remove(symbol.name.as_str());

                    let global_sym = gsymtab.insert(
                        symbol.name.clone(),
                        GlobalSymbol {
                            filename: object.filename.clone(),
                            symbol: symbol.clone(),
                        },
                    );

                    if let Some(gsym) = global_sym {
                        bail!(
                            "multiple definition of '{}', first defined here '{}'",
                            gsym.symbol.name,
                            gsym.filename,
                        );
                    }
                }
            }
        }
    }

    if !undefined_symbols.is_empty() {
        let undefined_symbols = undefined_symbols.into_iter().collect::<Vec<_>>().join(", ");
        bail!(
            "undefined reference to the following symbols: {}",
            undefined_symbols
        );
    }

    Ok(gsymtab)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{SegNum, Sizes};

    fn create_test_object(filename: &str, symbols: Vec<Symbol>) -> Object {
        Object {
            filename: filename.to_string(),
            sizes: Sizes {
                num_segments: 0,
                num_symbols: symbols.len(),
                num_relocs: 0,
            },
            segments: vec![],
            symtab: symbols,
            relocs: vec![],
            data: vec![],
        }
    }

    fn create_test_symbol(name: &str, value: u32, segnum: SegNum, symtype: SymbolType) -> Symbol {
        Symbol {
            name: name.to_string(),
            value,
            segnum,
            symtype,
        }
    }

    #[test]
    fn collect_global_symbols_empty_objects_ok() {
        let objects = vec![];
        let result = collect_global_symbols(&objects).unwrap();
        assert!(result.is_empty());
    }

    #[test]
    fn collect_global_symbols_single_defined_symbol_ok() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_symbol(
                "main",
                0x1000,
                SegNum::Segment(1),
                SymbolType::Defined,
            )],
        )];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 1);
        assert!(result.contains_key("main"));
        assert_eq!(result["main"].filename, "obj1.mild");
        assert_eq!(result["main"].symbol.name, "main");
        assert_eq!(result["main"].symbol.value, 0x1000);
    }

    #[test]
    fn collect_global_symbols_multiple_defined_symbols_ok() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![
                create_test_symbol("foo", 0x1000, SegNum::Segment(1), SymbolType::Defined),
                create_test_symbol("bar", 0x2000, SegNum::Segment(2), SymbolType::Defined),
            ],
        )];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 2);
        assert!(result.contains_key("foo"));
        assert!(result.contains_key("bar"));
        assert_eq!(result["foo"].filename, "obj1.mild");
        assert_eq!(result["bar"].filename, "obj1.mild");
    }

    #[test]
    fn collect_global_symbols_multiple_objects_defined_symbols_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "foo",
                    0x1000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "bar",
                    0x2000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
        ];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 2);
        assert_eq!(result["foo"].filename, "obj1.mild");
        assert_eq!(result["bar"].filename, "obj2.mild");
    }

    #[test]
    fn collect_global_symbols_undefined_symbol_with_definition_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "external_func",
                    0,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "external_func",
                    0x3000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
        ];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 1);
        assert!(result.contains_key("external_func"));
        assert_eq!(result["external_func"].filename, "obj2.mild");
        assert_eq!(result["external_func"].symbol.value, 0x3000);
    }

    #[test]
    fn collect_global_symbols_only_undefined_symbols_error() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_symbol(
                "missing_func",
                0,
                SegNum::AbsOrUndef,
                SymbolType::Undefined,
            )],
        )];

        let result = collect_global_symbols(&objects);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("undefined reference"));
        assert!(error_msg.contains("missing_func"));
    }

    #[test]
    fn collect_global_symbols_multiple_undefined_symbols_error() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![
                create_test_symbol("func1", 0, SegNum::AbsOrUndef, SymbolType::Undefined),
                create_test_symbol("func2", 0, SegNum::AbsOrUndef, SymbolType::Undefined),
            ],
        )];

        let result = collect_global_symbols(&objects);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("undefined reference"));
        assert!(error_msg.contains("func1"));
        assert!(error_msg.contains("func2"));
    }

    #[test]
    fn collect_global_symbols_duplicate_definition_error() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "duplicate_func",
                    0x1000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "duplicate_func",
                    0x2000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
        ];

        let result = collect_global_symbols(&objects);
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("multiple definition"));
        assert!(error_msg.contains("duplicate_func"));
        assert!(error_msg.contains("obj1.mild"));
    }

    #[test]
    fn collect_global_symbols_mixed_defined_undefined_symbols_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![
                    create_test_symbol(
                        "local_func",
                        0x1000,
                        SegNum::Segment(1),
                        SymbolType::Defined,
                    ),
                    create_test_symbol(
                        "external_func",
                        0,
                        SegNum::AbsOrUndef,
                        SymbolType::Undefined,
                    ),
                ],
            ),
            create_test_object(
                "obj2.mild",
                vec![
                    create_test_symbol(
                        "external_func",
                        0x2000,
                        SegNum::Segment(1),
                        SymbolType::Defined,
                    ),
                    create_test_symbol(
                        "another_func",
                        0x3000,
                        SegNum::Segment(2),
                        SymbolType::Defined,
                    ),
                ],
            ),
        ];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 3);
        assert!(result.contains_key("local_func"));
        assert!(result.contains_key("external_func"));
        assert!(result.contains_key("another_func"));
        assert_eq!(result["local_func"].filename, "obj1.mild");
        assert_eq!(result["external_func"].filename, "obj2.mild");
        assert_eq!(result["another_func"].filename, "obj2.mild");
    }

    #[test]
    fn collect_global_symbols_undefined_zero_value_symbols_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "extern_var",
                    0,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "extern_var",
                    0x0,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
        ];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 1);
        assert!(result.contains_key("extern_var"));
        assert_eq!(result["extern_var"].filename, "obj2.mild");
        assert_eq!(result["extern_var"].symbol.value, 0x0);
    }

    #[test]
    fn collect_global_symbols_undefined_common_blocks_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "common_var",
                    0x80,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "common_var",
                    0x100,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
        ];

        // Common blocks (undefined symbols with non-zero values) should not cause errors
        // They are handled during allocation phase
        let result = collect_global_symbols(&objects);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn collect_global_symbols_same_name_different_types_ok() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_symbol(
                    "symbol",
                    0,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_symbol(
                    "symbol",
                    0x1000,
                    SegNum::Segment(1),
                    SymbolType::Defined,
                )],
            ),
            create_test_object(
                "obj3.mild",
                vec![create_test_symbol(
                    "symbol",
                    0,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
        ];

        let result = collect_global_symbols(&objects).unwrap();
        assert_eq!(result.len(), 1);
        assert!(result.contains_key("symbol"));
        assert_eq!(result["symbol"].filename, "obj2.mild");
        assert_eq!(result["symbol"].symbol.value, 0x1000);
        assert!(matches!(
            result["symbol"].symbol.symtype,
            SymbolType::Defined
        ));
    }
}
