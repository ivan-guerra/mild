use crate::object::{Object, SegFlags, Segment, Sizes, SymbolType};
use std::collections::HashMap;

const ADDR_SIZE: usize = 4;
const PAGE_SIZE: usize = 0x1000;
const TEXT_SEGMENT: &str = ".text";
const DATA_SEGMENT: &str = ".data";
const BSS_SEGMENT: &str = ".bss";
const BASE_TEXT_ADDR: usize = 0x1000;

macro_rules! next_aligned {
    ($addr:expr, $align:expr) => {
        ($addr + $align - 1) & !($align - 1)
    };
}

type ObjFilename = String;
type SegMap = HashMap<ObjFilename, HashMap<Segment, usize>>;

fn merge_segments(
    segments: &[(ObjFilename, Segment)],
    segname: &str,
    start_addr: usize,
    segmap: &mut SegMap,
) -> Segment {
    let name = segname.to_string();
    let desc = segments
        .iter()
        .fold(SegFlags::empty(), |acc, seg| acc | seg.1.desc);

    let mut prev_addr = start_addr;
    for (obj_filename, seg) in segments {
        let curr_addr = next_aligned!(prev_addr, ADDR_SIZE);
        segmap
            .entry((*obj_filename).clone())
            .or_default()
            .insert((*seg).clone(), curr_addr);
        prev_addr = curr_addr + seg.len;
    }
    let len = prev_addr - start_addr;

    Segment {
        name,
        len,
        address: start_addr as u32,
        desc,
    }
}

fn create_common_blks(objects: &[Object]) -> Option<Segment> {
    // Map undefined symbol names with nonzero values (common blocks) to their maximum length
    let mut common_blk_lens = HashMap::new();
    for object in objects {
        for symbol in &object.symtab {
            if matches!(symbol.symtype, SymbolType::Undefined) && symbol.value != 0 {
                let value = common_blk_lens.entry(symbol.name.clone()).or_insert(0);
                *value = (*value).max(symbol.value);
            }
        }
    }

    let common_blk_total_len = common_blk_lens.values().sum::<u32>() as usize;
    if common_blk_total_len == 0 {
        None
    } else {
        Some(Segment {
            name: BSS_SEGMENT.to_string(),
            len: common_blk_total_len,
            address: 0,
            desc: SegFlags::READ | SegFlags::WRITE,
        })
    }
}

pub fn allocate(objects: &[Object]) -> anyhow::Result<Object> {
    // Find all segments with the given name across all objects creating a vector of (object
    // filename, segment) tuples
    let find_segments = |segname: &str| -> Vec<(String, Segment)> {
        objects
            .iter()
            .flat_map(|obj| {
                obj.segments
                    .iter()
                    .filter(|seg| seg.name == segname)
                    .map(|seg| (obj.filename.clone(), seg.clone()))
            })
            .collect()
    };

    let mut segmap: SegMap = HashMap::new();

    let text_segments = find_segments(TEXT_SEGMENT);
    let text_seg = merge_segments(&text_segments, TEXT_SEGMENT, BASE_TEXT_ADDR, &mut segmap);

    let data_segments = find_segments(DATA_SEGMENT);
    let data_start = next_aligned!(BASE_TEXT_ADDR + text_seg.len, PAGE_SIZE);
    let data_seg = merge_segments(&data_segments, DATA_SEGMENT, data_start, &mut segmap);

    let mut bss_segments = find_segments(BSS_SEGMENT);
    let bss_start = next_aligned!(data_start + data_seg.len, ADDR_SIZE);
    if let Some(bss_common_seg) = create_common_blks(objects) {
        bss_segments.push(("common_blks.mild".to_string(), bss_common_seg));
    }
    let bss_seg = merge_segments(&bss_segments, BSS_SEGMENT, bss_start, &mut segmap);

    Ok(Object {
        filename: "a.out".to_string(),
        sizes: Sizes {
            num_segments: 3,
            ..Default::default()
        },
        segments: vec![text_seg, data_seg, bss_seg],
        ..Default::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{SegNum, Symbol, SymbolType};

    // Helper function to create test objects
    fn create_test_object(filename: &str, segments: Vec<Segment>, symbols: Vec<Symbol>) -> Object {
        Object {
            filename: filename.to_string(),
            sizes: Sizes {
                num_segments: segments.len(),
                num_symbols: symbols.len(),
                num_relocs: 0,
            },
            segments,
            symtab: symbols,
            relocs: vec![],
            data: vec![],
        }
    }

    fn create_test_segment(name: &str, address: u32, len: usize, desc: SegFlags) -> Segment {
        Segment {
            name: name.to_string(),
            address,
            len,
            desc,
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
    fn create_common_blks_no_undefined_symbols_none() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
            vec![create_test_symbol(
                "defined_sym",
                0x1000,
                SegNum::Segment(1),
                SymbolType::Defined,
            )],
        )];

        let result = create_common_blks(&objects);
        assert!(result.is_none());
    }

    #[test]
    fn create_common_blks_undefined_symbols_zero_value_none() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
            vec![create_test_symbol(
                "undef_sym",
                0,
                SegNum::AbsOrUndef,
                SymbolType::Undefined,
            )],
        )];

        let result = create_common_blks(&objects);
        assert!(result.is_none());
    }

    #[test]
    fn create_common_blks_single_common_block_some() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
            vec![create_test_symbol(
                "common_var",
                0x80,
                SegNum::AbsOrUndef,
                SymbolType::Undefined,
            )],
        )];

        let result = create_common_blks(&objects);
        assert!(result.is_some());
        let bss_seg = result.unwrap();
        assert_eq!(bss_seg.name, ".bss");
        assert_eq!(bss_seg.len, 0x80);
        assert_eq!(bss_seg.address, 0);
        assert_eq!(bss_seg.desc, SegFlags::READ | SegFlags::WRITE);
    }

    #[test]
    fn create_common_blks_multiple_common_blocks_some() {
        let objects = vec![create_test_object(
            "obj1.mild",
            vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
            vec![
                create_test_symbol(
                    "common_var1",
                    0x80,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                ),
                create_test_symbol(
                    "common_var2",
                    0x40,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                ),
            ],
        )];

        let result = create_common_blks(&objects);
        assert!(result.is_some());
        let bss_seg = result.unwrap();
        assert_eq!(bss_seg.name, ".bss");
        assert_eq!(bss_seg.len, 0x80 + 0x40);
        assert_eq!(bss_seg.address, 0);
        assert_eq!(bss_seg.desc, SegFlags::READ | SegFlags::WRITE);
    }

    #[test]
    fn create_common_blks_duplicate_names_max_value_some() {
        let objects = vec![
            create_test_object(
                "obj1.mild",
                vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
                vec![create_test_symbol(
                    "common_var",
                    0x80,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
            create_test_object(
                "obj2.mild",
                vec![create_test_segment(".text", 0x1000, 0x100, SegFlags::READ)],
                vec![create_test_symbol(
                    "common_var",
                    0x120,
                    SegNum::AbsOrUndef,
                    SymbolType::Undefined,
                )],
            ),
        ];

        let result = create_common_blks(&objects);
        assert!(result.is_some());
        let bss_seg = result.unwrap();
        assert_eq!(bss_seg.name, ".bss");
        assert_eq!(bss_seg.len, 0x120); // Should use maximum value
        assert_eq!(bss_seg.address, 0);
        assert_eq!(bss_seg.desc, SegFlags::READ | SegFlags::WRITE);
    }

    #[test]
    fn merge_segments_empty_segments_ok() {
        let segments = vec![];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0);
        assert_eq!(result.desc, SegFlags::empty());
        assert!(segmap.is_empty());
    }

    #[test]
    fn merge_segments_single_segment_ok() {
        let segment = create_test_segment(".text", 0x2000, 0x100, SegFlags::READ);
        let segments = vec![("obj1.mild".to_string(), segment.clone())];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0x100);
        assert_eq!(result.desc, SegFlags::READ);

        assert!(segmap.contains_key("obj1.mild"));
        assert_eq!(segmap["obj1.mild"][&segment], 0x1000);
    }

    #[test]
    fn merge_segments_multiple_segments_ok() {
        let segment1 = create_test_segment(".text", 0x2000, 0x100, SegFlags::READ);
        let segment2 = create_test_segment(".text", 0x3000, 0x80, SegFlags::READ);
        let segments = vec![
            ("obj1.mild".to_string(), segment1.clone()),
            ("obj2.mild".to_string(), segment2.clone()),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0x180);
        assert_eq!(result.desc, SegFlags::READ);

        assert!(segmap.contains_key("obj1.mild"));
        assert!(segmap.contains_key("obj2.mild"));
        assert_eq!(segmap["obj1.mild"][&segment1], 0x1000);
        assert_eq!(segmap["obj2.mild"][&segment2], 0x1100);
    }

    #[test]
    fn merge_segments_alignment_ok() {
        let segment1 = create_test_segment(".text", 0x2000, 0x101, SegFlags::READ); // Odd length
        let segment2 = create_test_segment(".text", 0x3000, 0x80, SegFlags::READ);
        let segments = vec![
            ("obj1.mild".to_string(), segment1.clone()),
            ("obj2.mild".to_string(), segment2.clone()),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);

        assert!(segmap.contains_key("obj1.mild"));
        assert!(segmap.contains_key("obj2.mild"));
        assert_eq!(segmap["obj1.mild"][&segment1], 0x1000);
        assert_eq!(segmap["obj2.mild"][&segment2], 0x1104); // 0x1000 + 0x101 aligned to next 4-byte boundary
    }

    #[test]
    fn merge_segments_flag_combination_ok() {
        let segment1 = create_test_segment(".data", 0x4000, 0x100, SegFlags::READ);
        let segment2 = create_test_segment(".data", 0x5000, 0x80, SegFlags::WRITE);
        let segment3 = create_test_segment(".data", 0x6000, 0x40, SegFlags::PRESENT);
        let segments = vec![
            ("obj1.mild".to_string(), segment1.clone()),
            ("obj2.mild".to_string(), segment2.clone()),
            ("obj3.mild".to_string(), segment3.clone()),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".data", 0x4000, &mut segmap);

        assert_eq!(result.name, ".data");
        assert_eq!(result.address, 0x4000);
        assert_eq!(
            result.desc,
            SegFlags::READ | SegFlags::WRITE | SegFlags::PRESENT
        );
    }
}
