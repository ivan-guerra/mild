use crate::object::{Object, SegFlags, SegNum, Segment, Sizes};
use crate::symbols::GlobalSymbolTable;
use anyhow::bail;
use std::collections::HashMap;

const ADDR_SIZE: usize = 4;
const PAGE_SIZE: usize = 0x1000;
const BSS_SEGMENT: &str = ".bss";
const BASE_ADDR: usize = 0x1000;
const COMMON_BLOCK_OBJECT_NAME: &str = "common_blk";

macro_rules! next_aligned {
    ($addr:expr, $align:expr) => {
        ($addr + $align - 1) & !($align - 1)
    };
}

type ObjFilename = String;
type SegMap = HashMap<ObjFilename, HashMap<SegNum, (SegNum, usize)>>;

fn merge_segments(
    segments: &[(ObjFilename, SegNum, Segment)],
    segname: &str,
    new_segnum: SegNum,
    start_addr: usize,
    segmap: &mut SegMap,
) -> Segment {
    let name = segname.to_string();
    let desc = segments
        .iter()
        .fold(SegFlags::empty(), |acc, seg| acc | seg.2.flags);

    let mut prev_addr = start_addr;
    for (obj_filename, old_segnum, segment) in segments {
        let curr_addr = next_aligned!(prev_addr, ADDR_SIZE);
        segmap
            .entry((*obj_filename).clone())
            .or_default()
            .insert(*old_segnum, (new_segnum, curr_addr));
        prev_addr = curr_addr + segment.len;
    }
    let len = prev_addr - start_addr;

    Segment {
        name,
        len,
        address: start_addr as u32,
        flags: desc,
    }
}

fn create_common_blks(objects: &[Object]) -> Option<Segment> {
    // Map undefined symbol names with nonzero values (common blocks) to their maximum length
    let mut common_blk_lens = HashMap::new();
    for object in objects {
        for symbol in &object.symtab {
            if symbol.is_common_blk() {
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
            flags: SegFlags::READ | SegFlags::WRITE,
        })
    }
}

fn update_symbol_addresses(gsymtab: &mut GlobalSymbolTable, segmap: &SegMap) -> anyhow::Result<()> {
    for (symname, entry) in gsymtab {
        let old_segnum = &entry.symbol.segnum;
        match segmap.get(&entry.filename).and_then(|m| m.get(old_segnum)) {
            Some((new_segnum, addr)) => {
                entry.symbol.segnum = *new_segnum;
                entry.symbol.value += *addr as u32;
            }
            None => {
                bail!(format!(
                    "Failed to find segment mapping for symbol: {} in file: {} with segnum: {:?}",
                    symname, entry.filename, old_segnum
                ));
            }
        }
    }
    Ok(())
}

pub fn allocate(objects: &[Object], gsymtab: &mut GlobalSymbolTable) -> anyhow::Result<Object> {
    type SegGroup = (ObjFilename, SegNum, Segment);
    type SegGroupMap = HashMap<SegFlags, HashMap<ObjFilename, Vec<SegGroup>>>;
    let mut group_by_segflags: SegGroupMap = HashMap::new();
    for object in objects {
        for (segnum, segment) in object.segments.iter().enumerate() {
            group_by_segflags
                .entry(segment.flags)
                .or_default()
                .entry(segment.name.clone())
                .or_default()
                .push((
                    object.filename.clone(),
                    SegNum::Segment(segnum + 1),
                    segment.clone(),
                ));
        }
    }

    // Allocate segments in the order of:
    // 1. Read-only segments (e.g., .text)
    // 2. Read-write segments (e.g., .data)
    // 3. Read-write segments without PRESENT flag (e.g., .bss)
    let alloc_order = [
        SegFlags::READ | SegFlags::PRESENT,
        SegFlags::READ | SegFlags::WRITE | SegFlags::PRESENT,
        SegFlags::READ | SegFlags::WRITE,
    ];
    let bss_flags = SegFlags::READ | SegFlags::WRITE;
    let mut segmap: SegMap = HashMap::new();
    let mut prev_addr = BASE_ADDR;
    let mut output_segs = Vec::new();
    let mut segment_idx = 1;
    for flags in &alloc_order {
        // Special case for BSS or BSS like segments. The BSS segment grouping aligns to the word
        // boundary. All other segment groupings align to the page size boundary.
        if flags == &bss_flags {
            prev_addr = next_aligned!(prev_addr, ADDR_SIZE);
        } else {
            prev_addr = next_aligned!(prev_addr, PAGE_SIZE);
        }

        if let Some(segment_groups) = group_by_segflags.get_mut(flags) {
            for (segname, segs) in segment_groups.iter_mut() {
                if segname == BSS_SEGMENT {
                    let common_blk_seg = create_common_blks(objects);
                    if let Some(common_bss_seg) = common_blk_seg {
                        segs.push((
                            COMMON_BLOCK_OBJECT_NAME.to_string(),
                            SegNum::Segment(0),
                            common_bss_seg,
                        ));
                    }
                }

                let curr_addr = next_aligned!(prev_addr, ADDR_SIZE);
                let new_segnum = SegNum::Segment(segment_idx);
                let merged_seg = merge_segments(segs, segname, new_segnum, curr_addr, &mut segmap);

                prev_addr = curr_addr + merged_seg.len;
                output_segs.push(merged_seg);
            }
            segment_idx += 1;
        }
    }

    // Update symbol addresses to match the allocated segments
    update_symbol_addresses(gsymtab, &segmap)?;

    Ok(Object {
        filename: "a.out".to_string(),
        sizes: Sizes {
            num_segments: 3,
            ..Default::default()
        },
        segments: output_segs,
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
            flags: desc,
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
        assert_eq!(bss_seg.flags, SegFlags::READ | SegFlags::WRITE);
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
        assert_eq!(bss_seg.flags, SegFlags::READ | SegFlags::WRITE);
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
        assert_eq!(bss_seg.flags, SegFlags::READ | SegFlags::WRITE);
    }

    #[test]
    fn merge_segments_empty_segments_ok() {
        let segments = vec![];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", SegNum::Segment(0), 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0);
        assert_eq!(result.flags, SegFlags::empty());
        assert!(segmap.is_empty());
    }

    #[test]
    fn merge_segments_single_segment_ok() {
        let segment = create_test_segment(".text", 0x2000, 0x100, SegFlags::READ);
        let segments = vec![("obj1.mild".to_string(), SegNum::Segment(1), segment.clone())];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", SegNum::Segment(0), 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0x100);
        assert_eq!(result.flags, SegFlags::READ);

        assert!(segmap.contains_key("obj1.mild"));
        assert_eq!(
            segmap["obj1.mild"][&SegNum::Segment(1)],
            (SegNum::Segment(0), 0x1000)
        );
    }

    #[test]
    fn merge_segments_multiple_segments_ok() {
        let segment1 = create_test_segment(".text", 0x2000, 0x100, SegFlags::READ);
        let segment2 = create_test_segment(".text", 0x3000, 0x80, SegFlags::READ);
        let segments = vec![
            (
                "obj1.mild".to_string(),
                SegNum::Segment(1),
                segment1.clone(),
            ),
            (
                "obj2.mild".to_string(),
                SegNum::Segment(2),
                segment2.clone(),
            ),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", SegNum::Segment(0), 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);
        assert_eq!(result.len, 0x180);
        assert_eq!(result.flags, SegFlags::READ);

        assert!(segmap.contains_key("obj1.mild"));
        assert!(segmap.contains_key("obj2.mild"));
        assert_eq!(
            segmap["obj1.mild"][&SegNum::Segment(1)],
            (SegNum::Segment(0), 0x1000)
        );
        assert_eq!(
            segmap["obj2.mild"][&SegNum::Segment(2)],
            (SegNum::Segment(0), 0x1100)
        );
    }

    #[test]
    fn merge_segments_alignment_ok() {
        let segment1 = create_test_segment(".text", 0x2000, 0x101, SegFlags::READ); // Odd length
        let segment2 = create_test_segment(".text", 0x3000, 0x80, SegFlags::READ);
        let segments = vec![
            (
                "obj1.mild".to_string(),
                SegNum::Segment(1),
                segment1.clone(),
            ),
            (
                "obj2.mild".to_string(),
                SegNum::Segment(2),
                segment2.clone(),
            ),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".text", SegNum::Segment(0), 0x1000, &mut segmap);

        assert_eq!(result.name, ".text");
        assert_eq!(result.address, 0x1000);

        assert!(segmap.contains_key("obj1.mild"));
        assert!(segmap.contains_key("obj2.mild"));
        assert_eq!(
            segmap["obj1.mild"][&SegNum::Segment(1)],
            (SegNum::Segment(0), 0x1000)
        );
        assert_eq!(
            segmap["obj2.mild"][&SegNum::Segment(2)],
            (SegNum::Segment(0), 0x1104)
        ); // 0x1000 + 0x101 aligned to next 4-byte boundary
    }

    #[test]
    fn merge_segments_flag_combination_ok() {
        let segment1 = create_test_segment(".data", 0x4000, 0x100, SegFlags::READ);
        let segment2 = create_test_segment(".data", 0x5000, 0x80, SegFlags::WRITE);
        let segment3 = create_test_segment(".data", 0x6000, 0x40, SegFlags::PRESENT);
        let segments = vec![
            (
                "obj1.mild".to_string(),
                SegNum::Segment(1),
                segment1.clone(),
            ),
            (
                "obj2.mild".to_string(),
                SegNum::Segment(2),
                segment2.clone(),
            ),
            (
                "obj3.mild".to_string(),
                SegNum::Segment(3),
                segment3.clone(),
            ),
        ];
        let mut segmap = HashMap::new();

        let result = merge_segments(&segments, ".data", SegNum::Segment(0), 0x4000, &mut segmap);

        assert_eq!(result.name, ".data");
        assert_eq!(result.address, 0x4000);
        assert_eq!(
            result.flags,
            SegFlags::READ | SegFlags::WRITE | SegFlags::PRESENT
        );
    }
}
