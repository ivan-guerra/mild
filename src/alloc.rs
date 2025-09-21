use crate::object::{Object, SegFlags, Segment, Sizes};
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
    segments: &[(&String, &Segment)],
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
    let len = prev_addr - start_addr + 1;

    Segment {
        name,
        len,
        address: start_addr as u32,
        desc,
    }
}

pub fn allocate(objects: &[Object]) -> anyhow::Result<Object> {
    // Find all segments with the given name across all objects
    let find_segments = |segname: &str| -> Vec<(&String, &Segment)> {
        objects
            .iter()
            .flat_map(|obj| {
                obj.segments
                    .iter()
                    .filter(|seg| seg.name == segname)
                    .map(|seg| (&obj.filename, seg))
            })
            .collect()
    };

    let mut segmap: SegMap = HashMap::new();

    let text_segments = find_segments(TEXT_SEGMENT);
    let text_seg = merge_segments(&text_segments, TEXT_SEGMENT, BASE_TEXT_ADDR, &mut segmap);

    let data_segments = find_segments(DATA_SEGMENT);
    let data_start = next_aligned!(BASE_TEXT_ADDR + text_seg.len, PAGE_SIZE);
    let data_seg = merge_segments(&data_segments, DATA_SEGMENT, data_start, &mut segmap);

    let bss_segments = find_segments(BSS_SEGMENT);
    let bss_start = next_aligned!(data_start + data_seg.len, ADDR_SIZE);
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
