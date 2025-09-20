use anyhow::{bail, Context};
use bitflags::bitflags;
use std::collections::HashSet;
use std::fmt::Display;
use std::fs;
use std::path::Path;

const MILD_MAGIC: &str = "LINK";

#[derive(Debug)]
struct Sizes {
    num_segments: u32,
    num_symbols: u32,
    num_relocs: u32,
}

impl Display for Sizes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Sizes {{ num_segments: {:X}, num_symbols: {:X}, num_relocs: {:X} }}",
            self.num_segments, self.num_symbols, self.num_relocs
        )
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct SegmentFlags: u8 {
        const READ = 0b00000001;
        const WRITE = 0b00000010;
        const PRESENT = 0b00000100;
    }
}

impl Display for SegmentFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut flags = Vec::new();
        if self.contains(SegmentFlags::READ) {
            flags.push("R");
        }
        if self.contains(SegmentFlags::WRITE) {
            flags.push("W");
        }
        if self.contains(SegmentFlags::PRESENT) {
            flags.push("P");
        }
        write!(f, "{}", flags.join(""))
    }
}

#[derive(Debug)]
struct Segment {
    name: String,
    address: u32,
    len: u32,
    desc: SegmentFlags,
}

impl Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Segment {{ name: {}, address: {:X}, len: {:X}, desc: {} }}",
            self.name, self.address, self.len, self.desc
        )
    }
}

#[derive(Debug)]
enum SegNum {
    Segment(u8),
    AbsOrUndef,
}

impl Display for SegNum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SegNum::Segment(num) => write!(f, "SegNum({:X})", num),
            SegNum::AbsOrUndef => write!(f, "Absolute/Undefined"),
        }
    }
}

#[derive(Debug)]
enum SymbolType {
    Defined,
    Undefined,
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolType::Defined => write!(f, "Defined"),
            SymbolType::Undefined => write!(f, "Undefined"),
        }
    }
}

#[derive(Debug)]
struct Symbol {
    name: String,
    value: u32,
    segnum: SegNum,
    symtype: SymbolType,
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Symbol {{ name: {}, value: {:X}, segnum: {}, symtype: {} }}",
            self.name, self.value, self.segnum, self.symtype
        )
    }
}

#[derive(Debug)]
enum RelocationType {
    Absolute,
    Relative,
}

impl Display for RelocationType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelocationType::Absolute => write!(f, "Absolute"),
            RelocationType::Relative => write!(f, "Relative"),
        }
    }
}

#[derive(Debug)]
struct Relocation {
    location: u32,
    seg: SegNum,
    relref: u8,
    reltype: RelocationType,
}

impl Display for Relocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Relocation {{ location: {:X}, seg: {}, relref: {:X}, reltype: {} }}",
            self.location, self.seg, self.relref, self.reltype
        )
    }
}

#[derive(Debug)]
struct SegData {
    seg: SegNum,
    data: Vec<u8>,
}

impl Display for SegData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Data {{ seg: {}, data: [", self.seg)?;
        for byte in &self.data {
            write!(f, "{:02X} ", byte)?;
        }
        write!(f, "] }}")
    }
}

#[derive(Debug)]
pub struct Object {
    sizes: Sizes,
    segments: Vec<Segment>,
    symtab: Vec<Symbol>,
    relocs: Vec<Relocation>,
    data: Vec<SegData>,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Object {{")?;
        writeln!(f, "  {},", self.sizes)?;
        if !self.segments.is_empty() {
            writeln!(f, "  segments: [")?;
            for segment in &self.segments {
                writeln!(f, "    {},", segment)?;
            }
            writeln!(f, "  ]")?;
        }

        if !self.symtab.is_empty() {
            writeln!(f, "  symtab: [")?;
            for symbol in &self.symtab {
                writeln!(f, "    {},", symbol)?;
            }
            writeln!(f, "  ]")?;
        }

        if !self.relocs.is_empty() {
            writeln!(f, "  relocs: [")?;
            for reloc in &self.relocs {
                writeln!(f, "    {},", reloc)?;
            }
            writeln!(f, "  ]")?;
        }

        if !self.data.is_empty() {
            writeln!(f, "  data: [")?;
            for data in &self.data {
                writeln!(f, "    {},", data)?;
            }
            writeln!(f, "  ]")?;
        }
        write!(f, "}}")
    }
}

fn load_magic(content: &[&str]) -> anyhow::Result<()> {
    match content.first() {
        Some(&MILD_MAGIC) => Ok(()),
        Some(magic) => bail!(
            "Invalid magic number: expected '{MILD_MAGIC}', found '{}'",
            magic
        ),
        None => bail!("Invalid object file: no magic number found"),
    }
}

fn load_sizes(contents: &[&str]) -> anyhow::Result<Sizes> {
    if contents.len() < 2 {
        bail!("Invalid object file: insufficient lines for sizes");
    }

    let mut parts = contents[1].split_whitespace();

    let num_segments = u32::from_str_radix(parts.next().context("Missing number of segments")?, 16)
        .context("Failed to parse number of segments")?;
    let num_symbols = u32::from_str_radix(parts.next().context("Missing number of symbols")?, 16)
        .context("Failed to parse number of symbols")?;
    let num_relocs =
        u32::from_str_radix(parts.next().context("Missing number of relocations")?, 16)
            .context("Failed to parse number of relocations")?;

    Ok(Sizes {
        num_segments,
        num_symbols,
        num_relocs,
    })
}

fn load_segments(contents: &[&str], num_segments: u32) -> anyhow::Result<Vec<Segment>> {
    let mut segments = Vec::new();

    for i in 0..num_segments as usize {
        let line_index = 2 + i; // Segments start from the third line
        if line_index >= contents.len() {
            bail!("Invalid object file: insufficient lines for segments");
        }

        let line = contents[line_index];
        let mut parts = line.split_whitespace();

        let name = parts
            .next()
            .context(format!("Missing name for segment {}", i))?
            .to_string();

        let address = u32::from_str_radix(
            parts
                .next()
                .context(format!("Missing address for segment {}", i))?,
            16,
        )
        .context(format!("Failed to parse address for segment {}", i))?;

        let len = u32::from_str_radix(
            parts
                .next()
                .context(format!("Missing length for segment {}", i))?,
            16,
        )
        .context(format!("Failed to parse length for segment {}", i))?;

        let desc_str = parts
            .next()
            .context(format!("Missing descriptor for segment {}", i))?;
        let mut desc = SegmentFlags::empty();
        let mut seen = HashSet::new();
        for ch in desc_str.chars() {
            if seen.contains(&ch) {
                bail!(format!(
                    "Duplicate descriptor character '{}' in segment {}",
                    ch, i
                ));
            }
            seen.insert(ch);

            match ch {
                'R' | 'r' => desc |= SegmentFlags::READ,
                'W' | 'w' => desc |= SegmentFlags::WRITE,
                'P' | 'p' => desc |= SegmentFlags::PRESENT,
                _ => bail!(format!(
                    "Invalid descriptor character '{}' in segment {}",
                    ch, i
                )),
            }
        }

        if desc_str.is_empty() {
            bail!(format!("Empty descriptor for segment {} is not allowed", i));
        }

        segments.push(Segment {
            name,
            address,
            len,
            desc,
        });
    }

    Ok(segments)
}

fn load_symbols(contents: &[&str], sizes: &Sizes) -> anyhow::Result<Vec<Symbol>> {
    let mut symbols = Vec::new();
    let start_index = 2 + sizes.num_segments as usize; // Symbols start after segments

    for i in 0..sizes.num_symbols as usize {
        let line_index = start_index + i;
        if line_index >= contents.len() {
            bail!("Invalid object file: insufficient lines for symbols");
        }

        let line = contents[line_index];
        let mut parts = line.split_whitespace();

        let name = parts
            .next()
            .context(format!("Missing name for symbol {}", i))?
            .to_string();

        let value = u32::from_str_radix(
            parts
                .next()
                .context(format!("Missing value for symbol {}", i))?,
            16,
        )
        .context(format!("Failed to parse value for symbol {}", i))?;

        let segnum = u8::from_str_radix(
            parts
                .next()
                .context(format!("Missing segment number for symbol {}", i))?,
            16,
        )
        .context(format!("Failed to parse segment number for symbol {}", i))?;
        let segnum = if segnum == 0 {
            SegNum::AbsOrUndef
        } else {
            SegNum::Segment(segnum)
        };

        let symtype_str = parts
            .next()
            .context(format!("Missing type for symbol {}", i))?;
        let symtype = match symtype_str {
            "D" => SymbolType::Defined,
            "U" => SymbolType::Undefined,
            _ => bail!(format!(
                "Invalid symbol type '{}' for symbol {}",
                symtype_str, i
            )),
        };

        symbols.push(Symbol {
            name,
            value,
            segnum,
            symtype,
        });
    }

    Ok(symbols)
}

fn load_relocations(contents: &[&str], sizes: &Sizes) -> anyhow::Result<Vec<Relocation>> {
    let mut relocations = Vec::new();
    let start_idx = 2 + sizes.num_segments as usize + sizes.num_symbols as usize;

    for i in 0..sizes.num_relocs as usize {
        let line_index = start_idx + i;
        if line_index >= contents.len() {
            bail!("Invalid object file: insufficient lines for relocations");
        }

        let line = contents[line_index];
        let mut parts = line.split_whitespace();

        let location = u32::from_str_radix(
            parts
                .next()
                .context(format!("Missing location for relocation {}", i))?,
            16,
        )
        .context(format!("Failed to parse location for relocation {}", i))?;

        let segnum = u8::from_str_radix(
            parts
                .next()
                .context(format!("Missing segment number for relocation {}", i))?,
            16,
        )
        .context(format!(
            "Failed to parse segment number for relocation {}",
            i
        ))?;
        let segnum = SegNum::Segment(segnum);

        let relref = u8::from_str_radix(
            parts
                .next()
                .context(format!("Missing reference for relocation {}", i))?,
            16,
        )
        .context(format!("Failed to parse reference for relocation {}", i))?;

        let reltype_str = parts
            .next()
            .context(format!("Missing type for relocation {}", i))?;
        let reltype = match reltype_str {
            "A4" => RelocationType::Absolute,
            "R4" => RelocationType::Relative,
            _ => bail!(format!(
                "Invalid relocation type '{}' for relocation {}",
                reltype_str, i
            ),),
        };
        relocations.push(Relocation {
            location,
            seg: segnum,
            relref,
            reltype,
        });
    }

    Ok(relocations)
}

fn parse_byte_data(hex_str: &str, segnum: usize) -> anyhow::Result<Vec<u8>> {
    let mut data = Vec::new();
    for pair in hex_str.as_bytes().chunks(2) {
        if pair.len() != 2 {
            bail!(format!(
                "Invalid data length {} in segment {}: must be even number of hex digits",
                hex_str.len(),
                segnum
            ));
        }
        let hex_str = std::str::from_utf8(pair).context(format!(
            "Failed to convert segment {} data from utf8 to str",
            segnum
        ))?;
        let byte = u8::from_str_radix(hex_str, 16).with_context(|| {
            format!("Failed to parse segment {} data byte '{}'", segnum, hex_str)
        })?;
        data.push(byte);
    }

    Ok(data)
}

fn load_data(
    contents: &[&str],
    segments: &[Segment],
    sizes: &Sizes,
) -> anyhow::Result<Vec<SegData>> {
    let mut seg_data = Vec::new();
    let mut data_idx =
        2 + sizes.num_segments as usize + sizes.num_symbols as usize + sizes.num_relocs as usize;

    for (segnum, segment) in segments.iter().enumerate() {
        if !segment.desc.contains(SegmentFlags::PRESENT) {
            continue;
        }
        if data_idx >= contents.len() {
            bail!(format!(
                "Invalid object file: missing segment {} data",
                segnum + 1
            ));
        }
        if contents[data_idx].is_empty() {
            bail!(format!(
                "Invalid object file: empty data for segment {}",
                segnum + 1
            ));
        }

        seg_data.push(SegData {
            seg: SegNum::Segment((segnum + 1) as u8),
            data: parse_byte_data(contents[data_idx], segnum + 1)?,
        });

        data_idx += 1;
    }

    Ok(seg_data)
}

pub fn load_object(file: &Path) -> anyhow::Result<Object> {
    let contents = fs::read_to_string(file).with_context(|| "Failed to read object file")?;
    let contents: Vec<&str> = contents.lines().collect();

    load_magic(&contents)?;
    let sizes = load_sizes(&contents)?;
    let segments = load_segments(&contents, sizes.num_segments)?;
    let symtab = load_symbols(&contents, &sizes)?;
    let relocs = load_relocations(&contents, &sizes)?;
    let data = load_data(&contents, &segments, &sizes)?;

    Ok(Object {
        sizes,
        segments,
        symtab,
        relocs,
        data,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_magic_valid_magic_ok() {
        let content = vec!["LINK", "1 2 3"];
        assert!(load_magic(&content).is_ok());
    }

    #[test]
    fn load_magic_invalid_magic_error() {
        let content = vec!["INVALID", "1 2 3"];
        assert!(load_magic(&content).is_err());
    }

    #[test]
    fn load_magic_empty_content_error() {
        let content = vec![];
        assert!(load_magic(&content).is_err());
    }

    #[test]
    fn load_magic_empty_string_error() {
        let content = vec![""];
        assert!(load_magic(&content).is_err());
    }

    #[test]
    fn load_magic_case_sensitive_error() {
        let content = vec!["link", "1 2 3"];
        assert!(load_magic(&content).is_err());
    }

    #[test]
    fn load_sizes_valid_input_ok() {
        let content = vec!["LINK", "10 20 30"];
        let result = load_sizes(&content).unwrap();
        assert_eq!(result.num_segments, 0x10);
        assert_eq!(result.num_symbols, 0x20);
        assert_eq!(result.num_relocs, 0x30);
    }

    #[test]
    fn load_sizes_insufficient_lines_error() {
        let content = vec!["LINK"];
        assert!(load_sizes(&content).is_err());
    }

    #[test]
    fn load_sizes_empty_content_error() {
        let content = vec![];
        assert!(load_sizes(&content).is_err());
    }

    #[test]
    fn load_sizes_missing_numbers_error() {
        let content = vec!["LINK", "10 20"];
        assert!(load_sizes(&content).is_err());
    }

    #[test]
    fn load_sizes_invalid_number_format_error() {
        let content = vec!["LINK", "hello 20 30"];
        assert!(load_sizes(&content).is_err());
    }

    #[test]
    fn load_sizes_empty_sizes_line_error() {
        let content = vec!["LINK", ""];
        assert!(load_sizes(&content).is_err());
    }

    #[test]
    fn load_sizes_extra_whitespace_ok() {
        let content = vec!["LINK", "  10   20   30  "];
        let result = load_sizes(&content).unwrap();
        assert_eq!(result.num_segments, 0x10);
        assert_eq!(result.num_symbols, 0x20);
        assert_eq!(result.num_relocs, 0x30);
    }

    #[test]
    fn load_sizes_extra_values_ok() {
        let content = vec!["LINK", "10 20 30 40 50"];
        let result = load_sizes(&content).unwrap();
        assert_eq!(result.num_segments, 0x10);
        assert_eq!(result.num_symbols, 0x20);
        assert_eq!(result.num_relocs, 0x30);
    }

    #[test]
    fn load_sizes_hexadecimal_numbers_ok() {
        let content = vec!["LINK", "A 14 1E"];
        let result = load_sizes(&content).unwrap();
        assert_eq!(result.num_segments, 0xA);
        assert_eq!(result.num_symbols, 0x14);
        assert_eq!(result.num_relocs, 0x1E);
    }

    #[test]
    fn load_segments_valid_segments_ok() {
        let content = vec!["LINK", "2 0 0", ".text 1000 2500 RP", ".data 4000 C00 RWP"];
        let result = load_segments(&content, 2).unwrap();

        assert_eq!(result.len(), 2);

        assert_eq!(result[0].name, ".text");
        assert_eq!(result[0].address, 0x1000);
        assert_eq!(result[0].len, 0x2500);
        assert_eq!(result[0].desc, SegmentFlags::READ | SegmentFlags::PRESENT);

        assert_eq!(result[1].name, ".data");
        assert_eq!(result[1].address, 0x4000);
        assert_eq!(result[1].len, 0xC00);
        assert_eq!(
            result[1].desc,
            SegmentFlags::READ | SegmentFlags::WRITE | SegmentFlags::PRESENT
        );
    }

    #[test]
    fn load_segments_zero_segments_ok() {
        let content = vec!["LINK", "0 0 0"];
        let result = load_segments(&content, 0).unwrap();
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn load_segments_insufficient_lines_error() {
        let content = vec!["LINK", "2 0 0", ".text 1000 2500 RP"];
        assert!(load_segments(&content, 2).is_err());
    }

    #[test]
    fn load_segments_missing_name_error() {
        let content = vec!["LINK", "1 0 0", ""];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_missing_address_error() {
        let content = vec!["LINK", "1 0 0", ".text"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_missing_length_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_missing_descriptor_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_invalid_address_format_error() {
        let content = vec!["LINK", "1 0 0", ".text INVALID 2500 RP"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_invalid_length_format_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 INVALID RP"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_invalid_descriptor_character_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 RXP"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_empty_descriptor_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 "];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_duplicate_flags_err() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 RRP"];
        assert!(load_segments(&content, 1).is_err());
    }

    #[test]
    fn load_segments_mixed_case_descriptor_ok() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 rWp"];
        assert!(load_segments(&content, 1).is_ok());
    }

    #[test]
    fn load_segments_extra_whitespace_ok() {
        let content = vec!["LINK", "1 0 0", "  .text   1000   2500   RP  "];
        let result = load_segments(&content, 1).unwrap();

        assert_eq!(result[0].name, ".text");
        assert_eq!(result[0].address, 0x1000);
        assert_eq!(result[0].len, 0x2500);
        assert_eq!(result[0].desc, SegmentFlags::READ | SegmentFlags::PRESENT);
    }

    #[test]
    fn load_symbols_valid_symbols_ok() {
        let content = vec!["LINK", "0 3 0", "foo 1234 1 D", "bar 5678 2 U", "baz 0 0 U"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 3,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();

        assert_eq!(result.len(), 3);

        assert_eq!(result[0].name, "foo");
        assert_eq!(result[0].value, 0x1234);
        assert!(matches!(result[0].segnum, SegNum::Segment(1)));
        assert!(matches!(result[0].symtype, SymbolType::Defined));

        assert_eq!(result[1].name, "bar");
        assert_eq!(result[1].value, 0x5678);
        assert!(matches!(result[1].segnum, SegNum::Segment(2)));
        assert!(matches!(result[1].symtype, SymbolType::Undefined));

        assert_eq!(result[2].name, "baz");
        assert_eq!(result[2].value, 0x0);
        assert!(matches!(result[2].segnum, SegNum::AbsOrUndef));
        assert!(matches!(result[2].symtype, SymbolType::Undefined));
    }

    #[test]
    fn load_symbols_zero_symbols_ok() {
        let content = vec!["LINK", "0 0 0"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn load_symbols_insufficient_lines_error() {
        let content = vec!["LINK", "0 2 0", "foo 1234 1 D"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 2,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_missing_name_error() {
        let content = vec!["LINK", "0 1 0", ""];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_missing_value_error() {
        let content = vec!["LINK", "0 1 0", "foo"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_missing_segment_number_error() {
        let content = vec!["LINK", "0 1 0", "foo 1234"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_missing_type_error() {
        let content = vec!["LINK", "0 1 0", "foo 1234 1"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_invalid_value_format_error() {
        let content = vec!["LINK", "0 1 0", "foo INVALID 1 D"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_invalid_segment_number_format_error() {
        let content = vec!["LINK", "0 1 0", "foo 1234 INVALID D"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_invalid_type_error() {
        let content = vec!["LINK", "0 1 0", "foo 1234 1 X"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        assert!(load_symbols(&content, &sizes).is_err());
    }

    #[test]
    fn load_symbols_zero_segment_number_abs_or_undef_ok() {
        let content = vec!["LINK", "0 1 0", "foo 1234 0 U"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();

        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].segnum, SegNum::AbsOrUndef));
    }

    #[test]
    fn load_symbols_hexadecimal_values_ok() {
        let content = vec!["LINK", "0 1 0", "foo DEADBEEF A D"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].value, 0xDEADBEEF);
        assert!(matches!(result[0].segnum, SegNum::Segment(0xA)));
    }

    #[test]
    fn load_symbols_extra_whitespace_ok() {
        let content = vec!["LINK", "0 1 0", "  foo   1234   1   D  "];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 1,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].name, "foo");
        assert_eq!(result[0].value, 0x1234);
        assert!(matches!(result[0].segnum, SegNum::Segment(1)));
        assert!(matches!(result[0].symtype, SymbolType::Defined));
    }

    #[test]
    fn load_symbols_with_segments_offset_ok() {
        let content = vec![
            "LINK",
            "2 2 0",
            ".text 1000 2500 RP",
            ".data 4000 C00 RWP",
            "foo 1234 1 D",
            "bar 5678 2 U",
        ];
        let sizes = Sizes {
            num_segments: 2,
            num_symbols: 2,
            num_relocs: 0,
        };
        let result = load_symbols(&content, &sizes).unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].name, "foo");
        assert_eq!(result[1].name, "bar");
    }

    #[test]
    fn load_relocations_valid_relocations_ok() {
        let content = vec!["LINK", "0 0 3", "1100 1 1 A4", "4100 2 2 R4", "8000 0 3 A4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 3,
        };
        let result = load_relocations(&content, &sizes).unwrap();

        assert_eq!(result.len(), 3);

        assert_eq!(result[0].location, 0x1100);
        assert!(matches!(result[0].seg, SegNum::Segment(1)));
        assert_eq!(result[0].relref, 1);
        assert!(matches!(result[0].reltype, RelocationType::Absolute));

        assert_eq!(result[1].location, 0x4100);
        assert!(matches!(result[1].seg, SegNum::Segment(2)));
        assert_eq!(result[1].relref, 2);
        assert!(matches!(result[1].reltype, RelocationType::Relative));

        assert_eq!(result[2].location, 0x8000);
        assert!(matches!(result[2].seg, SegNum::Segment(0)));
        assert_eq!(result[2].relref, 3);
        assert!(matches!(result[2].reltype, RelocationType::Absolute));
    }

    #[test]
    fn load_relocations_zero_relocations_ok() {
        let content = vec!["LINK", "0 0 0"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_relocations(&content, &sizes).unwrap();
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn load_relocations_insufficient_lines_error() {
        let content = vec!["LINK", "0 0 2", "1100 1 1 A4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 2,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_missing_location_error() {
        let content = vec!["LINK", "0 0 1", ""];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_missing_segment_number_error() {
        let content = vec!["LINK", "0 0 1", "1100"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_missing_reference_error() {
        let content = vec!["LINK", "0 0 1", "1100 1"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_missing_type_error() {
        let content = vec!["LINK", "0 0 1", "1100 1 1"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_invalid_location_format_error() {
        let content = vec!["LINK", "0 0 1", "INVALID 1 1 A4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_invalid_segment_number_format_error() {
        let content = vec!["LINK", "0 0 1", "1100 INVALID 1 A4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_invalid_reference_format_error() {
        let content = vec!["LINK", "0 0 1", "1100 1 INVALID A4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_invalid_type_error() {
        let content = vec!["LINK", "0 0 1", "1100 1 1 X4"];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        assert!(load_relocations(&content, &sizes).is_err());
    }

    #[test]
    fn load_relocations_extra_whitespace_ok() {
        let content = vec!["LINK", "0 0 1", "  1100   1   1   A4  "];
        let sizes = Sizes {
            num_segments: 0,
            num_symbols: 0,
            num_relocs: 1,
        };
        let result = load_relocations(&content, &sizes).unwrap();

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].location, 0x1100);
        assert!(matches!(result[0].seg, SegNum::Segment(1)));
        assert_eq!(result[0].relref, 1);
        assert!(matches!(result[0].reltype, RelocationType::Absolute));
    }

    #[test]
    fn load_relocations_with_segments_and_symbols_offset_ok() {
        let content = vec![
            "LINK",
            "2 2 2",
            ".text 1000 2500 RP",
            ".data 4000 C00 RWP",
            "foo 1234 1 D",
            "bar 5678 2 U",
            "1100 1 1 A4",
            "4100 2 2 R4",
        ];
        let sizes = Sizes {
            num_segments: 2,
            num_symbols: 2,
            num_relocs: 2,
        };
        let result = load_relocations(&content, &sizes).unwrap();

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].location, 0x1100);
        assert_eq!(result[1].location, 0x4100);
    }

    #[test]
    fn load_data_valid_data_ok() {
        let content = vec![
            "LINK",
            "2 0 0",
            ".text 1000 2500 RP",
            ".data 4000 C00 RWP",
            "DEADBEEF",
            "CAFEBABE",
        ];
        let segments = vec![
            Segment {
                name: ".text".to_string(),
                address: 0x1000,
                len: 0x2500,
                desc: SegmentFlags::READ | SegmentFlags::PRESENT,
            },
            Segment {
                name: ".data".to_string(),
                address: 0x4000,
                len: 0xC00,
                desc: SegmentFlags::READ | SegmentFlags::WRITE | SegmentFlags::PRESENT,
            },
        ];
        let sizes = Sizes {
            num_segments: 2,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_data(&content, &segments, &sizes).unwrap();

        assert_eq!(result.len(), 2);
        assert!(matches!(result[0].seg, SegNum::Segment(1)));
        assert_eq!(result[0].data, vec![0xDE, 0xAD, 0xBE, 0xEF]);
        assert!(matches!(result[1].seg, SegNum::Segment(2)));
        assert_eq!(result[1].data, vec![0xCA, 0xFE, 0xBA, 0xBE]);
    }

    #[test]
    fn load_data_no_present_segments_ok() {
        let content = vec!["LINK", "2 0 0", ".text 1000 2500 RW", ".data 4000 C00 RW"];
        let segments = vec![
            Segment {
                name: ".text".to_string(),
                address: 0x1000,
                len: 0x2500,
                desc: SegmentFlags::READ | SegmentFlags::WRITE,
            },
            Segment {
                name: ".data".to_string(),
                address: 0x4000,
                len: 0xC00,
                desc: SegmentFlags::READ | SegmentFlags::WRITE,
            },
        ];
        let sizes = Sizes {
            num_segments: 2,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_data(&content, &segments, &sizes).unwrap();

        assert_eq!(result.len(), 0);
    }

    #[test]
    fn load_data_mixed_present_segments_ok() {
        let content = vec![
            "LINK",
            "3 0 0",
            ".text 1000 2500 RP",
            ".data 4000 C00 RW",
            ".bss 5000 1900 RWP",
            "DEADBEEF",
            "CAFEBABE",
        ];
        let segments = vec![
            Segment {
                name: ".text".to_string(),
                address: 0x1000,
                len: 0x2500,
                desc: SegmentFlags::READ | SegmentFlags::PRESENT,
            },
            Segment {
                name: ".data".to_string(),
                address: 0x4000,
                len: 0xC00,
                desc: SegmentFlags::READ | SegmentFlags::WRITE,
            },
            Segment {
                name: ".bss".to_string(),
                address: 0x5000,
                len: 0x1900,
                desc: SegmentFlags::READ | SegmentFlags::WRITE | SegmentFlags::PRESENT,
            },
        ];
        let sizes = Sizes {
            num_segments: 3,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_data(&content, &segments, &sizes).unwrap();

        assert_eq!(result.len(), 2);
        assert!(matches!(result[0].seg, SegNum::Segment(1)));
        assert_eq!(result[0].data, vec![0xDE, 0xAD, 0xBE, 0xEF]);
        assert!(matches!(result[1].seg, SegNum::Segment(3)));
        assert_eq!(result[1].data, vec![0xCA, 0xFE, 0xBA, 0xBE]);
    }

    #[test]
    fn load_data_empty_data_err() {
        let content = vec!["LINK", "1 0 0", ".text 1000 0 RP", ""];
        let segments = vec![Segment {
            name: ".text".to_string(),
            address: 0x1000,
            len: 0x0,
            desc: SegmentFlags::READ | SegmentFlags::PRESENT,
        }];
        let sizes = Sizes {
            num_segments: 1,
            num_symbols: 0,
            num_relocs: 0,
        };
        assert!(load_data(&content, &segments, &sizes).is_err());
    }

    #[test]
    fn load_data_single_byte_ok() {
        let content = vec!["LINK", "1 0 0", ".text 1 1000 RP", "FF"];
        let segments = vec![Segment {
            name: ".text".to_string(),
            address: 0x1000,
            len: 0x1,
            desc: SegmentFlags::READ | SegmentFlags::PRESENT,
        }];
        let sizes = Sizes {
            num_segments: 1,
            num_symbols: 0,
            num_relocs: 0,
        };
        let result = load_data(&content, &segments, &sizes).unwrap();

        assert_eq!(result.len(), 1);
        assert!(matches!(result[0].seg, SegNum::Segment(1)));
        assert_eq!(result[0].data, vec![0xFF]);
    }

    #[test]
    fn load_data_insufficient_lines_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 RP"];
        let segments = vec![Segment {
            name: ".text".to_string(),
            address: 0x1000,
            len: 0x2500,
            desc: SegmentFlags::READ | SegmentFlags::PRESENT,
        }];
        let sizes = Sizes {
            num_segments: 1,
            num_symbols: 0,
            num_relocs: 0,
        };
        assert!(load_data(&content, &segments, &sizes).is_err());
    }

    #[test]
    fn load_data_invalid_hex_length_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 RP", "DEADBEE"];
        let segments = vec![Segment {
            name: ".text".to_string(),
            address: 0x1000,
            len: 0x2500,
            desc: SegmentFlags::READ | SegmentFlags::PRESENT,
        }];
        let sizes = Sizes {
            num_segments: 1,
            num_symbols: 0,
            num_relocs: 0,
        };
        assert!(load_data(&content, &segments, &sizes).is_err());
    }

    #[test]
    fn load_data_invalid_hex_character_error() {
        let content = vec!["LINK", "1 0 0", ".text 1000 2500 RP", "DEADBEZF"];
        let segments = vec![Segment {
            name: ".text".to_string(),
            address: 0x1000,
            len: 0x2500,
            desc: SegmentFlags::READ | SegmentFlags::PRESENT,
        }];
        let sizes = Sizes {
            num_segments: 1,
            num_symbols: 0,
            num_relocs: 0,
        };
        assert!(load_data(&content, &segments, &sizes).is_err());
    }
}
