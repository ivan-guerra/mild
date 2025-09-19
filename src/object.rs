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
pub struct Object {
    sizes: Sizes,
    segments: Vec<Segment>,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Object {{")?;
        writeln!(f, "  {},", self.sizes)?;
        writeln!(f, "  segments: [")?;
        for segment in &self.segments {
            writeln!(f, "    {},", segment)?;
        }
        writeln!(f, "  ]")?;
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

pub fn load_object(file: &Path) -> anyhow::Result<Object> {
    let contents = fs::read_to_string(file).with_context(|| "Failed to read object file")?;
    let contents: Vec<&str> = contents.lines().collect();

    load_magic(&contents)?;
    let sizes = load_sizes(&contents)?;
    let segments = load_segments(&contents, sizes.num_segments)?;

    Ok(Object { sizes, segments })
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
}
