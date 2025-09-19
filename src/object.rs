use anyhow::{bail, Context};
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
            "Sizes {{ num_segments: {}, num_symbols: {}, num_relocs: {} }}",
            self.num_segments, self.num_symbols, self.num_relocs
        )
    }
}

#[derive(Debug)]
pub struct Object {
    sizes: Sizes,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object {{ sizes: {} }}", self.sizes)
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

    let num_segments = parts
        .next()
        .context("Missing number of segments")?
        .parse::<u32>()
        .context("Failed to parse number of segments")?;

    let num_symbols = parts
        .next()
        .context("Missing number of symbols")?
        .parse::<u32>()
        .context("Failed to parse number of symbols")?;

    let num_relocs = parts
        .next()
        .context("Missing number of relocations")?
        .parse::<u32>()
        .context("Failed to parse number of relocations")?;

    Ok(Sizes {
        num_segments,
        num_symbols,
        num_relocs,
    })
}

pub fn load_object(file: &Path) -> anyhow::Result<Object> {
    let contents = fs::read_to_string(file).with_context(|| "Failed to read object file")?;
    let contents: Vec<&str> = contents.lines().collect();

    load_magic(&contents)?;
    let sizes = load_sizes(&contents)?;

    Ok(Object { sizes })
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
        assert_eq!(result.num_segments, 10);
        assert_eq!(result.num_symbols, 20);
        assert_eq!(result.num_relocs, 30);
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
        let content = vec!["LINK", "abc 20 30"];
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
        assert_eq!(result.num_segments, 10);
        assert_eq!(result.num_symbols, 20);
        assert_eq!(result.num_relocs, 30);
    }

    #[test]
    fn load_sizes_extra_values_ok() {
        let content = vec!["LINK", "10 20 30 40 50"];
        let result = load_sizes(&content).unwrap();
        assert_eq!(result.num_segments, 10);
        assert_eq!(result.num_symbols, 20);
        assert_eq!(result.num_relocs, 30);
    }
}
