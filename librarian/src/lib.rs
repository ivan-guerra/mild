use anyhow::Context;
use std::path::{Path, PathBuf};

fn cp_to_lib(lib_path: &Path, obj_path: &Path) -> anyhow::Result<PathBuf> {
    let file_name = obj_path
        .file_name()
        .with_context(|| {
            format!(
                "Failed to get file name from object path: {}",
                obj_path.display()
            )
        })?
        .to_owned();
    let dest_path = lib_path.join(file_name);
    std::fs::copy(obj_path, &dest_path).with_context(|| {
        format!(
            "Failed to copy object file from {} to {}",
            obj_path.display(),
            dest_path.display()
        )
    })?;

    Ok(dest_path)
}

pub fn create_initial_lib(lib_path: PathBuf, obj_paths: Vec<PathBuf>) -> anyhow::Result<()> {
    // Create the output library directory if it doesn't exist
    std::fs::create_dir_all(&lib_path).with_context(|| {
        format!(
            "Failed to create output library path: {}",
            lib_path.display()
        )
    })?;

    for obj_path in obj_paths {
        // Copy each object file to the output_path
        let archived_obj_path = cp_to_lib(&lib_path, &obj_path)?;

        // Load the object file and collect its global symbols
        let object = mild::load_object(&obj_path)
            .with_context(|| format!("Failed to load object file: {}", obj_path.display()))?;
        let gsymtab = mild::collect_global_symbols(&[object]).with_context(|| {
            format!(
                "Failed to collect global symbols from: {}",
                obj_path.display()
            )
        })?;

        // For each global symbol, create a symlink in lib_path pointing to the copied object file
        for symname in gsymtab.keys() {
            let symbol_path = lib_path.join(symname);
            if !symbol_path.exists() {
                #[cfg(unix)]
                std::os::unix::fs::symlink(&archived_obj_path, symbol_path).with_context(|| {
                    format!(
                        "Failed to create symlink for symbol {} in {}",
                        symname,
                        lib_path.display()
                    )
                })?;
            }
        }
    }

    Ok(())
}
