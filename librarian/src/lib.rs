use anyhow::Context;
use std::path::{Path, PathBuf};

fn get_symbol_names(archived_obj_path: &Path) -> anyhow::Result<Vec<String>> {
    let object = mild::load_object(archived_obj_path).with_context(|| {
        format!(
            "Failed to load object file: {}",
            archived_obj_path.display()
        )
    })?;
    let gsymtab = mild::collect_global_symbols(&[object]).with_context(|| {
        format!(
            "Failed to collect global symbols from: {}",
            archived_obj_path.display()
        )
    })?;

    Ok(gsymtab.keys().cloned().collect())
}

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

fn rm_object(lib_path: &Path, obj_path: &Path) -> anyhow::Result<()> {
    let archived_obj_path = lib_path.join(obj_path.file_name().with_context(|| {
        format!(
            "Failed to get file name from object path: {}",
            obj_path.display()
        )
    })?);
    if !archived_obj_path.exists() {
        anyhow::bail!(
            "Object file {} does not exist in library path {}",
            obj_path.display(),
            lib_path.display()
        );
    }

    // For each global symbol, remove the symlink in lib_path
    let sym_names = get_symbol_names(&archived_obj_path)?;
    for sym_name in &sym_names {
        let symbol_path = lib_path.join(sym_name);
        if symbol_path.exists() {
            std::fs::remove_file(&symbol_path).with_context(|| {
                format!(
                    "Failed to remove symlink for symbol {} in {}",
                    sym_name,
                    lib_path.display()
                )
            })?;
        }
    }

    // Finally, remove the copied object file
    std::fs::remove_file(&archived_obj_path).with_context(|| {
        format!(
            "Failed to remove archived object file: {}",
            archived_obj_path.display()
        )
    })?;

    Ok(())
}

pub fn create_lib(lib_path: &Path, obj_paths: &Vec<PathBuf>) -> anyhow::Result<()> {
    // Create the output library directory if it doesn't exist
    std::fs::create_dir_all(lib_path).with_context(|| {
        format!(
            "Failed to create output library path: {}",
            lib_path.display()
        )
    })?;

    for obj_path in obj_paths {
        // Copy each object file to the output_path
        let archived_obj_path = cp_to_lib(lib_path, obj_path)?;

        // For each global symbol, create a symlink in lib_path pointing to the copied object file
        let sym_names = get_symbol_names(&archived_obj_path)?;
        for sym_name in &sym_names {
            let symbol_path = lib_path.join(sym_name);
            if !symbol_path.exists() {
                #[cfg(unix)]
                std::os::unix::fs::symlink(&archived_obj_path, symbol_path).with_context(|| {
                    format!(
                        "Failed to create symlink for symbol {} in {}",
                        sym_name,
                        lib_path.display()
                    )
                })?;
            }
        }
    }

    Ok(())
}

pub fn rm_modules(lib_path: &Path, obj_paths: &Vec<PathBuf>) -> anyhow::Result<()> {
    for obj_path in obj_paths {
        rm_object(lib_path, obj_path)?;
    }

    Ok(())
}
