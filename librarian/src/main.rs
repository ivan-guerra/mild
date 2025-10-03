use anyhow::Context;
use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: LibrarianCmds,
}

#[derive(Subcommand)]
enum LibrarianCmds {
    #[command(about = "Create a mild lib.")]
    Create(CommonArgs),
    #[command(about = "Remove modules.")]
    Rm(CommonArgs),
    #[command(about = "Add modules.")]
    Add(CommonArgs),
    #[command(about = "Replace one module with another.")]
    Replace(ReplaceArgs),
}

#[derive(Args)]
struct CommonArgs {
    #[arg(help = "library path")]
    lib_path: PathBuf,

    #[arg(help = "object file path(s)", required = true, num_args = 1..)]
    obj_paths: Vec<PathBuf>,
}

#[derive(Args)]
struct ReplaceArgs {
    #[arg(help = "library path")]
    lib_path: PathBuf,

    #[arg(help = "object file path to be replaced")]
    old_obj: PathBuf,

    #[arg(help = "object file path to replace with")]
    new_obj: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        LibrarianCmds::Create(args) => {
            librarian::create_lib(&args.lib_path, &args.obj_paths).with_context(|| {
                format!("Failed to create library at {}", args.lib_path.display())
            })?;
        }
        LibrarianCmds::Rm(args) => {
            librarian::rm_modules(&args.lib_path, &args.obj_paths).with_context(|| {
                format!(
                    "Failed to remove modules from library at {}",
                    args.lib_path.display()
                )
            })?;
        }
        LibrarianCmds::Add(args) => {
            librarian::add_modules(&args.lib_path, &args.obj_paths).with_context(|| {
                format!(
                    "Failed to add modules to library at {}",
                    args.lib_path.display()
                )
            })?;
        }
        LibrarianCmds::Replace(args) => {
            librarian::rm_modules(&args.lib_path, &vec![args.old_obj.clone()]).with_context(
                || {
                    format!(
                        "Failed to replace module {} in library at {}",
                        args.old_obj.display(),
                        args.lib_path.display()
                    )
                },
            )?;
            librarian::add_modules(&args.lib_path, &vec![args.new_obj.clone()]).with_context(
                || {
                    format!(
                        "Failed to replace module {} in library at {}",
                        args.new_obj.display(),
                        args.lib_path.display()
                    )
                },
            )?;
        }
    }

    Ok(())
}
