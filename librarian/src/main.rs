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
    #[command(about = "Create a mild lib from one or more object files.")]
    Create(LibArgs),
    #[command(about = "Remove one or more object files from a mild lib.")]
    Remove(LibArgs),
    #[command(about = "Add one or more object files to a mild lib.")]
    Add(LibArgs),
}

#[derive(Args)]
struct LibArgs {
    #[arg(help = "output library path")]
    lib_path: PathBuf,

    #[arg(help = "object file path(s)", required = true, num_args = 1..)]
    obj_paths: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        LibrarianCmds::Create(args) => {
            librarian::create_lib(&args.lib_path, &args.obj_paths).with_context(|| {
                format!("Failed to create library at {}", args.lib_path.display())
            })?;
        }
        LibrarianCmds::Remove(args) => {
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
    }

    Ok(())
}
