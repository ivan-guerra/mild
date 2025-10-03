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
    Create(CreateArgs),
    Remove(RemoveArgs),
}

#[derive(Args)]
#[command(about = "Create a mild lib from one or more object files.")]
struct CreateArgs {
    #[arg(help = "output library path")]
    lib_path: PathBuf,

    #[arg(help = "object files")]
    obj_paths: Vec<PathBuf>,
}

#[derive(Args)]
#[command(about = "Remove one or more object files from a mild lib.")]
struct RemoveArgs {
    #[arg(help = "output library path")]
    lib_path: PathBuf,

    #[arg(help = "object files")]
    obj_paths: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        LibrarianCmds::Create(args) => {
            librarian::create_lib(&args.lib_path, &args.obj_paths)?;
        }
        LibrarianCmds::Remove(args) => {
            librarian::rm_modules(&args.lib_path, &args.obj_paths)?;
        }
    }

    Ok(())
}
