use clap::{Args, Parser, Subcommand};
use librarian::create_initial_lib;
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
}

#[derive(Args)]
#[command(about = "Create a mild lib from one or more object files.")]
struct CreateArgs {
    #[arg(help = "output library path")]
    output_path: PathBuf,

    #[arg(help = "object files")]
    object_files: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match &cli.command {
        LibrarianCmds::Create(args) => {
            create_initial_lib(&args.output_path, &args.object_files)?;
        }
    }

    Ok(())
}
