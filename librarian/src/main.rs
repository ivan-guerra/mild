use clap::Parser;
use librarian::create_initial_lib;
//use mild::Object;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct LibrarianArgs {
    #[arg(help = "output library path")]
    output_path: PathBuf,

    #[arg(help = "object files")]
    object_files: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = LibrarianArgs::parse();
    create_initial_lib(args.output_path, args.object_files)?;

    Ok(())
}
