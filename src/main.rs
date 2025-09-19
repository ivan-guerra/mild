use clap::Parser;
use std::path::PathBuf;

mod object;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct MildArgs {
    #[arg(help = "object file")]
    object_file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = MildArgs::parse();
    let object = object::load_object(&args.object_file)?;

    println!("{}", object);

    Ok(())
}
