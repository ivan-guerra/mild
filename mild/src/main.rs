use clap::Parser;
use mild::{allocate, collect_global_symbols, load_object, Object};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct MildArgs {
    #[arg(help = "object file")]
    object_files: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = MildArgs::parse();
    let object_files: Vec<Object> = args
        .object_files
        .iter()
        .map(|path| load_object(path))
        .collect::<Result<Vec<_>, _>>()?;
    let mut global_symtab = collect_global_symbols(&object_files)?;
    let allocated = allocate(&object_files, &mut global_symtab)?;

    println!("{:#?}", global_symtab);
    println!("{}", allocated);

    Ok(())
}
