use clap::Parser;
use std::path::PathBuf;

mod alloc;
mod object;
mod symbols;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct MildArgs {
    #[arg(help = "object file")]
    object_files: Vec<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = MildArgs::parse();
    let object_files: Vec<object::Object> = args
        .object_files
        .iter()
        .map(|path| object::load_object(path))
        .collect::<Result<Vec<_>, _>>()?;
    let _allocated = alloc::allocate(&object_files)?;
    let global_symtab = symbols::collect_global_symbols(&object_files)?;

    println!("{:#?}", global_symtab);
    //println!("{}", allocated);

    Ok(())
}
