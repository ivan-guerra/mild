pub mod alloc;
pub mod object;
pub mod symbols;

// Re-export commonly used items for convenience
pub use alloc::allocate;
pub use object::{load_object, Object};
pub use symbols::{collect_global_symbols, GlobalSymbolTable};
