# Machine Independent Linker (mild)

This project implements a Machine Independent Linker (mild) and a related
librarian application. Both applications are an implementation of the linker and
librarian described in the book ["Linkers and Loaders"][1] by John R. Levine.

The code today only implements features up to and including Project 6-1. The
implementation's object parsing, allocation, and librarian features are
relatively complete and stable. Symbol resolution/management could do with some
refactoring. Relocation isn't implemented at all since that isn't covered until
Chapter 7 of the book.

## Usage

The `mild` linker takes as input a set of object files and prints the contents
of the global symbol table along with the contents of the linked object file to
STDOUT. A collection of mild object files is provided under
[`objects/`](objects/).

```bash
Usage: mild [OBJECT_FILES]...

Arguments:
  [OBJECT_FILES]...  object file
```

The `librarian` application provides a set of commands for creating a directory
format object library. It also includes subcommands for adding, removing, and
replacing library modules.

```bash
Usage: librarian <COMMAND>

Commands:
  create   Create a mild lib.
  rm       Remove modules.
  add      Add modules.
  replace  Replace one module with another.
  help     Print this message or the help of the given subcommand(s)
```

[1]: https://www.amazon.com/Linkers-Kaufmann-Software-Engineering-Programming/dp/1558604960
