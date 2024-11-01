# eva

Eva is an EVM assembler.

It is designed to be simple, support simple expressions for computing things such as jump offsets and code lengths, and can compute optimal `PUSH*` instruction sizes to use.

## Usage

The simplest way to invoke the assmebler is with:

```sh
eva code.evm
```

This will generate EVM bytecode in raw binary format, and write it to `code.bin`. The assembler also supports additional flags to control the output file, and write the output in hex format:

```sh
eva input.evm -o output.bin -x
```

In order to view full usage documentation, run:

```sh
eva --help
```

## Remaining Work

- [ ] Consider expanding the expression language to allow additional features such as:
  - Conditions
  - Binary operations
- [ ] Improve error reporting, right now there basically isn't any!
- [ ] Change samples to build with Dune
