# ApiGen

FFI API generator for TokTok style C API headers.

## Usage

ApiGen can generate bindings for various languages or output the semantic model in different formats.

```bash
apigen --json model.json --strict input.h
apigen --hs-out src/ --strict input.h
```

See `apigen --help` for more details.
