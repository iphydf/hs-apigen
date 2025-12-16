# Apigen: Semantic API Design

Apigen is a tool for generating high-quality, idiomatic FFI bindings for
multiple languages (Python, Rust, C++, Haskell, Kotlin, ...) from C headers. It
uses a **Semantic Model** as the intermediate representation to ensure
consistency and safety across all target platforms.

## Project Goals & Requirements

These are the requirements that drive the design. They take precedence over any
incidental detail of the current implementation.

### 1. Inference-driven generation

Running apigen must generate **most of the Python, Rust, and C++ bindings purely
by inference from the C headers**. The generator derives the binding shape from
the structure of the headers themselves:

- **Naming conventions** (`tox_` prefix, `_new`/`_kill`/`_free` lifecycle,
  `tox_self_*`, `get`/`set` accessors, `tox_callback_*` registrars).
- **Parameter ordering and types** (receiver handle first, trailing
  `Tox_Err_* *error`, `const uint8_t *x, size_t length` array pairs).
- **`const`-correctness** — e.g. a `const uint8_t *` array parameter is an
  input; a non-`const` `uint8_t *` buffer with no length is an output.
- **Doc-comments** carried through the parser (the cimple AST preserves
  comments).

All of these are encoded as explicit rules in the extractor. Apigen is a
*convention extractor*, not a general C parser: a header that follows the
TokTok conventions should produce a correct model with no extra input.

### 2. Generate fully — no sidecars, no overlays

- Apigen generates the **whole** binding file(s) for each target. There is no
  `.gen` split and no hand-written overlay that the generated code is merged
  with. "Generate as much as possible, fully."
- There are **no sidecar files** carrying per-API metadata or rename tables.
  Everything the generator needs comes from the headers.
- The one exception is small, static **runtime support** shared by all
  generated modules of a language (e.g. Python's `common.py`: the exception
  classes and length-check helper). This is a hand-maintained library that
  generated code imports — it is not a sidecar of annotations and does not vary
  per API.

### 3. Non-inferrable information lives in the headers

Some facts cannot be derived structurally — principally **nullability**,
**output-buffer sizes**, and **ownership/lifetime of returned pointers**.

- **Long-term:** these are expressed as **annotations in the C headers**
  (e.g. `_Nonnull` / `_Nullable`, and equivalents for output buffers and
  ownership). The experimental headers already use nullability annotations;
  the goal is to extend this to the stable API.
- **For now:** where annotations do not yet exist, apigen falls back to
  **conventions, heuristics, and doc-comment mining**. Nullability is the
  weakest signal and the prime candidate for real header annotations.
- Sidecar annotation files are explicitly **not** used.

### 4. Idiomatic output, with freedom to improve

- Generated bindings should be as **close to the current hand-written
  bindings as possible** in shape and idiom.
- **API compatibility is not a constraint.** The bindings have no external
  users yet, so apigen is free to make the API *better* and more consistent.
  Where the hand-written code has inconsistent one-off renames, the generator
  emits the convention-derived name instead; small churn is acceptable.

### 5. Manual invocation; output committed to git

Apigen is **not** wired into the Bazel build (yet). It is run manually; its
output is committed to the respective binding repositories
(`py_toxcore_c`, `rs-toxcore-c`, `cpp-toxcore-c`).

### 6. Language priority

Backends are driven to completion in this order:

1. **Python** (`py_toxcore_c`, Cython `.pxd` / `.pyx`) — first.
2. **Rust** (`rs-toxcore-c`).
3. **C++** (`cpp-toxcore-c`).

The type stub (`.pyi`) for Python is produced by introspection (`stubgen.py`)
and is out of apigen's scope.

## Core Philosophy

Traditional FFI generators map C syntax directly to the target language,
resulting in "C-in-Python" or "C-in-Rust" APIs. Apigen instead focuses on
**Semantic Extraction**: it identifies the underlying object model (Resources,
Properties, Events) and provides an explicit "recipe" for how each high-level
construct maps to the C implementation.

The design is governed by the **Round-Trip Property**:

> `lift(lower(lift(source))) == lift(source)`

This means the semantic model is stable and deterministic: if you generate C
headers from the model and then re-extract the model from those headers, you
get the identical representation back.

### Convention Enforcement

Apigen relies on strict naming and architectural conventions. If a C API
violates these conventions, the extractor must produce **useful diagnostic
error messages** explaining which convention was violated and where, rather
than silently producing an incorrect model.

## Architecture

The generation pipeline consists of three focused stages:

1.  **Thin Parser**: A minimal layer that converts raw C headers into a basic
    tree of declarations. It ignores any symbols marked as **Deprecated**.
2.  **Semantic Extractor**: The heart of Apigen. It analyzes the raw C
    declarations to identify:
    *   **Resources**: Objects and Entities.
    *   **Hierarchies**: Ownership relationships.
    *   **Properties**: High-level attributes with associated getters and
        setters.
    *   **Events**: Callback registration patterns.
3.  **Language Generators**: Language-agnostic backends that consume the model
    (via JSON) and follow the mapping "recipes" to generate safe, idiomatic
    code.

## The Semantic Model

### 1. Resources & Lifecycles

Resources are categorized into two types:

*   **Handle-based Resources**: Objects identified by an opaque pointer (e.g.
    `Tox *`).
*   **ID-based Resources**: Objects identified by a unique ID type (e.g.
    `Tox_Friend_Number`). To be identified correctly, the identity type name
    must strictly follow the convention of ending in `_Number` or `_Id`.

The model provides enough high-level metadata (prefixes, types) for generators
to **infer constructor and destructor names** (e.g. `tox_new` / `tox_kill`)
without them being explicitly marked in the model. All resources, including
configuration structs like `Tox_Options`, are treated as **opaque handles**
accessed only via getters and setters.

### 2. Properties & Methods

*   **Byte Arrays vs. Strings**: Parameters of type `uint8_t[]` (names, status
    messages, packet data) are lifted as high-level **byte arrays**, as the
    protocol may not guarantee UTF-8 validity. Parameters of type `char *`
    (e.g. hostnames) are lifted as **strings**. The C element type alone
    distinguishes the two.
*   **Associated Constants**: Numeric constants used for buffer sizes or limits
    are preserved in the model and associated with the relevant properties or
    methods. Zero-argument size/limit functions (`tox_*_size`, `tox_max_*`)
    are also surfaced as module-level constants.
*   **Distinct Semantic Types**: Types like `size_t` are preserved as distinct
    semantic types (e.g. `SSizeT`) to maintain perfect fidelity in function
    signatures during code generation.
*   **Buffer Patterns**: The "size-then-get" pattern (e.g.
    `tox_friend_get_name_size` + `tox_friend_get_name`) is collapsed into a
    single high-level property or method returning a byte array. The model
    preserves the explicit `sizeGetter` mapping so the size function is
    correctly generated and round-tripped.
*   **Property vs. Method**: An accessor whose only argument is the receiver is
    lifted as a property; an accessor that takes further arguments (e.g. a
    `friend_number`) is lifted as a method. A getter/setter pair becomes a
    read-write property.

### 3. Error Handling

The model explicitly links functions to their corresponding **Error Enums**.

*   **Error Dominance**: If a function takes an `ErrorPtr`, the generator must
    prioritize the error code over the function's return value. Redundant
    return values (e.g. a `bool` indicating success) are ignored by the
    high-level API in favor of inspecting the error enum.
*   **Success Value Convention**: All error enums MUST contain a member that
    simplifies to exactly `"OK"` (after stripping the common prefix). This is
    enforced as the universal success indicator.

Every high-level call includes a `CFunctionMapping` recipe, enriched with
enough metadata to perfectly reconstruct the original C function signature:

*   `ThisObject Constness`: The primary resource handle, preserving its
    `const`-correctness.
*   `ParentObject Constness`: The owning parent handle.
*   `ResourceId`: The unique ID for sub-resources.
*   `SemanticArg n`: Mapping to high-level arguments.
*   `ErrorPtr`: Pointer to the associated error enum.
*   `BufferSize`: Automated length handling for byte arrays.
*   **Exact C Metadata**: The mapping stores the original `cReturnType`,
    `cParamNames`, and `cSemParams` so the generated C headers are identical to
    the source.

### 4. Events

C callbacks are lifted into `SEvent` objects.

*   **Transient Context**: The `user_data` pointer is treated as a transient
    bridge for the FFI layer and is hidden from the high-level API.
*   **Special Cases**: While most callbacks follow the event pattern triggered
    during an iteration loop (e.g. `tox_iterate`), specific callbacks like
    `log` may require special handling in the mapping recipe.

## JSON Representation

The final output of the extractor is a single, deterministic JSON file. This
allows backends in any language to generate bindings without needing to
implement complex C parsing or heuristic logic themselves.

## Target Languages

| Language | Repository       | Form                       | Status        |
|----------|------------------|----------------------------|---------------|
| Python   | `py_toxcore_c`   | Cython `.pxd` / `.pyx`     | in progress   |
| Rust     | `rs-toxcore-c`   | safe wrappers over bindgen | exists, WIP   |
| C++      | `cpp-toxcore-c`  | single-header idiomatic API| exists, WIP   |
| Haskell  | —                | c2hs raw + safe layer      | raw done      |

See `doc/rust.md` and `doc/haskell.md` for backend-specific notes.
