# Haskell Bindings Generation Plan

## Goal
Generate a safe, idiomatic, and ABI-correct Haskell binding for `c-toxcore` using `apigen` and `c2hs`.

## Architecture

The generation is split into two layers:

1.  **Raw FFI Layer (`FFI.Tox.Raw`)**:
    *   **Tool:** `c2hs` (via `apigen` generating `.chs`).
    *   **Role:** Defines the exact C ABI grounding.
    *   **Content:**
        *   `{# enum ... #}`: Haskell `Enum` types matching C enum values.
        *   `{# pointer ... #}`: Haskell `Ptr` types for opaque structs.
        *   `{# fun ... #}`: Haskell FFI hooks with correct arity and basic types.
        *   Callbacks defined as `FunPtr`.
    *   **Safety:** Ensures compile-time verification against `tox.h`.

2.  **Safe Wrapper Layer (`FFI.Tox.*`)**:
    *   **Tool:** `apigen` (generating `.hs`).
    *   **Role:** Provides the idiomatic Haskell API.
    *   **Content:**
        *   Marshals `ByteString` <-> `CString`.
        *   Marshals Enums (if not handled by Raw, but Raw now handles it).
        *   Uses `ForeignPtr` for resource management (`tox_kill` as finalizer).
        *   Handles error codes -> `Either ErrorType Result`.
        *   Exposes properties as Haskell functions.

## Implementation Steps

### Phase 1: Raw Layer (Completed)
- [x] Integrate `c2hs` into Bazel build (`third_party/haskell/c2hs`).
- [x] Implement `generateRaw` in `hs-apigen` to produce `.chs`.
- [x] Verify `c2hs` catches API mismatches (arity, missing symbols).
- [x] Support `FunPtr` for callbacks.
- [x] Support `{# enum #}` for accurate Enum value mapping.

### Phase 2: Safe Layer (In Progress)
- [ ] Implement `generateSafe` in `hs-apigen`.
- [ ] Use `ForeignPtr` for `Tox`, `ToxAV` handles.
- [ ] Generate pure wrappers for getters.
- [ ] Generate `IO` wrappers for actions.
- [ ] Verify compilation of Safe layer against Raw layer.

### Phase 3: Migration
- [ ] Switch `hs-toxcore-c` to use the new generated modules.
- [ ] Run existing tests to ensure no regression.

## Development & Debugging

To generate the bindings and verify the output:

1.  **Build the API target:**
    ```sh
    bazel build //hs-toxcore-c:api
    ```

2.  **Inspect the generated Raw layer:**
    The generated `.chs` file is located in the bazel output directory:
    ```sh
    cat bazel-bin/hs-toxcore-c/src/FFI/Tox/Raw.chs
    ```

3.  **Inspect the generated Safe layer:**
    The generated Haskell files are in the same directory:
    ```sh
    ls bazel-bin/hs-toxcore-c/src/FFI/Tox/*.hs
    ```