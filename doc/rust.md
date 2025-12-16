# Rust Generation Plan

This document outlines the changes required in `hs-apigen` to generate Rust bindings that match the manual implementation in `rs-toxcore-c`.

## Goal

Produce idiomatic Rust bindings that align with the structure and type safety of the manually written code in `rs-toxcore-c/src/core`.

## Required Changes

### 1. Function Naming
**Current:** Generates full C names (e.g., `tox_friend_add`).
**Target:** Strip `tox_` prefix (e.g., `friend_add`).

*   **Action:** Modify `Apigen.Language.Rust.generateMethod` to strip the leading `tox_` from the inferred method name.

### 2. Type Safety (Wrapper Types)
**Current:** Uses `u32` for IDs (e.g., `friend_number: u32`) and `&[u8]` or `Vec<u8>` for keys/data.
**Target:** Use newtype wrappers (e.g., `FriendNumber`, `PublicKey`, `Address`) defined in `crate::types`.

*   **Action:**
    *   Update `Apigen.Language.Rust.toSafeArg` and `toSafeRetType`.
    *   When the semantic type is `SResourceId` (e.g., "Friend"), map to `FriendNumber` instead of checking `knownTypes` and falling back to `u32` or similar defaults logic that might be failing.
    *   Map `SFixedBytes` (e.g., 32 bytes) to reference to wrapper types like `&PublicKey` or `&Address`. This requires checking if the `SFixedBytes` constant matches known sizes (e.g., `TOX_PUBLIC_KEY_SIZE` -> `PublicKey`).

### 3. Module Organization (Extension Impls)
**Current:** Methods are generated in the module of their primary handle. Since `tox_friend_add` takes `Tox*`, it goes into `tox.rs`.
**Target:** Flatten the `core` module. Methods can be generated in separate files/modules (e.g. `friend.rs`, `file.rs`) for organization, but the `core` module (or `generated` module) should re-export everything (e.g. `pub use friend::*;`). This puts `Tox`, `FriendNumber`, `friend_add`, etc., all in the same namespace.

*   **Action:**
    *   Modify `Apigen.Language.Rust.generate` grouping logic.
    *   Ensure the generated `mod.rs` uses `pub use submodule::*;` instead of just `pub mod submodule;`.
    *   Group methods by functionality (e.g., `friend_...` methods in `friend.rs`, `file_...` methods in `file.rs`) as `impl Tox` blocks, but ensure they are accessible via `core::method_name` or `core::Tox::method_name`.

### 4. Ownership and Mutability
**Current:** Often generates `&mut self` for C functions taking non-const pointers.
**Target:** Prefer `&self` for `Tox` methods, as `Tox` is generally treated as shared/thread-safe in the manual bindings (or internal mutability is assumed/handled).

*   **Action:**
    *   Adjust `Apigen.Language.Rust.generateMethod` to default to `&self` for `Tox` methods, or add a configuration/heuristic to override inferred constness.

### 5. Error Handling
**Current:** Returns `Result` with `Tox_Err_...` enum.
**Target:** Matches manual code (mostly), but ensure `ffi_call!` macros are used or equivalent logic is generated to map C enums to Rust Results cleanly.

### 6. Event Dispatch
**Current:** Generates `events.rs` with `ToxEvents` struct.
**Target:** Matches manual `events.rs`. The current generator for events seems reasonably close but needs to be verified against the manual `events.rs` which uses `ToxHandler` trait in `dispatch.rs` (manually written).
*   **Note:** `dispatch.rs` is likely to remain manual or requires a specialized generator. The plan focuses on `tox.rs` and functional modules.

## Implementation Steps

1.  **Refactor Grouping:** Modify `Apigen.Language.Rust.hs` to group methods by "subsystem" prefix (friend, file, conference, etc.) even if they belong to `Tox`.
2.  **Update Type Mapping:** Hardcode or infer mappings for `PublicKey`, `SecretKey`, `Address`, `FriendNumber`, etc., in `toSafeRsType`.
3.  **Rename Methods:** Implement `stripPrefix` logic in method name generation.
4.  **Fix Self:** Force `&self` for `Tox` context arguments.
