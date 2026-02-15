ISSUE: Backend support for integer widths (int8..int64, uint8..uint64)

Background
----------
The frontend now recognizes integer widths and unsigned variants (int8/int16/int32/int64, uint8/uint16/uint32/uint64). Sema treats the integer family as compatible and promotes arithmetic to 64‑bit for now.

Goal
----
Implement correct backend code generation for integer widths and unsigned semantics, i.e. ensure loads/stores, arithmetic and comparisons obey target width and signedness.

Proposed changes
----------------
1. IR (ir/ir.pas)
   - Add operations: irSExt (sign-extend), irZExt (zero-extend), irTrunc (optional)
   - Optionally allow TIRInstr to carry a width field or type annotation for temps.

2. Lowering (ir/lower_ast_to_ir.pas)
   - When lowering constants or loads into temps, insert SExt/ZExt to normalize to the operation width.
   - Before a binary operation, ensure both operands are extended to a common width and sign convention.
   - For assignments to smaller targets, emit truncation/masking instructions or use irTrunc.

3. Emitter (backend/x86_64/x86_64_emit.pas)
   - Implement encoding for irSExt / irZExt using movsx/movzx.
   - For comparisons, choose signed vs unsigned sets/jump instructions (e.g. setl vs setb or jl vs jb semantics).

4. Tests
   - Unit tests for Parser+Sema accepting the widths.
   - Lowering tests to inspect IR (compare for SExt/ZExt presence).
   - End2end tests: compile sample programs using small widths and validate behavior (e.g. unsigned wrap-around, comparisons).

Estimated effort
----------------
- Design and IR changes: 0.5–1 day
- Lowering changes: 0.5–1.5 days
- Emitter changes + tests: 1–2 days
Total: 2–4 days depending on edge cases and test coverage.

Notes
-----
- A simpler interim approach is to keep promoting to 64‑bit everywhere and only add explicit SExt/ZExt for loads/stores. This is less correct but much faster.
- Architecture decisions (e.g., whether IR temps carry types) affect implementation complexity.
