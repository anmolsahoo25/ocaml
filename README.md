Bolting AVX/AVX2 into the OCaml compiler
===

Attempt 1
---
Provide new functions in the Array stdlib implemented as c-intrinsics,
check out `runtime/array.c` for the functions.
