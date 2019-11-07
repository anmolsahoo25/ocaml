Bolting AVX/AVX2 into the OCaml compiler
===

Ideas
===
* Inductive datatypes like lists/trees automapped to SIMD functions

Attempt 1
---
Provide new functions in the Array stdlib implemented as c-intrinsics,
check out `runtime/array.c` for the functions.

Observations:

* Heap alignment is a major issue for using AVX2 ops
