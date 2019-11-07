Bolting AVX/AVX2 into the OCaml compiler
===

Ideas
===
* Inductive datatypes like lists/trees automapped to SIMD functions

CAVEATS
===
* No bounds checking / size checking implemented. Assuming perfect
vectors divisible by 4

Attempt 1
---
Provide new functions in the Array stdlib implemented as c-intrinsics,
check out `runtime/array.c` for the functions.


Status:

* `avx_bcast_inplace` : inplace update an array with a value
* `avx_mem` : check if an element present in array

Observations:

* Heap alignment is a major issue for using AVX2 ops
