NWAsm
=====

Experimenting with a webassembly backend for nim.  
Currently tryign to get `system.nim` to compile, roughly 1200 lines out of 4000 work.
Trying out
----------
Short version: you can't yet. I still need to push a compatible `system.nim`
Once I'll have that done, you can:
- clone this repo
- `cd <yourRepoFolder>`
- set `basePath` in `nim.cfg` to `<yourRepoFolder>`
- `git clone https://github.com/nim-lang/Nim`
- copy `compiler/` into `Nim` (overwriting files)
- copy `config/` into `Nim` (overwriting files)
- `nim e build.nims`
- `nim e test.nims`


Ramblings
---------
This section contains random thoughts on how I should implement the codegen. Why? Because I like to write
my ideas down, it helps me think. *Warning*: hobby, self-taught "programmer".

### Exceptions
Should I forward exceptions to the js side, eg. by a `throw toJsStr(<wasmstringmessage>)`, or just `trap` in wasm?
Meh. `trap`for now.

### Semantics
I **really** need to solidify a set of semantics and stick to it.

**tyString, tySeq** are represented by a pointer to a length+data block, eg:
```
  [ptr][otherdata...][len][str/seq data]
                      ^
                      ptr points to here
```
**tyPtr,tyRef** etc (pointers types basically) are a ptr to a data block (like above, but no `len` block)

**tyArray** is a nice chunk of memory that gets passed around as a pointer to the first byte of the array by
the backend, but this pointer is never actually stored in the wasm binary. Which to my understanding is pretty normal,
it's basically an implicit pointer.

**tyInt, tyFloat, tyChar, tyBool** etc, basically everything that is representable as a single wasm value (btw wasm
values are i32, i64, f32, f64, and i64 doesn't even existing in practice as the version implemented in browser is
limited to 32bits integers) is directly copied. Thus these are passed around by value. What happens when I need
to pass a value as a `tyVar`? Good question. I'm not sure. Atm every is in the nice, big heap, also known as `WebAssembly.Memory`. Why? It was simpler to understand. Also the logic for store/load operation is easier if I don't
need to distinguish between things local to a particular function and everything else. The single var introduced in wasm
functions is `result`.

### Global/Local/Heap
Should I use wasm locals/globals? The problems is where to store the map from `PSym` to an index into the local/global space.
In an ideal world, I could simply make use of `TLoc`, but sadly that is optimised for rope structure as used by all other
nim backends. I could use the `k` field of `TLoc` and introduce a `b: int` field to store the byte offset in memory I guess,
maybe if I put it around a `when defined(hasWasmBackend)` nim maintainers won't hate me too much.
Currently, I use the offset field in `PSym`. This seems to work well enough, for now.

### Bytes alignment
I really need to drop the 4byte alignment for everything. It is especially bad when using the `getSize` proc, as an object
with 4 bool fields ends up 16bytes instead of the reported 4bytes, killing al subsequent store/load ops. The problem then 
becomes how to do load/store things smaller than 4 bytes in wasm. This could probably be overcome with a nice usage of
my super awesome `mapKind / mapStoreKind / mapLoadKind` triplet of procs, doing something like `i32.load8` and dealing with
the added complication of determining the correct offset. This is probably with I'll end up doing in the long run, probably
after I get to around half of `system.nim`, just to avoid having that nice feeling of satisfaction you get when completing
something. Also because by then I should have a rough implementation of most comparisons for ints, so I can ensure I don't
break everything by writing a bunch of test importing `console.assert` and feeding them to a `nodejs` runner.


