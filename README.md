NWAsm
=====
Experimenting with a webassembly backend for nim.  
Currently trying to get `system.nim` to compile, roughly 1200 lines out of 4000 work.

This repository is used for documentation, issue tracking, and other misc things.
The actual code can be found in a fork of Nim here: https://github.com/stisa/Nim/tree/nwasm

This split-branch model was chosen to allow easy comparison with the main nim branch
via github fork interface, simple compilation (see [Trying out](#trying-out)). 
Also if/when, in the future, the wasm backend is mature enough, merging into upstream would
be a simple pull request.

Notes
-----

I'm writing some [notes](NOTES.md) on how (*I think*) nim compiler works and also random thoughts on how to implement
the backend.

Trying out
----------
- Clone: `git clone -b nwasm --single-branch https://github.com/stisa/Nim`
- `cd Nim` 
- Compile koch `nim c -d:release koch.nim`
- Compile a test version with wasm support `koch temp -d:debug`

If everything goes well, you will find `nim_temp` inside `bin`. You can use this to compile to wasm,
eg: `./bin/nim_temp wasm <file.nim>` and you will find your wasm file inside `nimcache`,  
together with a `.html` file with some glue code.

Passing `-r` when compiling will generate a `.js` file that will be run with nodejs.

Some files you can run are in [tests/wasm](https://github.com/stisa/Nim/tree/nwasm/tests/wasm).
