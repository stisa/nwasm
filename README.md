NWAsm
=====
Experimenting with a webassembly backend for nim.  
Currently tryign to get `system.nim` to compile, roughly 1200 lines out of 4000 work.

Trying out
----------
- Clone: `git clone https://github.com/stisa/nwasm`
- `cd nwasm` 
- Compile koch `nim c -d:release koch.nim`
- Compile a test version with wasm support `koch temp -d:debug`

If everything goes well, you will find `nim_temp` inside `bin`. You can use this to compile to wasm,
eg: `./bin/nim_temp wasm <file.nim>` and you will find your wasm file inside `nimcache`,  
together with a `.html` file with some glue code.

Passing `-r` when compiling will generate a `.js` file that will be run with nodejs.

Some files you can run are in [tests/wasm](tests/wasm).

A nimscript file is provided for convenience, to run tests inside `tests/wasm` with 
`nim e run <nameoftest.nim>`.

See also [ramblings](RAMBLINGS.md) for some random thoughts and [notes](NOTES.md) for some notes on how (*I think*) nim compiler works.
