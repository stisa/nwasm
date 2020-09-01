NWAsm
=====
Experimenting with a webassembly backend for nim.  

This repository is used for documentation, issue tracking, and other misc things.
The actual code can be found in a fork of Nim [here](https://github.com/stisa/Nim/tree/nwasm)

Compilation is pretty simple (see [Trying out](#trying-out)). 

Notes
-----

I've been writing some [notes](NOTES.md) on how (*I think*) nim compiler works and also random thoughts on how to implement the backend.

Trying out
----------
- Clone: `git clone -b nwasm --single-branch https://github.com/stisa/Nim nwasm`
- `cd nwasm` 
- Compile koch `nim c -d:release koch.nim`
- Compile a test version with wasm support `koch temp -d:debug`

If everything goes well, you will find `nim_temp` inside `bin`. You can use this to compile to wasm,
eg: `nim_temp wasm <file.nim>` (assuming the "./Nim/bin/" folder is in path) and you will find your wasm file, together with a `.html` file with some glue code. 

Passing `-r` when compiling will also generate a `.js` file that will be run with nodejs.

Some files you can run are in [tests/wasm](https://github.com/stisa/Nim/tree/nwasm/tests/wasm).

The glue code and loaders are found in [nimwasm.cfg](https://github.com/stisa/Nim/blob/nwasm/config/nimwasm.cfg), the idea is to make it similar to the template used for `nim doc`, allowing users to change it by writing their own `nimwasm.cfg` in their project root.

Simple things like fizzbuzz can be compiled (minus `echo`, varargs aren't done yet):
```nim
proc log(x:SomeOrdinal) {.header:"glue", importc:"echoInt".}
proc log(x:string) {.header:"glue", importc:"echoString".}

for i in 1..100:
  if i mod 15==0:
    log "FizzBuzz"
  elif i mod 5==0:
    log "Buzz"
  elif i mod 3==0:
    log "Fizz"
  else:
    log i
```

Testament
----------
Testament support is present!

First do `koch temp` to get a test compiler. Then:

```bash
# build testament
cd .\testament
nim c -d:release testament.nim
# run tests
.\testament.exe --nim:"..\bin\nim_temp.exe" cat wasm
# optional: nice html page with test results
.\testament.exe html
```

Contributing
------------

This project is very much WIP. Main goal for now is to get all nim basic features to work. The `gc` implementation should probably only handle `arc` and `orc`.

If you would like to contribute to the code generation, first of all, thank you!  
If you implement some proc or feature, please also add some (simple) tests in [tests/wasm](https://github.com/stisa/Nim/tree/nwasm/tests/wasm).  

If your implementation is particularly complex, please consider also adding an explanation in the notes in this repo or add your own file.

See also the [github project](https://github.com/stisa/nwasm/projects/1) for some ideas on what to do.
