Notes
=====

This document contains random thoughts and notes on how I should implement the codegen and how I understand the compiler generation pass. 

Please note that this document is *very* WIP, I may start talking about something and get sidetracked. I'll try to keep rewriting sections of it until it makes sense, so if you have questions or find something unclear, please write an issue or notify me some other way.

### Compiling

```
.\\bin\\nim_temp.exe wasm -r --gc:none -d:noSignalHandler -d:debug .\\bin\\test.nim 
```

Codegen pass
------------

Here is what I understand of the code generation pass:  
the generator module (cgen, jsgen, vmgen etc) exports a `const`, the pass, that receives the module graph (which to my understanding is the ast after it's type checked and lowered).   
This pass is composed of 4 procs, normally called ``myOpen, myOpenCached, myClose, myProcess``.

These four procs operate on (the same) context object, that generally holds a rope structure which holds the target code generated.

`myOpen` receives the module symbol and is the place in which the context is initialized.   
Then `myProcess` is called for every top level statement, and is where the ast of the statement is converted to target source code (usually by a proc named `gen`).  
Node kinds that may appear here (may not be inclusive):
```
nnkStmtList
nnkRoutineKinds(procdef, etc)
nnkInclude, nnkImport, nnkExport
```
Lastly, `myClose` is called, which closes the module.

These passages are repeated for all modules used by the project.

A simplified graph:
![flow](flow.jpg)

Overview
--------

The main idea behind this backend is to transform nim ast into "wasm" ast. This ast is defined in [compiler/wasm/wasmast](https://github.com/stisa/Nim/tree/nwasm/compiler/wasm/wasmast.nim), and doesn't follow a standard, at least for now. 
The wasm generation is divided in 2 main phases:
1. nim ast -> wasm ast
2. wasm ast -> bytecode

The first phase happens in `wasmgen`, and is the actual backend nim sees.
The other phase happens in `wasmencode` and is mostly decoupled from the previous, meaning that it could be "easily" changed to output wast (wasm textual representation), an html page with inlined js and wasm as an array of uint8, ..., etc.

Another module that may be of interest is `wasmrender`, which tries to print out a json-like representation of the wasm ast.

### File list
Most of the relevant files are either in a wasm folder or have wasm in the name (wasmgen wasmutil wasmsys...) , the changes to other files are just one or two lines to make nim recognize wasm as a target. The actual wasm translation is only in `wasmgen`.

- `compiler/wasm` contains wasm ast definition, along with some convenience procs, json-like renderer, encoder to wasm bytecode etc
- `compiler/wasm/wasmglue.tmpl` contains the templates for html and js parts of the generator. The Js part is used when passing `-r` to run via nodejs
- `compiler/wasmgen.nim` (and `wasmutils.nim`) is the code generation pass
- `tests/wasm` contains some tests, that may be run with testament (`testament cat wasm`??)


Nim to Wasm
-----------

First of all, only _ONE_ wasm module is produced. Much like it happens with the js backend, only the functions actually called in the _main_ module are compiled.  
As an example, lets say we have module A with 100 procs, and the main module B which imports module A and calls a single function, lets say `a`. Only the proc `a` is actually produced in the wasm module.
```nim
# module A
proc a(x: int): int = x*2
#proc b...
#...proc hundred

# module B
import A
var x = 123
echo a(x)
```
Once `echo a(x)` reaches `myProcess`, and the function `a` goes in `gen`, we have 3 possibilities:
1. `a` is a magic proc
2. `a` is a proc we already generated
3. `a` is a proc we need to generate

Lets see these cases:
1. each magic is a bit different and is handled in `genCallMagic`
2. lucky! Do nothing
3. the proc generation happens in `genProc`

After this, the arguments are generated and lastly the call is generated. Note we distinguish imported (from js) procs as they are hoisted in the function index space, so the index needs to be adjusted in the generation pass. I don't particularly like this coupling, but it's the simplest way I could think of. 

Every `var/let/const` section is handled in `gen<Var|IdentDef>`.

Top level (ie. `sym.owner.kind == skModule`) definitions are declared as wasm globals if they're `let` or `const`. Further, `let` are _mutable_ globals. The `sym.loc.pos` is the index into global space, and the `k` is `locGlobalVar`, while the `storage` is `OnStatic`.  
Proc level (ie `sym.owner.kind != skModule`) definitions go into local space. The `sym.loc.pos` is the index into locals space, `storage` is `OnStatic`.  
Top level `var` go in linear memory, they `loc.pos` is the position in linear memory and `loc.storage` is `OnHeap`.

Note: locals and arguments are ~same in wasm. This means that, for a proc
```nim
proc sum(a,b:int) =
  var s = a+b
  echo s
```
`a`,`b` are respectively `0, 1` in the local space, while `s` should be `3`.

For numeric types, this is pretty easy as they can be cleanly mapped to wasm types.  
For the rest, we have two possibilities:
1. the owner of the symbol is a module ( `=>` top level)
2. the owner is a proc ( `=>` definition is inside a proc)

In case **1**, we store the var linearly in the data section of the wasm module, and store the adress of the first
byte in the global.
As an example, let's consider `var x: seq[int32]`. Notice I didn't initialize the seq.
The pointer to the length of the seq is stored in a global (as `i32`). In this case, since the seq is `nil`, it is just 0.
The initialization of the seq will store a length+data pair, sequentially, on the linear memory and update the pointer reserved
before to point to the length. Strings, arrays, object, refs and pointers work in a similar way.
TODO: For sets, assume they're small enough to fit in a int32.

Case **2** is still in flux.
The idea is to do the same as case 1, but substituting locals to globals. We should also keep track of the index in linear memory we were at when we went in the proc, and clear out the memory between that index and current index once we exit the proc ( or dont? we can just roll back the stackpointer and avoid assuming memory is all 0 on gen)

#### Result

In case the proc defines a result, add an implicit result local to procs. 
(ie if the result ``wasmtype != vtnone``)  
ideas for this:
1) This result local should be a pointer
to a memory location outside the proc. This means the wasm functions don't actually return a value, but merely modify the 
value that is injected.
1) A local, with the typ defined as the type of the result var. From the pow of the rest, this just means that local 0 is the result, and everything happens normally. When exiting the proc, a `return result` is added. This also means I need to inject a 0 value with appropiate type when generating the call, and sliding back the stackptr to free up stuff won't work, unless I make it ignore memory used by the result, but this will leave holes. God do I need a gc? :(


### Exceptions
Forward exceptions to the js side, eg. by a `throw new Error(toJsStr(<wasmstringmessage>))`.

### Semantics
I **really** need to solidify a set of semantics and stick to it.

**tyString, tySeq** are represented by a pointer to a length+cap+data block, eg:
```
  [ptr][otherdata...][len][cap][str/seq data]
                      ^
                      ptr points to here
```
**tyPtr,tyRef** etc (pointers types basically) are a ptr to a data block (like above, but no `len` or `cap` blocks)

**tyArray** is a nice chunk of memory that gets passed around as a pointer to the first byte of the array by
the backend. This pointer is probably stored in a global/local, while the array lives in the data section.

**tyInt, tyFloat, tyChar, tyBool** etc, basically everything that is representable as a single wasm value (btw wasm
values are i32, i64, f32, f64, and i64 doesn't even existing in practice as the version implemented in browser is
limited to 32bits integers) is directly stored in globals/locals. Thus these are passed around by value. What happens when I need
to pass a value as a `tyVar`? Good question. I'm not sure.


### Bytes alignment
Need to investigate if bytes alignment is going to mess with my carefully packed memory structure. Maybe I should implicitly add `{.packed.}` to everything?

## Testament
First do `koch temp` to get a test compiler.
Then:
```bash
# build testament
cd .\testament
nim c -d:release testament.nim
# run tests
.\testament.exe --nim:"..\bin\nim_temp.exe" cat wasm
# optional: nice html page with test results
.\testament.exe html
```

References and links
------------
Some links that may prove helpful:

- [Go approach to WAsm](https://docs.google.com/document/d/131vjr4DH6JFnb-blm_uRdaC0_Nv3OUwjEY5qVCxCup4/preview#)
- [wat2wasm](https://cdn.rawgit.com/WebAssembly/wabt/aae5a4b7/demo/wat2wasm/)
- [wasm2wat](https://cdn.rawgit.com/WebAssembly/wabt/aae5a4b7/demo/wasm2wat/)
- [spec](https://webassembly.github.io/spec/core/index.html)
- [wee_alloc](http://fitzgeraldnick.com/2018/02/09/wee-alloc.html)
- https://www.assemblyscript.org/editor.html
- https://webassembly.github.io/wabt/demo/wat2wasm/index.html
- https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md
- https://dev.to/xflywind/how-to-use-testament-in-nim-1l0h
- https://gist.github.com/dschuff/f0f4c7d3a1f09001e83a33cc9a20aebd switch/case
- https://www.assemblyscript.org/editor.html
  
### Assemblyscript constructs for reference
#### Case/Switch
```typescript
var i = 2
var res = 0
switch(i) { 
   case 1: { 
      res = 1;
      break; 
   } 
   case 2: { 
      res = 2;
      break; 
   } 
   default: { 
      res = -1;
      break; 
   } 
}
```
```wat
block $break|0
  block $case2|0
    block $case1|0
      block $case0|0
        global.get $module/i
        local.set $0
        local.get $0
        i32.const 1
        i32.eq
        br_if $case0|0
        local.get $0
        i32.const 2
        i32.eq
        br_if $case1|0
        br $case2|0
      end
      i32.const 1
      global.set $module/res
      br $break|0
    end
    i32.const 2
    global.set $module/res
    br $break|0
  end
  i32.const -1
  global.set $module/res
  br $break|0
end
```
#### If/Else
```ts
var i = 2
var res = 0
if (i == 1) {
  res = 1;
} else if (i == 2) {
  res = 2;
} else {
  res = -1;
} 
```
```wat
global.get $module/i
i32.const 1
i32.eq
if
  i32.const 1
  global.set $module/res
else
  global.get $module/i
  i32.const 2
  i32.eq
  if
  i32.const 2
  global.set $module/res
  else
  i32.const -1
  global.set $module/res
  end
end
```
#### While loop
```ts
var i = 0
while (i<2){
  i += 1
}
```

```
loop $while-continue|0
  global.get $module/i
  i32.const 2
  i32.lt_s
  local.set $0
  local.get $0
  if
    global.get $module/i
    i32.const 1
    i32.add
    global.set $module/i
    br $while-continue|0
  end
end
```

Nim's `If` and `Case` can be expressions, meaning they return a result. Luckily, wasm blocks can return a value, so this should be relatively straightforward to implement
```
(block (result i32)
  ....
  leave something on the stack for result, or
  return something
)
```
It's probably enough to just expose the `sig` param for `newIf/newIfElse` etc

### Switch and br_table (stmt and expr versions)
from https://github.com/WebAssembly/testsuite/blob/master/br_table.wast

```
  (func (export "multiple") (param i32) (result i32)
    (block
      (block
        (block
          (block
            (block
              (br_table 3 2 1 0 4 (local.get 0))
              (return (i32.const 99))
            )
            (return (i32.const 100))
          )
          (return (i32.const 101))
        )
        (return (i32.const 102))
      )
      (return (i32.const 103))
    )
    (i32.const 104)
  )

  (func (export "multiple-value") (param i32) (result i32)
    (local i32)
    (local.set 1 (block (result i32)
      (local.set 1 (block (result i32)
        (local.set 1 (block (result i32)
          (local.set 1 (block (result i32)
            (local.set 1 (block (result i32)
              (br_table 3 2 1 0 4 (i32.const 200) (local.get 0))
              (return (i32.add (local.get 1) (i32.const 99)))
            ))
            (return (i32.add (local.get 1) (i32.const 10)))
          ))
          (return (i32.add (local.get 1) (i32.const 11)))
        ))
        (return (i32.add (local.get 1) (i32.const 12)))
      ))
      (return (i32.add (local.get 1) (i32.const 13)))
    ))
    (i32.add (local.get 1) (i32.const 14))
  )
```

### Var strings/seqs
eg add(s, s2)

should be:
```pseudo
if cap(s)> len(s)+len(s2):
  # don't need to move s somewhere else
  appendbytes(from s2 data, to s data)
  set len(s) to len(s)+len(s2)
else:
  # copy s at the bottom of the stack, so that at the end of data is just free space
  copymem(s, stackbottomptr, bytelen(s))
  appendbytes(from s2 data, to (new)s data)
  set len(s) to len(s)+len(s2)
  set cap(s) to some sensible number?
  set global(s) (new)s pos
```
What to do with the space left free after moving s?
The gc holds a list of holes and it can fill it if the value
to store is small enough?

What if there's no more space in the memory? => need to call mem.grow