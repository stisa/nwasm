Write Your Own Nim Code Generation Pass
=======================================

Warning: The content of this post is mainly derived from my own understanding of the code generation pass 
of the Nim Compiler. As such, there may be errors or misrepresentations. 
If you find one, please report it (eg. as a github issue).

The Nim compiler is structured as a series of passes, which to my understanding is pretty common as far 
as compilers go. In this post we are mainly interested in the code generation pass, that takes place at
the end of the compilation process and is responsible of the conversion from Nim AST to the actual
code, be it C, C++, Js, LLVM,... etc that will then be used to run the program. It also deals with
writing out these files to disk, and in the case of C/C++ can (will?) deal with symbol files etc.

A pass is defined as a tuple of procedures

``` nim
type
  TPass* = tuple[open: TPassOpen, openCached: TPassOpenCached,
                 process: TPassProcess, close: TPassClose,
                 isFrontend: bool]
```

The proc `makePass` creates the pass from four other functions.
These four function are:
- an `open` function, that is called at the start of compilation of every nim module
- an `openCached` function, which I don't know too much about. It's probably used for symbol files reading to speed up recompilations
- a `process` function, that is called for every top level statement of a given module
- a `close` function, that is called when the end of a module is reached.

![Code Flow Graph]()



`open` proc
-----------

This proc is called when starting to generate code for any given module.
It is usally responsible for creating the pass context if every nim module is compiled separately (like in the C backend) or updating a single context (eg. the JS backend uses a context stored in a global variable). Instead of a global var, the `ModuleGraph.backend` can be used to store a `ref object` with data to be persisted, like a list of processed modules etc.

The pass context should extend `PPassContext`:

```nim
type MyGenModule = ref object of TPassContext
  code: Rope
  module: PSym # nim module symbol
```

In this post we will extend the module graph to keep track of code from processed modules.

``` nim
type MyList = ref object of RootObj
  code: Rope # Code for all previously generated modules
  modules: seq[MyGenModule]
```

The `open` proc looks something like this:
```nim
proc newMyList(graph: MOduleGraph): MyList =
  MyList(code: rope(""), modules: @[])

proc newMyModule(ml: MyList, s: PSym): MyGenModule =
  result = MyGenModule(code: rope(""), module: s)
  ml.modules.add(result)

proc myOpen(graph: ModuleGraph; s: PSym; cache: IdentCache): PPassContext =
  if graph.backend.isNil: graph.backend = newMyList(graph)
  let ml = MyList(graph.backend)
  result = newMyGenModule(ml, s)
```
It simply adds our generated module list to the module graph if not present and initializes a new generated module.

`openCached` proc
-----------------
We won't deal with this proc in this post, so we can just error out.
```nim
proc myOpenCached(graph: ModuleGraph; s: PSym, rd: PRodReader): PPassContext =
  internalError("symbol files not implemented yet")
  result = nil
```

`myProcess` proc
----------------

This is the most complex part of the code generation. It needs to translate nim AST to equivalent code for the target language.
It's common to have a main `gen` proc that takes the `PNode` passed to the process proc, with branches dealing with specific ast node kinds.

The structure of the `gen` proc is completely up to you, as long as it does something with the `PNode` that needs to be processed. In this post we will just deal with **simple** variables declaration, directly added to the code.


```nim
proc gen(m: MyGenModule, n: PNode) =
  case n.kind:
  of nkVarSection, nkLetSection:
    # echo treeToYaml n
    let mut = n.kind == nkVarSection
    for iddef in n.sons:
      if iddef[namePos].kind == nkSym: # Top level symbols/sections
        let varname = iddef[namePos].sym.name.s
        m.code & "$nlet $1 $2 = $3$n" % [
          if mut: "mut" else: "", 
          varname,
          newLiteral(iddef[2])
        ]
  else:
    echo "CodeGen for " & $n.kind & " not implemented"
    discard

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n

  var m = MyGenModule(b)
  m.gen(n)
  result = n
```

`myClose` proc
--------------
This is where we close up and finalize a generated module.
```nim
proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  result = myProcess(b, n)
  var m = MyGenModule(b)
  if m.s.flags.contains(sfMainModule):
    let ext = "rs" # extension of target language, eg. Rust
    let f = m.s.info.fileIndex.toFilename
    let outfile =
      if options.outFile.len > 0:
        if options.outFile.isAbsolute: options.outFile
        else: getCurrentDir() / options.outFile
      else:
        changeFileExt(completeCFilePath(f), ext)
    writeFile(outfile, $MyList(graph.backend).code)
    # compile?
```

We process any remaining statements, then if the module we are closing is the main module, we know that the compilation is finished, so we write out
the file, and we are done. The code generation is complete, now we need to
compile the generated code (if the target language needs it).
