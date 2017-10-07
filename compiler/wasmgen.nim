import
  ../Nim/compiler/[ast, astalgo, options, msgs, idents, types, passes, rodread,
  ropes, wordrecg,extccomp],
  ospaths, tables, os, strutils,
  wasmast, wasmstructure, wasmencode, wasmnode, wasmleb128, wasmrender, wasmutils

from math import ceil,log2

from ../Nim/compiler/modulegraphs import ModuleGraph

include "../lib/wasm/wasmglue.templ" # TODO: check out how nimdoc does overloading of template

type
  WasmGen = ref object of TPassContext
    initExprs: seq[WasmNode] # sequence of initializer expressions. for use in start sec
    nextimportIdx: Natural # function index space ( doesn't account for hoisting of imported procs )
    nextFuncIdx: Natural # the function index space (only for non-imported funcs)
    nextGlobalIdx: Natural # the global index space
    nextMemIdx: Natural # the linear memory index space
    nextTableIdx: Natural # the table index space
    s: PSym # symbol of the current module, taken from myOpen
    m : WasmModule #current module
    generatedProcs: Table[string,tuple[id:int,imported:bool]] # name, funcIdx
    generatedTypeInfos: Table[string, int32] # name, location in memory
    stack: tuple[top,bottom:int32] # stack pointers location, used in procs?
  
proc newWasmGen(s:PSym): WasmGen =
  result = WasmGen(
    generatedProcs: initTable[string,tuple[id:int,imported:bool]](),
    generatedTypeInfos: initTable[string,int32]()
    )
  result.s = s
  result.nextFuncIdx = 0
  result.nextImportIdx = 0
  result.nextGlobalIdx = 0
  result.nextTableIdx = 0
  # 4 byte aligned, reserve 8 bytes to store the stack pointer
  # This mean effective address start at 12?
  result.nextMemIdx = 12
  result.initExprs = newSeq[WasmNode]()
  result.m = newModule(s.name.s) #gen.modules[^1]
  #initialize the module's sections
  result.m.memory = newMemory()
  add result.m.exports, newExport(0, ekMemory, "$memory")

const 
  passedAsBackendPtr = {tyVar, tyObject}
  heapPtrLoc = 4'i32

var genCtx : WasmGen

proc gen(w: WasmGen, n: PNode):WasmNode

proc mangleName(s:PSym):string = 
  echo "# mangleName ", s.name.s, " " , s.kind #,"\n",typeToyaml s.typ
  case s.kind:
  of skType:
    result = s.name.s.mangle & $s.typ.kind
  else:
    result = s.name.s.mangle
    for tson in s.typ.sons:
      if not tson.isNil:
        result.add("_" & $tson.kind)
    
proc getArrayLen(t:PType): int =
  doAssert t.kind == tyArray, $t.kind
  result = 1+(t[0].n[1].intVal - t[0].n[0].intVal).int

proc calcFieldOffset(obj: PType, field:PSym) =
  # Get the offset by adding typ.size until the field in objType == `field`
  # The zero is at the start of the object.

  if field.offset>=0 : return # field has been calculated already
  field.offset = 0

  assert(not obj.isnil, "#1 nil objtype")
  let objType = obj.skipTypesOrNil({tyRef, tyPtr, tyVar, tyGenericInst})
  
  assert( objtype.kind == tyObject, $objtype.kind & "\n" & $typeToYaml(objType))
  for objf in objType.n:
    if objf.sym == field: break
    field.offset += objf.typ.size.int

proc location(s:PSym):int32 = 
  echo "# crazyLoc ", s.name.s, " owner: ", s.owner.name.s
  if s.offset<0:
    internalError("uninitialized offset: " & s.name.s)
  result = s.offset.int32 

proc genIdentDefs(w: WasmGen, n: PNode) =
  # a var section stores its data in `data` or has an
  # initialization expr, so no node is returned.
  #echo treeToYaml n
  let
    s = n[namePos].sym
    typ = if n[1].typ != nil: n[1].typ.skipTypes({tyGenericInst}) else: n[2].typ.skipTypes({tyGenericInst})
  
  s.offset = w.nextMemIdx # initialize the position in memory of the symbol
  
  assert(typ.kind in ConcreteTypes, $typ.kind)
  var dataseg: seq[byte] = newSeq[byte]()
  case n[2].kind:
  of nkEmpty:
    dataseg.setLen(typ.size)
  of nkInt64Lit, nkUInt64Lit:
    internalError("Wasm MVP integers are 32bit only")

  of nkCharLit, nkIntLit, nkInt8Lit,
    nkInt16Lit, nkInt32Lit:
    dataseg = n[2].intVal.int32.toBytes
  of nkUIntLit, nkUInt8Lit, nkUInt16Lit, nkUInt32Lit:
    dataseg = n[2].intVal.uint32.toBytes
  of nkFloatLit, nkFloat32Lit, nkFloat64Lit, nkFloat128Lit:
    dataseg = n[2].floatVal.toBytes
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    internalError("TODO: genIdentDefs: string literals")
  of nkNilLit:
    debug s
  of nkExprColonExpr, nkInfix, nkBracket, nkCall:
    internalError("TODO: genIdentDefs: initialize")
  else:
    internalError("genIdentDefs: unhandled identdef kind: " & $n[2].kind)

  w.m.data.add(newData(w.nextMemIdx, dataseg))

  w.nextMemIdx = ( w.nextMemIdx + dataseg.len ).alignTo4

proc genBody(w: WasmGen,params:Table[string,tuple[vt:WasmValueType,default:PNode]], n: PNode): WasmNode =
  result = newWANode(opList)
  var res: WasmNode
  case n.kind:
  of nkAsgn:
    let
      lhs = n[0]
      rhs = n[1]
    if lhs.sym.kind == skResult:
      if rhs.sym.name.s in params:
        res = newGet(woGetLocal,rhs.sym.position)
      else:
        res = w.gen(rhs)
  else:
    echo "# genBody ", $n.kind
    echo treeToYaml n
    result.sons.add(w.gen(n))
  if not res.isNil:
    result.sons.add(res)# the last value on the stack is the return value

proc genProc(w: WasmGen, s: PSym) =
  let 
    fparams = s.typ.n
    pragmas = s.ast[4]
    body = s.getBody()

  var 
    params = initTable[string,tuple[vt:WasmValueType,default:PNode]]()
    res = mapType(fparams[0].typ)
  
  #echo "params....", treeToYaml fparams

  for par in fparams.sons[1..^1]:
    if par.kind == nkIdentDefs:
      #echo treeToYaml par
      if par.len > 3:
        for i in 0..<par.len-2: # eg a,b: int
          params.add(
            par[i].ident.s, 
            (mapType(par[^2].typ), if par[^1].kind!=nkEmpty: par[^1] else: nil ) # FIXME: detect type of param properly
          )
      else:
        var 
          defaultVal : PNode
          typ: PType = par[1].typ
        if par[2].kind != nkEmpty:
          if par[2].kind == nkDotExpr:
            defaultVal = par[2][0]
            typ = defaultVal.typ
        params.add( # a: int
          par[0].ident.s, 
          (mapType(typ), defaultVal) # FIXME: detect type of param properly
        )
    elif par.kind == nkSym:
      params.add( # a: int
        par.sym.name.s, 
        (mapType(par.sym.typ), par.sym.ast) # FIXME: detect type of param properly
      )
    elif par.kind == nkEmpty: continue
    else:
      internalError("# unknown putProc par kind: " & $par.kind)
      #echo treeToYaml par
    
  var
    fntype = newType(res)

  for name,val in params:
    if not val.default.isNil:
      echo "# TODO: need to init result"
    fntype.params.add(val.vt)
  
  if s.flags.contains(sfImportc):
    # it's an imported proc
    # assume it has a header too
    let
      headerPrg = pragmas.getPragmaStmt(wHeader)
      importcPrg = pragmas.getPragmaStmt(wImportc)
    if headerPrg.isNil: 
      #echo treeToYaml s.ast
      internalerror("putProc: missing header for imported proc "&s.name.s)
    elif importcPrg.isNil:
      internalerror("putProc: missing importc for imported proc "&s.name.s)
    
    let 
      headername = headerPrg[1].strVal
      importcname = importcPrg[1].strval
    
    w.m.imports.add(
      newImport(
        w.nextImportIdx, ekFunction, headername, importcname, fntype, s.flags.contains(sfExported)
      )
    )
    w.generatedProcs.add(s.mangleName, (w.nextImportIdx,true)) 
    inc w.nextImportIdx
  elif s.flags.contains(sfUsed):
    w.m.functions.add(
      newFunction(
        w.nextFuncIdx, fntype, w.genBody(params, body), nil, s.mangleName, s.flags.contains(sfExported)
      )
    )
    w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
    inc w.nextFuncIdx
  else:
    internalError("# genProc generating unused proc " & s.name.s)

proc genSymLoc(w: WasmGen, s: PSym): WasmNode =
  let t = s.typ.skipTypes(abstractVarRange)
  case t.kind:
  of tyString, tyRef, tyPtr, tyPointer:
    result = newLoad(memLoadI32, 0, 1, newConst(s.offset.int32))
  else:
    result = newConst(s.offset.int32)

proc gen(w: WasmGen, n: PNode): WasmNode =
  echo "# gen ", $n.kind
  case n.kind:
  of nkNilLit:
    result = newConst(0'i32)
  of nkIntLit, nkInt32Lit, nkCharLit:
    result = newConst(n.intVal.int32)
  of nkFloat32Lit:
    result = newConst(n.floatVal.float32)
  of nkFloatLit, nkFloat64Lit:
    result = newConst(n.floatVal.float64)
  of nkCommentStmt, nkTypeSection, nkProcDef: discard
  of nkCallKinds:
    echo "# genCall", treeToYaml n
    let s = n[namePos].sym
    if not w.generatedProcs.hasKey(s.mangleName):
      w.genProc(s)

    var args = newSeq[WasmNode]()
    for i in 1..<n.sons.len:
      args.add(gen(w,n[i]))
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx, args, isImport)
  of nkSym:
    result = newLoad(memLoadI32, 0, 1, w.genSymLoc(n.sym))
  of nkStmtList:
    result = newOpList()
    for stm in n:
      let gn = w.gen(stm)
      if not gn.isNil: result.sons.add(gn)
  of nkVarSection:
    echo "# genVarSection" #TODO
    for iddef in n.sons:
      w.genIdentDefs(iddef)
  else:
    #echo $n.kind
    internalError("missing gen case: " & $n.kind)
#-------------putHeapPtr-------------------------------#
proc putHeapPtr(w:WasmGen) =
  w.m.data.add(
    newData(
      heapPtrLoc, w.nextMemIdx.toBytes
    )
  )
#------------------putInitFunc-------------------------#
proc putInitFunc(w: WasmGen) =
  # Generate the init expression
  if w.initExprs.len<1: return # no expression, no need for a init proc
  w.m.functions.add(
    newFunction(
      w.nextFuncIdx, newType(vtNone),  newOpList(w.initExprs), nil, "init", true
    )
  )
  echo "init :", w.nextFuncIdx
  inc w.nextFuncIdx
#-------------linker-----------------------------------#
proc linkPass(mainModuleFile:string, w:WasmGen) =
  echo "linking..."
  #TODO: allow user defined glue
  if optRun in gGlobalOptions:
    # generate a js file suitable to be run by node
    writeFile(mainModuleFile.changeFileExt("js"), generateNodeLoader(w.s.name.s))
  else:
    writefile(mainModuleFile.changeFileExt("html"), generateLoader(w.s.name.s))

#------------------myPass------------------------------#
proc myProcess(b: PPassContext, n: PNode): PNode =
  #echo "processing ", $n.kind, " from ", n.info.fileIndex.toFilename
  if passes.skipCodegen(n): return n

  var w = WasmGen(b)
  let generated = w.gen(n)
  if not generated.isNil: w.initExprs.add(generated)
  result = n

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  echo "# closing ", $n.kind, " from ", n.info.fileIndex.toFilename
  if passes.skipCodegen(n): return n
  result = myProcess(b, n)
  var w = WasmGen(b)
  #w.putInitFunc
  if w.s.flags.contains(sfMainModule):
    w.putInitFunc
    w.putHeapPtr
    let ext = "wasm"
    let f = w.s.info.fileIndex.toFilename
    let outfile =
      if options.outFile.len > 0:
        if options.outFile.isAbsolute: options.outFile
        else: getCurrentDir() / options.outFile
      else:
        changeFileExt(completeCFilePath(f), ext)
    encode(w.m).writeTo(outfile)
    linkPass(outfile, w)  
    writeFile(outfile.changeFileExt("json"), render(w.m))
    

proc myOpenCached(graph: ModuleGraph; s: PSym, rd: PRodReader): PPassContext =
  # TODO: is this even true?
  internalError("symbol files are not possible with the WASM code generator")
  result = nil

proc myOpen(graph: ModuleGraph; s: PSym; cache: IdentCache): PPassContext =
  echo "# begin myOpen ",s.info.fileIndex.toFilename," s.name: ",$s.name.s
  if genCtx.isNil: genCtx = newWasmGen(s)
  genCtx.s = s
  result = genCtx
  echo "# end myOpen ",s.info.fileIndex.toFilename," s.name: ",$s.name.s

const WasmGenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)
