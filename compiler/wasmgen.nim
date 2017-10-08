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
  wasmPtrSize = 4 # FIXME
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
proc genSymLoc(w: WasmGen, s: PSym): WasmNode =
  let t = s.typ.skipTypes(abstractVarRange)
  #case t.kind:
  #of tyString, tyRef, tyPtr, tyPointer:
  #  result = newLoad(memLoadI32, 0, 1, newConst(s.offset.int32))
  #else:
  result = newConst(s.offset.int32)

proc store(w: WasmGen, s: PSym, typ: PType, n: PNode,  memIndex: var int) =
  # a var section stores its data in `data` or has an
  # initialization expr, so no node is returned.
  #echo treeToYaml n
  var dataseg: seq[byte] = newSeq[byte]()
  case n.kind:
  of nkEmpty:
    # eg. var s: string
    # produces 4 nil bytes where the string ptr goes
    dataseg.setLen(typ.size)
  of nkInt64Lit, nkUInt64Lit:
    internalError("Wasm MVP integers are 32bit only")
  of nkCharLit, nkIntLit, nkInt8Lit,
    nkInt16Lit, nkInt32Lit:
    if typ.kind in tyUInt..tyUint32:
      dataseg = n.intVal.uint32.toBytes
    else:
      dataseg = n.intVal.int32.toBytes
  of nkUIntLit, nkUInt8Lit, nkUInt16Lit, nkUInt32Lit:
    dataseg = n.intVal.uint32.toBytes
  of nkFloatLit, nkFloat64Lit: #, nkFloat128Lit:
    if typ.kind == tyFloat32:
      dataseg = n.floatVal.float32.toBytes
    else:
      dataseg = n.floatVal.float64.toBytes
  of nkFloat32Lit:
    dataseg = n.floatVal.float32.toBytes
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    # |ptr|len|strdata|
    # |4b |4b |?b     |
    #     ^ptr points here
    dataseg = (memIndex+wasmPtrSize).uint32.toBytes & n.strVal.len.uint32.toBytes & n.strVal.toBytes
  of nkAddr:
    dataseg.setLen(wasmPtrSize)
    w.initExprs.add(
      newStore(
        memStoreI32,
        w.genSymLoc(n[0].sym),
        0'i32, newConst(memIndex)
      )
    )
  of nkObjConstr:
    echo treeToYaml n
    echo typ.size
    #echo n[0].sym.name.s & " " & $n[0].sym.offset
    #dataseg.setLen(typ.size)
    var tmpMemIdx = memIndex
    for i, colonexpr in n:
      if i == 0: continue
      let es = colonexpr[0].sym
      let t = colonexpr[1].typ
      tmpMemIdx = memIndex+es.offset
      w.store(es, t, colonexpr[1], tmpMemIdx)
  of nkNilLit:
    debug s
  of nkExprColonExpr, nkInfix, nkBracket, nkCall:
    internalError("TODO: genIdentDefs: initialize")
  else:
    internalError("genIdentDefs: unhandled identdef kind: " & $n.kind)
  if not dataseg.isNil and dataseg.len > 0:
    w.m.data.add(newData(memIndex, dataseg))
    memIndex = ( memIndex + dataseg.len ).alignTo4
  else:
    memIndex = ( memIndex + typ.size ).alignTo4

proc genIdentDefs(w: WasmGen, n: PNode) =
  # a var section stores its data in `data` or has an
  # initialization expr, so no node is returned.
  #echo treeToYaml n
  let
    s = n[namePos].sym
    typ = if n[1].typ != nil: n[1].typ.skipTypes({tyGenericInst, tyTypeDesc}) else: n[2].typ.skipTypes({tyGenericInst})
  
  s.offset = w.nextMemIdx # initialize the position in memory of the symbol
  
  assert(typ.kind in ConcreteTypes, $typ.kind & "\n" & $treeToYaml(n))
  
  w.store(s, typ, n[2], w.nextMemIdx)

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

proc getMagicOp(m: TMagic): WasmOpKind =
  result = case m:
  of mAddI, mAddU: ibAdd32
  of mSubI, mSubU: ibSub32
  of mMulI, mMulU: ibMul32
  of mDivI: ibDivS32
  of mDivU: ibDivU32
  of mModI: ibRemS32
  of mModU: ibRemU32
  of mAnd: ibAnd32
  of mOr: ibOr32
  of mXor: ibXor32
  of mShlI: ibShl32
  of mShrI: ibShrS32
  of mNot: itEqz32
  of mEqI: irEq32  
  of mLtU: irLtU32
  of mLtI: irLtS32
  of mLeU: irLeU32
  of mLeI: irLeS32
  else: woNop

proc gen(w: WasmGen, n: PNode): WasmNode =
  echo "# gen ", $n.kind
  case n.kind:
  of nkCommentStmt, nkTypeSection, nkProcDef, nkPragma, nkEmpty: discard
  of nkNilLit:
    result = newConst(0'i32)
  of nkCharLit..nkInt32Lit:
    result = newConst(n.intVal.int32)
  of nkUIntLit..nkUInt32Lit:
    result = newConst(n.intVal.uint32)
  of nkFloat32Lit:
    result = newConst(n.floatVal.float32)
  of nkFloatLit, nkFloat64Lit:
    result = newConst(n.floatVal.float64)
  of nkCallKinds:
    echo "# genCall" #, treeToYaml n
    let 
      s = n[namePos].sym
      isMagic = s.magic != mNone

    if n.kind == nkPrefix and isMagic:
      return newUnaryOp(getMagicOp(n[0].sym.magic), w.gen(n[1]))
      
    elif n.kind == nkInfix and isMagic:
      return newBinaryOp(getMagicOp(n[0].sym.magic), w.gen(n[1]), w.gen(n[2]))
    elif not w.generatedProcs.hasKey(s.mangleName):
      w.genProc(s)
    var args = newSeq[WasmNode]()
    for i in 1..<n.sons.len:
      args.add(gen(w,n[i]))
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx, args, isImport)
  of nkSym:
    var loadKind : WasmOpKind = memLoadI32
    if n.sym.typ.kind in {tyFloat,tyFloat64}:
      loadKind = memLoadF64
    elif n.sym.typ.kind == tyFloat32:
      loadKind = memLoadF32
    result = newLoad(loadKind, 0, 1, w.genSymLoc(n.sym))
    # FIXME: max uint value is bound by max int value
    # becuase wasm mvp is 32bit only
  of nkStmtList:
    result = newOpList()
    for stm in n:
      let gn = w.gen(stm)
      if not gn.isNil: result.sons.add(gn)
  of nkVarSection, nkLetSection, nkConstSection:
    #echo treeToYaml n
    echo "# genVarSection" #TODO
    for iddef in n.sons:
      w.genIdentDefs(iddef)
  of nkDerefExpr, nkHiddenDeref:
    echo treeToYaml n
    var loadKind : WasmOpKind = memLoadI32
    if n.typ.kind in {tyFloat,tyFloat64}:
      loadKind = memLoadF64
    elif n.typ.kind == tyFloat32:
      loadKind = memLoadF32
    result = newLoad(loadKind, 0, 1, 
      newLoad(memLoadI32, 0, 1, w.genSymLoc(n[0].sym))
    )
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
  echo "init: ", w.nextFuncIdx
  inc w.nextFuncIdx
#-------------linker-----------------------------------#
proc linkPass(mainModuleFile:string, w:WasmGen) =
  echo "genloaders..."
  #TODO: allow user defined glue
  if optRun in gGlobalOptions:
    # generate a js file suitable to be run by node
    writeFile(mainModuleFile.changeFileExt("js"), generateNodeLoader(w.s.name.s))
    # TODO: removeme
    writefile(mainModuleFile.changeFileExt("html"), generateLoader(w.s.name.s))
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
