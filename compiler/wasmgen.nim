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
proc callMagic(w: WasmGen, s: PSym, n: PNode): WasmNode

proc location(s:PSym):int32 = 
  #echo "# crazyLoc ", s.name.s, " owner: ", s.owner.name.s
  if s.offset<0:
    internalError("uninitialized offset: " & s.name.s)
  result = s.offset.int32 

proc genSymLoc(w: WasmGen, s: PSym): WasmNode =
  #let t = s.typ.skipTypes(abstractVarRange)
  #case t.kind:
  #of tyString, tyRef, tyPtr, tyPointer:
  #  result = newLoad(memLoadI32, 0, 1, newConst(s.offset.int32))
  #else:
  result = newConst(s.location)

proc store(w: WasmGen, s: PSym, typ: PType, n: PNode,  memIndex: var int) =
  # a var section stores its data in `data` or has an
  # initialization expr, so no node is returned.
  #echo treeToYaml n
  var dataseg: seq[byte] = newSeq[byte]()
  case n.kind:
  of nkEmpty:
    # eg. var s: string
    # produces 4 nil bytes where the string ptr goes
    dataseg.setLen(typ.getSize)
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
    var tmpMemIdx = memIndex
    for i, colonexpr in n:
      if i == 0: continue # type symbol
      let 
        es = colonexpr[0].sym
        t = colonexpr[1].typ
      tmpMemIdx = memIndex+es.offset
      w.store(es, t, colonexpr[1], tmpMemIdx)
  of nkNilLit:
    debug s
  of nkBracket:
    for val in n:
      w.store(nil, val.typ, val, memIndex)
  of nkInfix, nkPrefix, nkCall:
    #echo treeToYaml n, n.typ.getSize.alignTo4
    dataseg.setLen(n.typ.getSize)
    if n[0].sym.magic != mNone:
      w.initExprs.add(w.callMagic(n[0].sym, n))
    else:
      w.initExprs.add( # FIXME: I'm here
        newStore(
          n.typ.mapStoreKind,
          w.gen(n),
          0'i32, newConst(memIndex)
        )
      )
  of nkExprColonExpr:
    internalError("TODO: genIdentDefs: initialize " & $n.kind)
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

proc genBody(w: WasmGen,params:Table[string,tuple[t: PType, vt:WasmValueType,default:PNode]], n: PNode): WasmNode =
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
    params = initTable[string,tuple[t: PType, vt:WasmValueType,default:PNode]]()
    res = mapType(fparams[0].typ)
    paramTypeList = newSeq[PType]()
  #echo "params....", treeToYaml fparams

  for par in fparams.sons[1..^1]:
    if par.kind == nkIdentDefs:
      #echo treeToYaml par
      if par.len > 3:
        for i in 0..<par.len-2: # eg a,b: int
          paramTypeList.add( par[^2].typ)
          params.add(
            par[i].ident.s, 
            (par[^2].typ, mapType(par[^2].typ), if par[^1].kind!=nkEmpty: par[^1] else: nil ) # FIXME: detect type of param properly
          )
      else:
        var 
          defaultVal : PNode
          typ: PType = par[1].typ
        if par[2].kind != nkEmpty:
          if par[2].kind == nkDotExpr:
            defaultVal = par[2][0]
            typ = defaultVal.typ
        paramTypeList.add(typ)
        params.add( # a: int
          par[0].ident.s, 
          (typ, mapType(typ), defaultVal) # FIXME: detect type of param properly
        )
    elif par.kind == nkSym:
      paramTypeList.add( par.sym.typ)
      params.add( # a: int
        par.sym.name.s, 
        (par.sym.typ, mapType(par.sym.typ), par.sym.ast) # FIXME: detect type of param properly
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
        w.nextImportIdx, ekFunction, headername, importcname, s.mangleName, fntype, s.flags.contains(sfExported)
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
  of mEqI, mEqEnum, mEqCh, mEqB,
    mEqRef, mEqStr: irEq32  
    # If addr strA == addr strB, they are the same string
  of mLtU: irLtU32
  of mLtI: irLtS32
  of mLeU: irLeU32
  of mLeI: irLeS32
  else: woNop
  if result == woNop:
    internalError("unmapped magic: " & $m)

const UnaryMagic = {mNot}    
const BinaryMagic = {mAddI,mAddU,mSubI,mSubU,mMulI,mMulU,mDivI,mDivU,
  mModI,mModU, mAnd, mOr, mXor, mShlI, mShrI, mLtU, mLtI, mLeU, mLeI,
  mEqI, mEqF64, mEqEnum, mEqCh, mEqB, mEqRef, mEqStr}

proc callMagic(w: WasmGen, s: PSym, n: PNode): WasmNode = 
  case s.magic:
  of UnaryMagic:
    return newUnaryOp(getMagicOp(s.magic), w.gen(n[1]))
  of BinaryMagic:
    return newBinaryOp(getMagicOp(s.magic), w.gen(n[1]), w.gen(n[2]))
  of mNew:
    if w.generatedProcs.hasKey(s.mangleName):
      return newCall(w.generatedProcs[s.mangleName].id, w.genSymLoc(n[1].sym), false)

    # new gets passed a ref, so local 0 is the location of the ref to the
    # object to initialize.
    #echo treeToYaml n[1]
    let size = n[1].typ.lastSon.getSize
    var magicbody = newOpList()
    magicbody.sons.add([
      # heap ptr points to next free byte in memory. Use it to
      # move the pointed-to location of the ref to somewhere free
      newStore(
        memStoreI32, newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc)), 
        0, newGet(woGetLocal, 0)
      ),
      # move heap ptr by `size` bytes.
      # the assumption is that everything after heap ptr is free to take
      newStore(
        memStoreI32, newBinaryOp(
          ibAdd32, newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc)), newConst(size.int32)
        ),  
        0, newConst(heapPtrLoc)
      )
    ])
    
    w.m.functions.add(
      newFunction(
        w.nextFuncIdx, newType(vtNone, vtI32), magicbody, nil, s.mangleName,
        s.flags.contains(sfExported)
      )
    )
    result = newCall(w.nextFuncIdx, w.genSymLoc(n[1].sym), false)
    w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
    inc w.nextFuncIdx
  of mReset:
    if w.generatedProcs.hasKey(s.mangleName):
      return newCall(w.generatedProcs[s.mangleName].id, w.genSymLoc(n[1].sym), false)
    var loopBody = newWANode(opList)

    loopBody.sons.add(
      newStore(
        memStoreI32,
        newConst(0'i32),
        0'i32,
        newGet(woGetLocal, 1)
      )
    )
    loopBody.sons.add(
      newSet(
        woSetLocal, 1'i32,
        newBinaryOp(
          ibAdd32,
          newGet(woGetLocal, 1),
          # FUTURE FIXME: when some types have size <4, this needs to be reworked too
          newConst(4'i32)
        )
      )
    )

    var magicbody = newWhileLoop(
          newBinaryOp(
            irLtU32,
            newGet(woGetLocal, 1),
            newBinaryOp(
              ibAdd32,
              newGet(woGetLocal, 0),
              newConst(n[0].typ.getSize.int32)
            )
          ),
          loopBody
        )
        
    w.m.functions.add(
      newFunction(
        w.nextFuncIdx, newType(vtNone, vtI32), magicbody, @[vtI32], s.mangleName,
        s.flags.contains(sfExported)
      )
    )
    
    result = newCall(w.nextFuncIdx, w.genSymLoc(n[1].sym), false)
    
    w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
    inc w.nextFuncIdx

  of mDotDot:
    let t = n[1].typ.skipTypes(abstractVarRange)
    if n.sonsLen > 2:
      result = newOpList(
        newStore(
          t.mapStoreKind,
          w.gen(n[1]), 0'i32, newConst(w.nextMemIdx.int32)
        ),
        newStore(
          t.mapStoreKind,
          w.gen(n[2]), 0'i32, newConst((w.nextMemIdx+t.getSize).int32)
        )
      )
    else:
      result = newOpList(
        newStore(
          t.mapStoreKind,
          w.gen(n[1]), 0'i32, newConst((w.nextMemIdx+t.getSize).int32)
        )
      )
  else: 
    internalError("# callMagic unhandled magic: " & $s.magic)

proc genAsgn(w: WasmGen, lhsNode, rhsNode: PNode): WasmNode =
  var 
    lhsIndex: WasmNode
    lhsOffset = 0'i32 # TODO: a node?
  # let's try to enumerate cases (a = lhs, c = rhs):
  # - a.b = c 
  # - a[].b = c
  # - a = c
  # - a[b] = c
  # - strings, seq (they copy, so data(a) = data(c) but addr a != addr c)
  echo lhsNode.kind, " - ", lhsNode.typ.kind 
  case lhsNode.kind
  of nkDerefExpr: 
    # index is the location pointed to
    lhsIndex = newLoad(memLoadI32, 0, 1, w.genSymLoc(lhsNode[0].sym))
  of nkSym:
    lhsIndex = w.genSymLoc(lhsNode.sym)
  of nkDotExpr:
    if lhsNode[0].kind == nkSym:
      lhsIndex = w.genSymLoc(lhsNode[0].sym)
      lhsOffset = lhsNode[1].sym.offset.int32
    elif lhsNode[0].kind in {nkHiddenDeref, nkDerefExpr}:
      lhsIndex = newLoad(memLoadI32, 0, 1, w.genSymLoc(lhsNode[0][0].sym))
      lhsOffset = lhsNode[1].sym.offset.int32
    else:
      internalError("genAsgn nkDotExpr error")
  else: 
    internalError("unhandled lhs kind " & $lhsNode.kind)
  
  case lhsNode.typ.kind:
  of tyInt,tyBool,tyInt32, tyEnum, tyChar:
    result = newStore(memStoreI32, w.gen(rhsNode), lhsOffset, lhsIndex)
  of tyRef:
    result = newStore(memStoreI32, w.gen(rhsNode), lhsOffset, lhsIndex) #w.gen(lhsNode))
  of tyFloat32:
    result = newStore(memStoreF32, w.gen(rhsNode), lhsOffset, lhsIndex)
  of tyFloat:
    var storeKind: WasmOpKind 
    if lhsNode.typ.size == 4:
      storeKind = memStoreF32
    elif lhsNode.typ.size == 8:
      storeKind = memStoreF64
    else:
      internalError("impossible float size" & $lhsNode.typ.size)
    result = newStore(storeKind, w.gen(rhsNode), lhsOffset, lhsIndex)
  of tyString:
    result = newStore(memStoreI32, w.gen(rhsNode), lhsOffset, lhsIndex) #w.gen(lhsNode))
  else:  
    internalError("# genAsgn missing case: " & $lhsNode.typ.kind)

proc accessLocAux(w: WasmGen, n: PNode): WasmNode =
  case n.kind
  of nkDerefExpr: 
    # index is the location pointed to
    result = newLoad(memLoadI32, 0, 1, w.genSymLoc(n[0].sym))
  of nkSym:
    result = w.genSymLoc(n.sym)
  of nkDotExpr:
    if n[0].kind == nkSym:
      result = newBinaryOp(
        ibAdd32,
        w.genSymLoc(n[0].sym),
        newConst(n[1].sym.offset.int32)
      ) 
    elif n[0].kind in {nkHiddenDeref, nkDerefExpr}:
      result = newBinaryOp(
        ibAdd32,
        newLoad(memLoadI32, 0, 1, w.genSymLoc(n[0][0].sym)),
        newConst(n[1].sym.offset.int32)
      )
    else:
      internalError("accessLocAux nkDotExpr error")
  else: 
    internalError("unhandled accessLocAux kind " & $n.kind)
  
proc genAccess(w: WasmGen, n: PNode): WasmNode =
  let 
    symIndex = w.accessLocAux(n[0])
    accPos = w.gen(n[1])
    t = n.typ
    accIndex = newBinaryOp(
      ibAdd32, 
      symIndex, 
      newBinaryOp(
        ibMul32,
        accPos, newConst(t.getSize().alignTo4.int32)
      )
    )
  echo "genaccess", mapType(t)
  case mapType(t):
  of vtI32: result = newLoad(memLoadI32, 0 , 1, accIndex)
  of vtF32: result = newLoad(memLoadF32, 0 , 1, accIndex)
  of vtF64: result = newLoad(memLoadF64, 0 , 1, accIndex)
  else:  
    internalError("# genAccess missing case: " & $t.kind)


proc gen(w: WasmGen, n: PNode): WasmNode =
  echo "# gen ", $n.kind
  case n.kind:
  of nkCommentStmt, nkTypeSection, nkProcDef, nkPragma, nkEmpty,
    nkTemplateDef, nkMacroDef: discard
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

    #[if n.kind == nkPrefix and isMagic:
      return newUnaryOp(getMagicOp(s.magic), w.gen(n[1]))
    elif n.kind == nkInfix and isMagic:
      return newBinaryOp(getMagicOp(s.magic), w.gen(n[1]), w.gen(n[2]))
    elif isMagic:
      return w.callMagic(s, n)]#
    if isMagic:
      return w.callMagic(s, n)
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
    #[if n.sym.typ.kind == tyRef:
      # tyRef generates the location of the pointer, not the location of
      # the data pointed to. This is to allow to do move the location pointed to
      # inside the procs, useful eg. for `new`
      echo "tyRef"
      result = w.genSymLoc(n.sym)
    else:]#
    result = newLoad(loadKind, 0, 1, w.genSymLoc(n.sym))
    # FIXME: max uint value is bound by max int value
    # becuase wasm mvp is 32bit only
  of nkBracketExpr:
    echo treeToYaml n
    result = w.genAccess(n)
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
    #echo treeToYaml n
    var loadKind : WasmOpKind = memLoadI32
    if n.typ.kind in {tyFloat,tyFloat64}:
      loadKind = memLoadF64
    elif n.typ.kind == tyFloat32:
      loadKind = memLoadF32
    result = newLoad(loadKind, 0, 1, 
      newLoad(memLoadI32, 0, 1, w.genSymLoc(n[0].sym))
    )
  of nkDotExpr:
    # a.b
    var loadKind : WasmOpKind = memLoadI32
    if n.typ.kind in {tyFloat,tyFloat64}:
      loadKind = memLoadF64
    elif n.typ.kind == tyFloat32:
      loadKind = memLoadF32
    if n[0].kind == nkSym:
      result = newLoad(loadKind, 0, 1, 
        newBinaryOp(ibAdd32, w.genSymLoc(n[0].sym), newConst(n[1].sym.offset))
      )
    elif n[0].kind in {nkDerefExpr, nkHiddenDeref}:
      result = newLoad(loadKind, 0, 1, 
        newBinaryOp(ibAdd32, newLoad(memLoadI32, 0, 1, w.genSymLoc(n[0][0].sym)), newConst(n[1].sym.offset))
      )
    else:
      internalError("nkDotExpr n[0] kind: " & $n[0].kind) 
  of nkAsgn:
    result = w.genAsgn(n[0], n[1])
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
      w.nextFuncIdx, newType(vtNone),  newOpList(w.initExprs), nil, "nimInit", true
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
