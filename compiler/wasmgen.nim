import
  ../Nim/compiler/[ast, astalgo, options, msgs, magicsys, idents, types, passes, rodread,
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
    generatedTypeInfos: initTable[string,int32](),
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
    # types that get passed around by the location of their 
    # pointer (NOT the pointed-to value) or first byte of the value
  heapPtrLoc = 4'i32
  wasmPtrSize = 4 # FIXME

var genCtx : WasmGen  # The global context. This is global because we merge
                      # all modules in a single wasm file and I have no idea
                      # how to this in other ways

proc gen(w: WasmGen, n: PNode):WasmNode
proc callMagic(w: WasmGen, s: PSym, n: PNode): WasmNode

#[
proc initLoc(result: var TLoc, k: TLocKind, lode: PNode, s: TStorageLoc) =
  result.k = k
  result.storage = s
  result.lode = lode
  result.p = -1
  result.flags = {}

proc fillLoc(a: var TLoc, k: TLocKind, lode: PNode, p: BiggestInt, s: TStorageLoc) =
  # fills the loc if it is not already initialized
  if a.k == locNone:
    a.k = k
    a.lode = lode
    a.storage = s
    if a.p == -1: a.p = p
]#
proc genSymLoc(w: WasmGen, s: PSym): WasmNode =
  echo "sym ", s.name.s," owner ", s.owner.name.s
  echo "pos ", s.position, " offset ", s.offset
  debug s #FIXME: s is not b when instatiating contains
  case s.kind:
  of skParam:
    echo "experiment: genSymParam"
    result = newGet(woGetLocal, s.position)
  else:
    assert s.offset >= 0, "uninitialized location for: " & s.name.s & " from " & s.owner.name.s
    result = newConst(s.offset)
  if s.offset<0: echo "something wrong above"

proc store(w: WasmGen, typ: PType, n: PNode,  memIndex: var int): WasmNode =
  # a var section stores its data in `data` or has an
  # initialization expr, so no node is returned.
  #echo treeToYaml n
  var dataseg: seq[byte] = newSeq[byte]()
  echo "# store ", n.kind #, " owner: ", $owner.name.s
  result = newOpList()
  
  case n.kind:
  of nkEmpty:
    # eg. var s: string
    # produces 4 nil bytes where the string ptr goes
    dataseg.setLen(typ.getSize.alignTo4)
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
    if n.strVal.len > 0:
      dataseg = (memIndex+wasmPtrSize).uint32.toBytes & n.strVal.len.uint32.toBytes & n.strVal.toBytes
    else:
      dataseg = (memIndex+wasmPtrSize).uint32.toBytes & n.strVal.len.uint32.toBytes
  of nkObjConstr:
    var tmpMemIdx = memIndex
    for i, colonexpr in n:
      if i == 0: continue # type symbol
      let 
        es = colonexpr[0].sym
        t = colonexpr[1].typ
      tmpMemIdx = memIndex+es.offset
      let gn = w.store(t, colonexpr[1], tmpMemIdx)
      if not gn.isNil: result.sons.add(gn)
  of nkNilLit:
    dataseg.setLen(n.typ.getSize.alignTo4)
  of nkBracket:
    for val in n:
      let gn = w.store(val.typ, val, memIndex)
      if not gn.isNil: result.sons.add(gn)
  of nkCurly:
    echo memIndex
    # this is a set. This means each val in n is either a (u)int8 or (u)int16
    # and thus val <= 65535 (max of uint16)
    dataseg.setLen(4)
    for val in n:
      dataseg[val.intVal.int32 div 32] = dataseg[val.intVal.int32 div 32] or (1 shl (val.intVal mod 32)).uint8
  of nkAddr:
    dataseg.setLen(wasmPtrSize)
    result = newStore(
      memStoreI32,
      w.genSymLoc(n[0].sym),
      0'i32, newConst(memIndex)
    )
  of nkSym:
    result = # FIXME: does this work for every symbol?
      newStore(
        n.typ.mapStoreKind,
        w.gen(n),
        0'i32, newConst(memIndex)
      )
  of nkCallKinds:
    # echo treeToYaml n, n.typ.getSize.alignTo4
    dataseg.setLen(n.typ.getSize.alignTo4)    
    result = # TODO: mmh
      newStore(
        n.typ.mapStoreKind,
        w.gen(n),
        0'i32, newConst(memIndex)
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

proc genBody(w: WasmGen,
  #params:Table[string,tuple[t: PType, vt:WasmValueType,default:PNode]],
  params: openArray[WasmValueType],
  n: PNode,
  resType: PType): WasmNode =
  #echo treeToYaml n
  var 
    explicitRet = n.kind == nkReturnStmt # wheter there was an explicit return statement
  result = newOpList()
  if resType.kind in NilableTypes+ConstantDataTypes:
    # This is wrong, as 
    # init the result local to a free space in memory
    result.sons.add(newSet(woSetLocal, params.len, newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc))))
  if n.kind == nkStmtList:
    for st in n:
      echo "# genBody ", $st.kind
      echo treetoyaml st
      explicitRet = st.kind == nkReturnStmt
      let gs = w.gen(st)
      if not gs.isNil: result.sons.add(gs)
  elif n.kind == nkReturnStmt and n[0].typ.kind == tyStmt: 
    # The child of the result stmt doesn't return anything in wasm terms.
    # So we generate the child and force the return of result.
    # eg nkReturn[nkAsgn], for wasm nkAsgn doesn't generate a return,
    # so it would generate return[void]
    explicitRet = false
    result.sons.add(w.gen(n[0]))
  else:
    # single stmt 
    result.sons.add(w.gen(n))
    #internalError("# genbody parent node kind: " & $n.kind)
  if not explicitRet and result != nil:
    # this is because sometime the result is appended implicitly, but
    # we make it explicit for the sake of wasm?
    result.sons.add(
      newReturn(newGet(woGetLocal, params.len))
    )
proc genProc(w: WasmGen, s: PSym) =
  let 
    fparams = s.typ.n
    pragmas = s.ast[pragmasPos]
    body = s.getBody()
  
  var 
    params = newSeq[WasmValueType](fparams.sons.len-1)#initTable[string,tuple[t: PType, vt:WasmValueType,default:PNode]]()
    res = mapType(s.typ.sons[0])
  
  for i, p in params.mpairs:
    let par = fparams[i+1]
    if par.kind == nkIdentDefs:
      if par.len > 3:
        for i in 0..<par.len-2: # eg a,b: int
          p = mapType(par[^2].typ)
      else:
        var typ: PType = par[1].typ
        if par[2].kind != nkEmpty:
          if par[2].kind == nkDotExpr:
            typ = par[2][0].typ
        p = mapType(typ)
    elif par.kind == nkSym:
      p = mapType(par.sym.typ)
    elif par.kind == nkEmpty: continue
    else:
      internalError("# unknown putProc par kind: " & $par.kind)
  
  if not s.typ.sons[0].isNil:
    # since wasm allows shadowing of params with local vars, this
    # tries to make sure result gets its very own var after all
    # params have theirs. Also make the symbol remember it so
    # we don't need a map from result to its position
    assert s.ast[resultPos].sym.kind == skResult
    s.ast[resultPos].sym.position = params.len
  
  
  var
    fntype = newType(rs=res)
  #echo "res: ", typeToYaml s.typ.sons[0]
  for val in params:
    fntype.params.add(val)
  
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
        w.nextFuncIdx, fntype, w.genBody(params, body, s.typ.sons[0]),
        if fntype.res != vtNone: @[fntype.res] else: nil, 
        s.mangleName, s.flags.contains(sfExported)
      )
    )
    w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
    inc w.nextFuncIdx
  else:
    internalError("# genProc generating unused proc " & s.name.s)

proc getMagicOp(m: TMagic): WasmOpKind =
  result = case m:
  of mAddI, mAddU, mSucc: ibAdd32
  of mSubI, mSubU, mPred: ibSub32
  of mMulI, mMulU: ibMul32
  of mDivI: ibDivS32
  of mDivU: ibDivU32
  of mModI: ibRemS32
  of mModU: ibRemU32
  of mAnd, mBitandI: ibAnd32
  of mOr, mBitorI: ibOr32
  of mXor, mBitxorI: ibXor32
  of mShlI: ibShl32
  of mShrI: ibShrS32
  of mNot: itEqz32
  of mEqI, mEqEnum, mEqCh, mEqB,
    mEqRef, mEqStr, mEqSet: irEq32  
    # If addr strA == addr strB, they are the same string
  of mLtU: irLtU32
  of mLtI, mLtEnum, mLtStr, mLtCh, mLtSet, mLtB, mLtPtr: irLtS32
  of mLeU: irLeU32
  of mLeI, mLeEnum, mLeStr, mLeCh, mLeSet, mLeB, mLePtr: irLeS32
  of mEqF64: frEq64
  of mLeF64: frLe64
  of mLtF64: frLt64
  of mAddF64: fbAdd64
  of mSubF64: fbSub64
  of mMulF64: fbMul64
  of mDivF64: fbDiv64
  else: woNop
  if result == woNop:
    internalError("unmapped magic: " & $m)

proc getFloat32Magic(m:TMagic):WasmOpKind =
  result = case m:
  of mEqF64: frEq32
  of mLeF64: frLe32
  of mLtF64: frLt32
  of mAddF64: fbAdd32
  of mSubF64: fbSub32
  of mMulF64: fbMul32
  of mDivF64: fbDiv32
  else: woNop
  if result == woNop: internalError("unmapped magic2: " & $m) 

const UnaryMagic = {mNot}    
const BinaryMagic = {mAddI,mAddU,mSubI,mSubU,mMulI,mMulU,mDivI,mDivU,mSucc,mPred,
  mModI,mModU, mAnd, mOr, mXor, mShlI, mShrI, mLtU, mLeU,
  mEqI, mEqEnum, mEqCh, mEqB, mEqRef, mEqStr, mEqSet,
  mLtI, mLtEnum, mLtStr, mLtCh, mLtSet, mLtB, mLtPtr,
  mLeI, mLeEnum, mLeStr, mLeCh, mLeSet, mLeB, mLePtr,
  mBitandI, mBitorI, mBitxorI}
const FloatsMagic = {mEqF64, mLeF64, mLtF64, mAddF64, mSubF64, mMulF64, mDivF64}

proc callMagic(w: WasmGen, s: PSym, n: PNode): WasmNode = 
  #echo $s.magic, treeToYaml n
  case s.magic:
  of UnaryMagic:
    return newUnaryOp(getMagicOp(s.magic), w.gen(n[1]))
  of BinaryMagic:
    return newBinaryOp(getMagicOp(s.magic), w.gen(n[1]), w.gen(n[2]))
  of FloatsMagic:
    if n.typ.kind == tyFloat32 or 
      (n.typ.kind == tyBool and n[1].typ.kind == tyFloat32): 
      # the bool part is because otherwise 1'f32+2.0 would use f32 arithm
      return newBinaryOp(getFloat32Magic(s.magic), w.gen(n[1]), w.gen(n[2]))
    else:
      return newBinaryOp(getMagicOp(s.magic), w.gen(n[1]), w.gen(n[2]))
  of mBitnotI:
    return newBinaryOp(ibXor32, w.gen(n[1]), newConst(-1.int32))
  of mNew:
    if w.generatedProcs.hasKey(s.mangleName):
      return newCall(w.generatedProcs[s.mangleName].id, w.genSymLoc(n[1].sym), false)

    # new gets passed a ref, so local 0 is the location of the ref to the
    # object to initialize.
    #echo treeToYaml n[1]
    let size = n[1].typ.lastSon.getSize.alignTo4
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
              newConst(n[0].typ.getSize.alignTo4.int32)
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
          w.gen(n[2]), 0'i32, newConst((w.nextMemIdx+t.getSize.alignTo4).int32)
        ),
        newLoad(t.mapLoadKind, 0, 1, newConst(w.nextMemIdx.int32))  # FIXME: this shouldn't be necessary
                                                                    # basically, load and store in itself       
      )
    else:
      result = newOpList(
        newStore(
          t.mapStoreKind,
          w.gen(n[1]), 0'i32, newConst((w.nextMemIdx+t.getSize.alignTo4).int32)
        ),
        newLoad(t.mapLoadKind, 0, 1, newConst(w.nextMemIdx.int32))
      )
  of mSizeOf:
    result = newConst(n[1].typ.getSize.alignTo4.int32)
  of mInc, mDec:
    result = newOpList(
      newStore(
        memStoreI32, 
        newBinaryOp(
          if s.magic == mInc: ibAdd32 else: ibSub32,
          w.gen(n[1]), w.gen(n[2])
        ), 0, 
        w.genSymLoc(n[1].sym)        
      )     
    )
  of mNewSeq:
    #echo "# mNewSeq"
    #echo treeToYaml n
    # we receive the index to a ptr.We then need to reserve a block of memory for the
    # len+data of the seq. Since a default len is always know, we can store it.
    # remember to return the pointer you initially got
    if not w.generatedProcs.hasKey(s.mangleName):      
      var magicbody = newOpList(
        newStore( # store len at pointed to
          memStoreI32,
          newGet(woGetLocal, 1),
          0, # offset
          newGet(woGetLocal, 0)
        ),
        newStore( # move heap ptr
          memStoreI32,
          newAdd32(
            newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc.int32)),
            newAdd32(
              newConst(wasmPtrSize.int32),
              newMul32(
                newConst(n[1].sym.typ.lastSon.getSize.alignTo4.int32),
                newGet(woGetLocal, 1)
              )
            )
          ),
          0, newConst(heapPtrLoc.int32)
        )
      )
      
      w.m.functions.add(
        newFunction(
          w.nextFuncIdx, newType(vtNone, vtI32, vtI32), magicbody, nil, s.mangleName,
          s.flags.contains(sfExported)
        )
      )
      w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
      inc w.nextFuncIdx
    
    # echo "n1: ",treeToYaml n[1]
    # I don't like special casing here.
    if n[1].kind == nkSym and n[1].sym.offset>0:
      result = newCall(w.generatedProcs[s.mangleName].id, 
        newLoad(memLOadI32, 0,1, w.genSymLoc(n[1].sym)), 
        w.gen(n[2]), false
      )
    else:
      result = newCall(w.generatedProcs[s.mangleName].id, 
        newLoad(memLOadI32, 0,1, newConst(heapPtrLoc)), # this is not really ideal, it works because we assume
                                                        # newseq(len):res and so result is at local #1
        w.gen(n[2]), false
      )
    
  of mNewSeqOfCap:
    echo "# mNewSeqOfCap"
    echo treeToYaml n
    internalError("# TODO: mNewSeqOfCap")
    # we receive the len of the block to reserve.
    # Since this proc is completely a magic, we can do everything here.
    # remember to return the pointer you initially got
    result = newOpList(
      newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc.int32)), # this is the returned loc
      newStore( # move heap ptr
        memStoreI32,
        newAdd32(
          newLoad(memLoadI32, 0, 1, newConst(heapPtrLoc.int32)),
          newAdd32(
            newConst(wasmPtrSize.int32),
            newMul32(
              newConst(n.typ.getSize.alignTo4.int32),
              w.gen(n[1])
            )
          )
        ),
        0, newConst(heapPtrLoc.int32)
      )
    )
  of mLengthSeq, mLengthStr:
    result = newLoad(memLoadI32, 0, 1, w.gen(n[1]))
  of mChr:
    result = w.gen(n[1][0]) # skip nkChckRange for now... FIXME:
  else: 
    echo treeToYaml n
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
  #echo treeToYaml lhsNode
  echo "# genAsgn ", lhsNode.kind #, " - ", lhsNode.typ.kind 
  case lhsNode.kind
  of nkDerefExpr: 
    # index is the location pointed to
    lhsIndex = newLoad(memLoadI32, 0, 1, w.genSymLoc(lhsNode[0].sym))
  of nkSym:
    if lhsNode.sym.kind in {skParam, skResult}:
      #echo "rhsn",treeToYaml rhsNode
      return newSet(woSetLocal, lhsNode.sym.position, w.gen(rhsNode))
    else:
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
  of nkBracketExpr:
   # echo treeToYaml lhsNode
    if lhsNode[0].kind == nkSym:
      if lhsNode[0].sym.typ.kind in {tySequence, tyString}:
        lhsIndex = newLoad(memLoadI32, 0, 1, w.genSymLoc(lhsNode[0].sym))
      else:
        lhsIndex = w.genSymLoc(lhsNode[0].sym)
    elif lhsNode[0].kind in {nkHiddenDeref, nkDerefExpr}:
      lhsIndex = newLoad(memLoadI32, 0, 1, w.genSymLoc(lhsNode[0][0].sym))
    else:
      internalError("genAsgn nkBracketExpr 0 error")

    lhsIndex = newBinaryOp(
      ibAdd32, 
      lhsIndex, 
      newBinaryOp(
        ibMul32, w.gen(lhsNode[1]), newConst(lhsNode.typ.getSize().alignTo4)
      )
    )
  else: 
    internalError("# genAsgn unhandled lhs kind " & $lhsNode.kind)
  
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
  var accIndex = newBinaryOp(
    ibAdd32, 
    symIndex, 
    newBinaryOp(
      ibMul32,
      accPos, newConst(t.getSize().alignTo4.int32)
    )
  )
  #if not t.isNil and not t.lastSon.isNil and t.lastSon.kind in {tySequence, tyString}:
  #  accIndex = newAdd32(accIndex, newConst(4'i32)) # due to len taking 4 bytes
  echo "genaccess ", t.kind, " ", mapType(t)
  case mapType(t):
  of vtI32: result = newLoad(memLoadI32, 0 , 1, accIndex)
  of vtF32: result = newLoad(memLoadF32, 0 , 1, accIndex)
  of vtF64: result = newLoad(memLoadF64, 0 , 1, accIndex)
  else:  
    internalError("# genAccess missing case: " & $t.kind)


proc gen(w: WasmGen, n: PNode): WasmNode =
  when defined excessive: echo "# gen ", $n.kind
  case n.kind:
  of nkCommentStmt, nkTypeSection, nkPragma, nkEmpty,
    nkTemplateDef, nkProcDef, nkMacroDef, nkIncludeStmt: discard
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
      
    if isMagic:
      echo "ismagic ", $s.magic
  
      return w.callMagic(s, n)
    elif not w.generatedProcs.hasKey(s.mangleName):
      w.genProc(s)
    var args = newSeq[WasmNode]()
    for i in 1..<n.sons.len:
      #echo "gencall ",i,treeToYaml n
      args.add(gen(w,n[i]))
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx, args, isImport)
  of nkSym:
    case n.sym.kind:
    of skParam:
      result = newGet(woGetLocal, n.sym.position)
    of skEnumField:
      result = newConst(n.sym.position.int32)
    of skResult:
      #initialize the result sym
      debug n.sym
      echo n.sym.offset
      # todo: n.sym.offset??
      # madness ensues
      echo "get result at pos: ", n.sym.position
      # result already initialized in local
      # this won't work...
      result = newGet(woGetLocal, n.sym.position)
    else:
      echo "gen sym ", n.sym.name.s," owner ", n.sym.owner.name.s
      echo "gen pos ", n.sym.position, " offset ", n.sym.offset
      debug n.sym
      echo "gen typ ", n.sym.typ.skipTypes(abstractInst).kind
      if n.sym.typ.skipTypes(abstractInst).kind in passedAsBackendPtr:
        result = w.genSymLoc(n.sym)
      else:
        result = newLoad(mapLoadKind(n.sym.typ), 0, 1, w.genSymLoc(n.sym))
    # FIXME: max uint value is bound by max int value
    # becuase wasm mvp is 32bit only
  of nkBracketExpr:
    #echo treeToYaml n
    # FIXME: seq, string have len at pos 0
    result = w.genAccess(n)
  of nkStmtList, nkStmtListExpr:
    result = newOpList()
    for stm in n:
      let gn = w.gen(stm)
      if not gn.isNil: result.sons.add(gn)
  of nkVarSection, nkLetSection, nkConstSection:
    #echo treeToYaml n
    echo "# genVarSection" #TODO
    result = newOpList()
    for iddef in n.sons:
      if iddef[namePos].kind == nkSym: # Top level symbols/sections
        let
          s = iddef[namePos].sym
          typ = if iddef[1].typ != nil: iddef[1].typ.skipTypes({tyGenericInst, tyTypeDesc}) else: iddef[2].typ.skipTypes({tyGenericInst})
        
        s.offset = w.nextMemIdx # initialize the position in memory of the symbol
        
        assert(typ.kind in ConcreteTypes, $typ.kind & "\n" & $treeToYaml(iddef))
        if s.owner.kind == skModule:
          w.initExprs.add(w.store(typ, iddef[2], w.nextMemIdx))
        elif s.owner.kind == skProc:
          result.sons.add(w.store(typ, iddef[2], w.nextMemIdx))
        else:
          internalError("# genIdentDefs error: " & $n[namePos].kind)
      else:
        internalError("# genIdentDefs loop error: " & $n[namePos].kind)
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
  of nkReturnStmt:
    if n[0].kind == nkEmpty:
      echo "FIXME: return getlocal 0 hardcoded"
      result = newReturn(newGet(woGetLocal, 0)) # return 0th local (I assume that's `result`)
    if n[0].kind == nkAsgn:
      result = newOpList( # FIXME: flimsy
        w.gen(n[0]),
        newReturn(newGet(woGetLocal, n[0][0].sym.position))
      )
    else:
      result = newReturn(w.gen(n[0]))
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
  of nkHiddenStdConv, nkConv:
    #echo "nkHiddenStdConv for " & $n.typ.kind
    var convOP: WasmOpKind
    case n[1].typ.mapType:
    of vtI32:
      case n.typ.mapType:
      of vtI32: convOp = woNop
      of vtF32: convOp = cvConvertF32S_I32
      of vtF64: convOp = cvConvertF64S_I32
      of vtI64: convOp = cvExtendI64S_I32
      else: internalError("#nkHiddenStdConv")
    of vtF32:
      case n.typ.mapType:
      of vtI32: convOp = cvTruncI32S_F32
      of vtF32: convOp = woNop
      of vtF64: convOp = cvPromoteF64_F32
      of vtI64: convOp = cvTruncI64S_F32
      else: internalError("#nkHiddenStdConv")
    of vtF64:
      case n.typ.mapType:
      of vtI32: convOp = cvTruncI32S_F64
      of vtF32: convOp = cvDemoteF32_F64
      of vtF64: convOp = woNop
      of vtI64: convOp = cvTruncI64S_F64
      else: internalError("#nkHiddenStdConv")
    of vtI64:
      case n.typ.mapType:
      of vtI32: convOp = cvWrapI32_I64
      of vtF32: convOp = cvConvertF32S_I64
      of vtF64: convOp = cvConvertF64S_I64
      of vtI64: convOp = woNop
      else: internalError("#nkHiddenStdConv")
    else: internalError("#nkHiddenStdConv")
    if convOp == woNop:
      result = w.gen(n[1]) 
    else:
      result = newUnaryOp(convOp, w.gen(n[1]))
  of nkBlockStmt:
    result = w.gen(n[1])
  of nkWhileStmt:
    result = newWhileLoop(w.gen(n[0]), w.gen(n[1]))
  of nkIfStmt:
    #echo "nkIfstmt",treeToYaml n
    # ifstmt are recursive for now
    result = newWaNode(woNop)
    
    for bidx in countdown(n.sonsLen-1,0):
      #result = gen else1
      #result2 = gen if1 else result1
      #result3 = gen if2 else result2
      if n[bidx].kind == nkElse:
        result = w.gen(n[bidx][0])
      else:
        result = newIfElse(w.gen(n[bidx][0]),w.gen(n[bidx][1]), result)
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
    linkPass(outfile, w)  
    writeFile(outfile.changeFileExt("json"), render(w.m))
    encode(w.m).writeTo(outfile)
    
    

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
