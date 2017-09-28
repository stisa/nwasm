import
  ../Nim/compiler/[ast, astalgo, options, msgs, idents, types, passes, rodread,
  ropes, wordrecg,extccomp],
  ospaths,tables, os, strutils,
  wautils, warender,watypes,wasecs,waenums,waencodes,wanodes,leb128

from math import ceil,log2

from ../Nim/compiler/modulegraphs import ModuleGraph

import typetraits # TEMPORARY

include ../lib/wasm/waglue.templ # TODO: check out how nimdoc does overloading of template

type
  WasmGen = ref object of TPassContext
    startExprs: seq[WasmNode] # sequence of start exprs. for use in `start` sec
    initExprs: seq[WasmNode] # sequence of initializer expressions. for use in start sec
    nextTotalFuncIdx: Natural # function index space ( doesn't account for hoisting of imported procs )
    nextFuncIdx: Natural # the function index space (only for non-imported funcs)
    nextGlobalIdx: Natural # the global index space
    nextMemIdx: Natural # the linear memory index space
    nextTableIdx: Natural # the table index space
    memOffset: int32
    s: PSym # symbol of the current module, taken from myOpen
    m : Module #current module
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
  result.nextTotalFuncIdx = 0
  result.nextGlobalIdx = 0
  result.nextMemIdx = 0
  result.nextTableIdx = 0
  # 4 byte aligned, reserve 8 bytes to store the stack pointer
  # This mean effective address start at 12?
  result.memOffset = 12'i32 
  result.initExprs = newSeq[WasmNode]()
  result.startExprs = newSeq[WasmNode]()
  result.m = newModule() #gen.modules[^1]
  #initialize the module's sections
  result.m.datas = newDataSec()
  result.m.exports = newExportSec()
  result.m.functions = newFnSec()
  result.m.types = newTypeSec()
  result.m.imports = newImportSec()
  result.m.memory = MemorySection( # FIXME:proper memory asgn
    entries: @[
      MemoryType( 
        limits: ResizableLimits(flags: 0, initial: 1 )
        )
      ]
  )
  result.m.exports = newExportSec(
    # Export the default memory
    newExportEntry( "$memory", ExternalKind.Memory, 0)
  )

proc nextImportIndex(w:WasmGen):int = waencodes.totalImports
proc incNextImportIndex(w:WasmGen) = inc waencodes.totalImports
#TODO:?  proc updateHeapPtr(w:WasmGen) 

proc getObjFieldOffset(obj: PSym, field:PSym): int

const 
  passedAsBackendPtr = {tyVar, tyObject}
  heapPtrLoc = 4'i32

proc process(w: WasmGen, n: PNode)
proc gen(w: WasmGen, n: PNode):WasmNode

#proc genExpr(w: WasmGen, n: PNode):Op

proc mangleName(s:PSym):string = 
  echo s.name.s, " " , s.kind #,"\n",typeToyaml s.typ
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

proc crazyLoc(s:PSym):int32 = 
  echo s.name.s, " owner: ", s.owner.name.s
  if s.offset<0:
    #echo typeToYaml s.owner.typ
    # TODO: check s.owner.typ is tyObject
    s.owner.typ.calcFieldOffset(s)
  doAssert(s.offset != -1, "uninitialized offset: " & s.name.s & " " & $result)
  result = s.offset.int32 
#--------------putVar-------------------------------#
proc store(w:WasmGen, n:PNode, s:PSym = nil)

proc storeInData(w:WasmGen,n:PNode,s:PSym = nil) =
  var val: bytes
  case n.kind:
  of nkEmpty,nkNilLit:
    if s.isNil or s.typ.kind == tyInt:
      val = "\0".repeat(4)
    elif s.typ.kind == tyArray:
      let siz = s.typ.getArrayLen * s.typ.lastSon.size
      #echo "siz",  siz
      val = "\0".repeat(siz)
    elif s.typ.kind == tyProc:
      #echo "tyProc storeInData\n",typeToYaml s.typ
      val = "\0".repeat(4)
    elif s.typ.kind == tyRef:
      val = "\0".repeat(4) #(w.memOffset+4).uint32.toBytes # assumes ILP32
      w.initExprs.add( # at init time store the memory loc in the ptr
        newStore(
          memStoreI32,
          newLoad(
            memLoadI32,
            heapPtrLoc, 1'i32,
            newConst(0'i32)
          ),
          w.memOffset, 
          newConst(0'i32)
        )
      )
    else:
      internalError("storeInData fails " & $s.typ.kind)
  of nkCharLit, nkIntLit, nkInt8Lit, nkInt16Lit, nkInt32Lit:
    val = n.intVal.int32.toBytes
    if val.len mod 2 != 0: val.setLen(val.len+1)
  of nkUIntLit, nkUInt8Lit, nkUInt16Lit, nkUInt32Lit:
    val = n.intVal.uint32.toBytes
    if val.len mod 2 != 0: val.setLen(val.len+1)
    #nkInt64Lit,nkUInt64Lit,
  of nkFloatLit:
    if n.typ.kind == tyFloat32:
      val = n.floatVal.float32.toBytes
      #echo "store nkFloatLit (32) len ",  val.len
      if val.len mod 2 != 0: val.setLen(val.len+1)
    elif n.typ.kind == tyFloat64 or n.typ.kind == tyFloat:
      val = n.floatVal.float64.toBytes
      #echo "store nkFloatLit (64) len ",  val.len, " ", treeToYaml n    
      if val.len mod 2 != 0: val.setLen(val.len+1)
    else: internalError("unknown float kind " & $n.typ.kind)
  of nkFloat64Lit:
    val = n.floatVal.float64.toBytes
    #echo "store nkFloat64Lit len ",  val.len, " ", treeToYaml n    
    if val.len mod 2 != 0: val.setLen(val.len+1)
  of nkFloat32Lit:
    val = n.floatVal.float32.toBytes
    #echo "store nkFloat32Lit len ",  val.len
    if val.len mod 2 != 0: val.setLen(val.len+1)
  of nkStrLit, nkRStrLit, nkTripleStrLit:
    # A string is represented as a pointer.
    # The pointer points to the length, first byte of the string follows.
    # So actual string pos is offset+4(ptr)+4(len), but string ptr points to
    # offset+4
    val = (w.memOffset+4).int32.toBytes & n.strval.len.int32.toBytes & n.strval
    # FIXME: the \x00 terminator is not considered in len ?
    if val.len mod 2 != 0: val.setLen(val.len+1)
  else:
    internalError("trying to store non literal value for " & $n.kind)
    #[echo "#storeindata else ",$n.kind, " ", n.typ.size
    let size = if n.typ.size<4: 4 else: n.typ.size.int # FIXME: proper sizing
    val = '\0'.repeat(size) # Reserve space
    #let store = newSt
    w.initExprs.add(gen(w,n)) # Register an initialization expression
    #echo treeToYaml n]#
  w.m.add(newDataSeg(w.nextMemIdx,w.memOffset, val))
  w.memOffset += val.len.int32

proc storeInInit(w: WasmGen, where: PSym, what: PNode) =
  case where.typ.skipTypes({tyGenericInst}).kind:
  of tyInt,tyBool, tyPointer:
    w.initExprs.add(
      newStore(memStoreI32, w.gen(what), where.crazyLoc, newConst(0))
    )
    w.memOffset += 4 # i32 => 4 bytes
  of tyFloat32:
    w.initExprs.add(
      newStore(memStoreF32, w.gen(what), where.crazyLoc, newConst(0))
    )
    w.memOffset += where.typ.size.int32
  of tyFloat:
    var storeKind: WasmOpKind 
    if where.typ.size == 4:
      storeKind = memStoreF32
    elif where.typ.size == 8:
      storeKind = memStoreF64
    else:
      internalError("impossible float size" & $where.typ.size)
    w.initExprs.add(
      newStore(storeKind, w.gen(what), where.crazyLoc, newConst(0))
    )
    w.memOffset += where.typ.size.int32
  of tyObject:
    #echo treeToYaml what
    #echo "TYPE: ", where.typ.size," ", $where.typ.kind
    let theoreticalMemOffset = w.memOffset+where.typ.size
    if what[0].sym.magic != mNone:
      if what[0].sym.magic == mDotDot:
        w.memOffset = where.crazyLoc
        if what.sonsLen > 2: # n..m
          # TODO: brittle
          w.store(what[1]) # first 
          w.store(what[2]) # last
        else: # ..n
          w.memOffset+=what[1].typ.size.int32 # FIXME: this skips the default(typ) part
          w.store(what[1])
      else:
        internalError("# store: unused magic " & $what[0].sym.magic)
    else:
      for i in 1..<what.sonsLen:
        # This should ensure the store happen at the correct memory offset
        where.typ.calcFieldOffset(what[i][0].sym)
        w.memOffset = where.crazyLoc+what[i][0].sym.crazyLoc # TODO: brittle?
        w.store(what[i])
    if w.memOffset < theoreticalMemOffset: 
      # Reserve space for all the object
      w.memOffset = theoreticalMemOffset.int32
  of tySequence:
    echo where.crazyLoc
    w.initExprs.add( # at init time store the memory loc in the ptr
      newStore(
        memStoreI32,
        newLoad(
          memLoadI32,
          heapPtrLoc, 1'i32,
          newConst(0'i32)
        ),
        where.crazyLoc, 
        newConst(0'i32)
      )
    ) # FIXME: the initial store is wrong?
    w.memOffset += 4 # i32 => 4 bytes
  of tyArray:
    let theoreticalMemOffset =  w.memOffset + 
                                what.typ.getArrayLen *
                                what.typ.lastSon.size
    for elem in what:
      w.store(elem)
    if w.memOffset < theoreticalMemOffset: 
      # Reserve space for all the array
      w.memOffset = theoreticalMemOffset.int32
  else:
    echo treeToYaml what
    internalError("# storeInInit missing case: " & $where.typ.kind)    

proc store(w:WasmGen, n:PNode, s:PSym = nil) =
  case n.kind:
  of nkEmpty:
    w.storeInData(n, s)
    #echo treeToYaml n
    #echo "offs ", s.offset
  of nkLiterals:
    w.storeInData(n)
  of nkExprColonExpr:
    w.store(n[1], n[0].sym) # literal, sym
  of nkInfix:
    w.storeInInit(s,n)
  of nkBracket:
    #echo "# store nkBracket"
    #echo treeToYaml n
    w.storeInInit(s,n)
  of nkCall:
    w.initExprs.add(w.gen(n))
  else:
    echo " # storing in init a non-literal: ", n.kind
    w.storeInInit(s, n)

proc genIdentDefs(w: WasmGen, n: PNode) =
  let 
    s = n[0].sym
  s.offset = w.memOffset
  w.store(n[2], s)

proc putVar(w:WasmGen, n:PNode) = 
  assert n.kind in {nkVarSection, nkLetSection, nkConstSection}, $n.kind
  # for each identDef, initialize the memory.
  # We store the offset in linear memory ( ie Pointer ) in sym.loc 
  # ( highly inefficient, but will do for now.)
  for iddef in n.sons:
    w.genIdentDefs(iddef)
#---------------putProc----------------------------------#
proc genBody(w: WasmGen,params:Table[string,tuple[vt:ValueType,default:PNode]], n: PNode): WasmNode =
  result = newWANode(opGroup)
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
proc putProc(w:WasmGen, s:PSym) = 
  # FIXME: ensure body len > 0 | nil
  let 
    fparams = s.typ.n
    pragmas = s.ast[4]
    body = s.getBody()
  if sfUsed notin s.flags: return
  #echo "NO----------------"
  #echo treeToYaml n[0]
  #echo typeToYaml fparams[1][1].typ.lastSon
  #echo treeToYaml fparams
  var 
    params = initTable[string,tuple[vt:ValueType,default:PNode]]()
    res = mapType(fparams[0].typ)
  
  #echo "params....", treeToYaml fparams

  for par in fparams.sons[1..^1]:
    if par.kind == nkEmpty:
      params.add(s.name.s, (ValueType.None,nil))
    elif par.kind == nkIdentDefs:
      #echo treeToYaml par
      if par.len > 3:
        for i in 0..<par.len-2: # eg a,b: int
          params.add(
            par[i].ident.s, 
            (mapType(par[^2].typ), if par[^1].kind!=nkEmpty: par[^1] else: nil ) # FIXME: detect type of param properly
          )
      else:
        #echo treeToYaml par
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
    else:
      internalError("# unknown putProc par kind: " & $par.kind)
      #echo treeToYaml par
    
  var
    fntype = newFnType(WasmType.Func)
  if res != ValueType.None:
      fntype.returns = @[res]
  fntype.params = newSeq[ValueType]()
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
    w.m.add(fntype)
    w.m.add(newFnImportEntry(headername,importcname,w.nextTotalFuncIdx)) 
    if s.flags.contains(sfExported):
      # fixme better disambiguation of exported procs
      w.m.add(newExportEntry(s.mangleName,ExternalKind.Function,w.nextImportIndex, true)) 
    w.generatedProcs.add(s.mangleName, (w.nextImportIndex,true)) 
    w.incNextImportIndex 
    inc w.nextTotalFuncIdx
  elif s.flags.contains(sfExported):
    w.m.add(fntype)
    echo s.name.s, " ", w.nextTotalFuncIdx
    w.m.add(w.nextTotalFuncIdx) # register the function in function section FIXME: export recalc
    w.m.add(newExportEntry(s.mangleName,ExternalKind.Function,w.nextFuncIdx))    
    #echo "# body\n ",treeToYaml body
    w.m.add(newFnBody(w.genBody(params,body)))
    
    w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
    inc w.nextFuncIdx 
    inc w.nextTotalFuncIdx
  elif s.flags.contains(sfUsed):
    w.m.add(fntype)
    w.m.add(w.nextTotalFuncIdx)
    w.m.add(newFnBody(w.genBody(params,body)))
    w.generatedProcs.add(s.mangleName, (w.nextFuncIdx,false)) 
    inc w.nextFuncIdx 
    inc w.nextTotalFuncIdx
  else:
    internalError("# putProc generating unused proc " & s.name.s)
    # echo treeToYaml(n)
#-----------------process-------------------------------#
var a = false
proc process(w:WasmGen, n:PNode) =
  case n.kind:
  of nkVarSection,nkLetSection,nkConstSection:
    w.putVar(n)
  # We are only interested in procs used by main module 
  #of nkProcDef, nkMethodDef, nkConverterDef:
  #  w.putProc(n)
  #of nkIteratorDef:
  #  discard # TODO:
  of nkCall,nkCommand,nkIfStmt:
    w.initExprs.add(gen(w, n))
  of nkBlockStmt:
    echo "# generating block stmt..."
    w.initExprs.add(gen(w, n))
    #echo treeToYaml n
    #[of nkTypeSection:
      #echo $n.kind, "  ",n.len, "  ", $n[0].kind
      for td in n.sons: w.process(td)
    of nkTypeDef:
      let s = n[0].sym
      if s.typ.kind == tyObject and sfUsed in s.flags:
        # Only object types ( or similar ) that are used in the module
        # are of interest to us?
        w.putTypeDesc(n)]#
    # nkCommentStmt, nkIncludeStmt,
    #  nkTemplateDef, nkPragma, nkEmpty:
    #  discard #echo "# skipping ", $n.kind
  of nkStmtList:
    for node in n: w.process(node)
  of nkAsgn:
    w.initExprs.add(gen(w, n))
  else:
    discard
    #echo "#missing process case: ",$n.kind
    #echo treeToYaml(n,0,2)

#------------------putStartSec-------------------------#
proc putStartSec(w: WasmGen) =
  # Generate the init expression
  if w.startExprs.len<1: return # no expression, no need for a start section.
  w.m.add(
    newFnBody( w.startExprs )
  )
  # TODO: check imports hoisting is already considered
  w.m.add(newFnType(WasmType.Func)) #Add the type of start
  w.m.add(w.nextTotalFuncIdx) # register start as func
  echo "start: ", w.nextTotalFuncIdx
  w.m.start = newStartSec(w.nextFuncIdx) # register start as start
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx
#------------------putInitFunc-------------------------#
proc putInitFunc(w: WasmGen) =
  # Generate the init expression
  if w.initExprs.len<1: return # no expression, no need for a init proc
  w.m.add(
    newFnBody( w.initExprs )
  )
  w.m.add(newFnType(WasmType.Func)) #Add the type of init
  # TODO: check imports hoisting is already considered
  w.m.add(w.nextTotalFuncIdx) # register init as func
  w.m.add(newExportEntry("init",ExternalKind.Function,w.nextFuncIdx))
  echo "init :", w.nextTotalFuncIdx
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx
#----------------------gen-----------------------------#
proc getMagic(m:TMagic):WasmOpKind =
  case m:
  of mLtI: result = irLtS32
  of mLeI: result = irLeS32
  of mAddI: result = ibAdd32
  of mOr: result = ibOr32
  of mAnd: result = ibAnd32
  of mXor: result = ibXor32
  of mEqI, mEqEnum, mEqRef, mEqCh, mEqB: result = irEq32
  of mEqF64: result = frEq64

  else:
    internalError("# missing getmagic " & $m)
proc genIfBranch(w:WasmGen, n:PNode):WasmNode =
  if n.kind == nkElifBranch:
    result = newIf(gen(w,n[0]),gen(w,n[1]))
  else:
    result = gen(w,n[0])  # else node is put in genIfStmt, we just
                          # want the expression from this

proc genIfStmt(w:WasmGen,n:PNode): WasmNode =
  #echo treeToYaml(n,3)
  result = newWANode(opGroup)
  result.sons.add( gen(w, n[0]) ) #first branch
  for i in 1..<n.sons.len:# this is fucked up
                          # add new else to last if
    result.sons[i-1].sons.add( newElse(gen(w,n[i])) )
    result.sons[i-1].sons.add(newEnd())
  
  # TODO: finish me properly
  #for branch in n.sons:

proc genInfix(w:WasmGen, n:PNode):WasmNode =  
  #echo treeToYaml n
  if n[0].sym.magic != mNone:
    let 
      op = getMagic(n[0].sym.magic)
      lhs = gen(w,n[1])
      rhs = gen(w,n[2])
    result = newBinaryOp(
      op, lhs, rhs
    )
  else:
    echo "# genInfix"

proc genEcho(w:WasmGen,s:PSym) =
  if w.generatedProcs.hasKey("echo"): return
  w.m.add(
    newFnType(WasmType.Func,ValueType.I32)
  )
  w.m.add(
    newFnImportEntry(
      "glue",
      "rawEcho",
      w.nextTotalFuncIdx
    )
  )
  w.m.add(
    newExportEntry(
      "echo", ExternalKind.Function, w.nextImportIndex, true
    )
  )
  w.m.add(w.nextTotalFuncIdx)
  w.generatedProcs.add("echo",(w.nextImportIndex.int,true))
  w.incNextImportIndex
  inc w.nextTotalFuncIdx

proc genDollar(w:WasmGEn,s:PSym) =
  
  if w.generatedProcs.hasKey(s.mangleName): return
  case s.magic:
  of mStrToStr:
    w.m.add( # Take adress and return adress (FIXME: basically noop)
      newFnType(ValueType.I32, WasmType.Func, ValueType.I32)
    )
    w.m.add(
      newFnBody( newReturn( newGet(woGetLocal, 0)) )
    )
    w.m.add(w.nextTotalFuncIdx)
  else:
    echo "# genDollar not covered: ", $s.magic

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false)) # move inside single case?
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc genInc(w:WasmGen, s:PSym) =
  echo "# genInc"
  w.m.add( # Take an I32 `x` and I32 `y` and return their sum
    newFnType(ValueType.I32, WasmType.Func, ValueType.I32, ValueType.I32)
  )
  w.m.add(
    newFnBody(
      newBinaryOp(
        ibAdd32,
        newGet(woGetLocal, 0),
        newGet(woGetLocal, 1)
      )
    )
  )

  w.m.add(w.nextTotalFuncIdx)

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc genDec(w:WasmGen, s:PSym) =
  echo "# genDec"
  w.m.add( # Take an I32 `x` and I32 `y` and return their difference
    newFnType(ValueType.I32, WasmType.Func, ValueType.I32, ValueType.I32)
  )
  w.m.add(
    newFnBody(
      newBinaryOp(
        ibSub32,
        newGet(woGetLocal, 0),
        newGet(woGetLocal, 1)
      )
    )
  )

  w.m.add(w.nextTotalFuncIdx)

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc genNew(w: WasmGen, s:PSym) =
  # kind of like a simplified malloc?
  echo "# genNew"
  # load bottom memory pointer
  # add `size` to it
  # store back into bottom memory pointer
  w.m.add( # Take an I32 `typesize` and add it to bottom memory pointer
            # Return new btm mem ptr
    newFnType(ValueType.I32, WasmType.Func, ValueType.I32)
  )
  w.m.add(
    newFnBody(
      [newStore(
        memStoreI32,
        newBinaryOp(
          ibAdd32,
          newLoad(
            memLoadI32,
            heapPtrLoc, 1'i32,
            newConst(0'i32)
          ),
          newGet(woGetLocal, 0)
        ),
        heapPtrLoc, 
        newConst(0'i32)
      ), 
      newLoad(
        memLoadI32,
        heapPtrLoc, 1'i32,
        newConst(0'i32)
      )]
    )
  )
  
  w.m.add(w.nextTotalFuncIdx)

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc genNewSeq(w: WasmGen, s:PSym) =
  # kind of like a simplified malloc?
  echo "# genNewSeq"
  # load bottom memory pointer
  # add `size` to it
  # store back into bottom memory pointer
  w.m.add( # Take an I32 `typesize` and add it to bottom memory pointer
            # Return new btm mem ptr
    newFnType(ValueType.I32, WasmType.Func, ValueType.I32)
  )
  w.m.add(
    newFnBody(
      [newStore(
        memStoreI32,
        newBinaryOp(
          ibAdd32,
          newLoad(
            memLoadI32,
            heapPtrLoc, 1'i32,
            newConst(0'i32)
          ),
          newGet(woGetLocal, 0)
        ),
        heapPtrLoc, 
        newConst(0'i32)
      ), 
      newLoad(
        memLoadI32,
        heapPtrLoc, 1'i32,
        newConst(0'i32)
      )]
    )
  )
  
  w.m.add(w.nextTotalFuncIdx)

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc genReset(w: WasmGen, s:PSym) =
  echo "# genReset"
  w.m.add(  # Take an I32 `ptr` and a I32 `typesize`
            # while i < size: ptr+i = "\0"
    newFnType(WasmType.Func, ValueType.I32,ValueType.I32)
  )

  var loopBody = newWANode(opGroup)

  loopBody.sons.add(
    newStore(
      memStoreI32,
      newConst(0'i32),
      0'i32,
      newGet(woGetLocal, 2)
    )
  )
  loopBody.sons.add(
    newSet(
      woSetLocal, 2'i32,
      newBinaryOp(
        ibAdd32,
        newGet(woGetLocal, 2),
        # FUTURE FIXME: when some types have size <4, this needs to be reworked too
        newConst(4'i32)
      )
    )
  )

  w.m.add(
    newFnBody(
      newWhileLoop(
        newBinaryOp(
          irLtU32,
          newGet(woGetLocal, 2),
          newBinaryOp(
            ibAdd32,
            newGet(woGetLocal, 0),
            newGet(woGetLocal, 1)
          )
        ),
        loopBody
      ),
      newLocalEntry(1, ValueType.I32)
    )
  )
  
  w.m.add(w.nextTotalFuncIdx)
  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx

proc useMagic(w: WasmGen, s: PSym, n: PNode): WasmNode =
  let needsGen = not w.generatedProcs.hasKey(s.mangleName)

  var args = newSeq[WasmNode]()
  for i in 1..<n.sons.len:
    args.add(gen(w,n[i]))

  case s.magic:
  of mEcho:
    if needsGen: genEcho(w,s)
    
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx,args, isImport)
  of mIntToStr, mStrToStr:
    if needsGen: genDollar(w,s)
    
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx,args, isImport)
  of mInc:
    if needsGen: genInc(w,s)
    
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newStore(
                memStoreI32, 
                newCall(idx, args, isImport),
                n[1].sym.crazyLoc, 
                newConst(0) )
  of mDec:
    if needsGen: genDec(w,s)
    
    let (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newStore(
                memStoreI32, 
                newCall(idx, args, isImport),
                n[1].sym.crazyLoc, 
                newConst(0) )
  of mNot:
    #echo treeToYaml n
    result = newUnaryOp(itEqz32, w.gen(n[1]))
  of mNew:
    #echo "-- mNew --"
    #echo treeToYaml n
    #echo typeToYaml n[1].typ.lastSon
    if needsGen: genNew(w,s)
    let 
      size = if n[1].typ.lastSon.size<4: 4 else: n[1].typ.lastSon.size.int
      (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newStore(
      memStoreI32,
      newCall(idx, newConst(size.int32), isImport),
      0'i32,
      newConst(n[1].sym.crazyLoc)
    )
  of mReset:
    echo "# mReset"
    #echo treeToYaml n
    if needsGen: genReset(w,s)
    let 
      size = if n[1].typ.lastSon.size<4: 4 else: n[1].typ.lastSon.size.int
      (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newCall(idx, newConst(n[1].sym.crazyLoc), newConst(size.int32), 
      isImport)
  of mUnaryLt:
    result = newBinaryOp( ibSub32, args[0], newConst(1'i32) )
  of mSucc:
    result = newBinaryOp( ibAdd32, args[0], args[1] )
  of mPred:
    result = newBinaryOp( ibSub32, args[0], args[1] )
  of mNewSeq:
    echo "-- mNewSeq --"
    echo treeToYaml n
    #echo typeToYaml n[1].typ.lastSon
    if needsGen: genNewSeq(w,s)
    let 
      size = if n[1].typ.lastSon.size<4: 4 else: n[1].typ.lastSon.size.int
      (idx, isImport) = w.generatedProcs[s.mangleName] 
    result = newStore(
      memStoreI32,
      # newSeq( varseq, len)
      newCall(idx, newBinaryOp(ibMul32, args[^1], newConst(size.int32)), isImport),
      0'i32,
      newConst(n[1].sym.crazyLoc)
    )
  else:
    debug s
    internalError("# missing magic " & $s.name.s & " " & $s.magic)
    

proc getObjFieldOffset(obj: PSym, field:PSym): int =
  echo "o genNewOffset ", obj.name.s," ", obj.offset
  echo "f genNewOffset ", field.name.s," ", field.offset
  
  # Get the offset by adding typ.size until field == 
  # The zero is at the start of the object.
  #echo typeToYaml objcstr.typ
  #echo treeToYaml objcstr
  #result = obj.crazyLoc.int
  let objType = if obj.typ.kind in {tyRef, tyPtr, tyVar}: obj.typ[0] else: obj.typ
  #echo obj.typ.len, typeToYaml obj.typ
  for objf in objType.n:
    #echo "objf ", objf.typ.size.int
    if objf.sym == field: break
      #when defined debug: echo objf.sym.name.s, "  ", objf.sym.typ.size
      #when defined debug: echo result
    result += objf.typ.size.int      

proc getLoadKind(typ:PType):WasmOpKind =
  case typ.skipTypes(abstractVarRange).kind:
  of tyInt,tyBool, tyRef, tyEnum, tyPointer,tyChar, tyString, tySequence: result = memLoadI32
  of tyFloat: result = if typ.size == 4 : memLoadF32 else: memLoadF64
  of tyFloat32: result = memLoadF32
  else:
    internalError("# getloadkind not specified for " & $typ.kind & " as " & $typ.skipTypes(abstractVarRange).kind)

proc accessAux(n: PNode, offset: int32): WasmNode =
  if n.typ.kind in passedAsBackendPtr: #needsDeref
    when defined debug: echo "# accessAux passedAsPtr ", n[1].typ.kind,"name: ", n[1].sym.name.s ," loc: ",$n[1].sym.loc.r
    result = newConst(offset)
  else:
    when defined debug: echo "# accessAux not passedAsPtr ", n.typ.kind
    result = newLoad(
      n.typ.getLoadKind(),
      offset, 1'i32,
      newConst(0'i32)
    )

proc accessAux(n: PNode, offset: WasmNode): WasmNode =
  #echo treeToYaml n
  if n.typ.kind in passedAsBackendPtr: #needsDeref
    when defined debug: echo "# accessAuxNode passedAsPtr ", n[1].typ.kind," name: ", n[1].sym.name.s ," loc: ",$n[1].sym.offset
    result = newLoad(
      memLoadI32,
      0'i32, 1'i32,
      offset
    )
  else:
    when defined debug: echo "# accessAuxNode not passedAsPtr ", n.typ.kind
    result = newLoad(
      n.typ.getLoadKind(),
      0'i32, 1'i32,
      offset
    )
    

proc genFieldAccess(w:WasmGen, n: PNode): WasmNode =
  #TODO: rework to have only a newLoad( loadKind, fieldOffset, 1, objLoc )
  # echo n[0].sym.name.s, " offs: ", n[0].sym.getObjFieldOffset(n[1].sym)
  if n[0].kind == nkSym: 
    n[0].sym.typ.calcFieldOffset(n[1].sym)
    result = n.accessAux(n[0].sym.crazyLoc+n[1].sym.crazyLoc)
  elif n[0].kind == nkHiddenDeref:
    n[0][0].sym.typ.calcFieldOffset(n[1].sym)
    let calculatedOffs = newBinaryOp(
      ibAdd32,
      newLoad(memLoadI32, 0'i32, 1'i32, newConst(n[0][0].sym.crazyLoc)),
      newConst(n[1].sym.crazyLoc)
    )
    result = n.accessAux(calculatedOffs)
  else:
    internalError("wtf " & $n[0].kind)

proc genArrayAccess(w:WasmGen, n: PNode): WasmNode =
  #echo treeToYaml n
  case n[1].kind:
  of nkIntLit:
    echo treeToYaml n
    if n[0].typ.kind == tySequence:
      result = n.accessAux(
        newBinaryOp(
          ibAdd32,
          newLoad(memLoadI32, 0'i32, 1'i32, newConst(n[0].sym.crazyLoc)),
          newBinaryOp(
            ibMul32,
            newConst(n[0].typ.lastSon.size.int32),
            w.gen(n[1])
          )
        )
      )
    else:
      let elemOffset = n[0].sym.crazyLoc + (n[0].typ.lastSon.size * n[1].intVal).int32
      result = n.accessAux(elemOffset)
  of nkSym:
    result = n.accessAux(
      newBinaryOp(
        ibAdd32,
        newConst(n[0].sym.crazyLoc),
        newBinaryOp(
          ibMul32,
          newConst(n[0].typ.lastSon.size.int32),
          w.gen(n[1])
        )
      )
    )
  else:
    internalError("missing branch " & $n[1].kind)

proc genDeref(w: WasmGen, n: PNode): WasmNode =
  #echo "genderef", treeToYaml n
  result = n.accessAux(w.gen(n))

proc genConv( frm, to: TTypeKind, convertee: WasmNode): WasmNode =
  var convOp : WasmOpKind
  case to:
  of tyFloat:
    case frm:
    of tyFloat32: convOp = cvPromoteF64_F32
    else: 
      internalError("missing conversion for " & $frm)  
  else:
    internalError("missing conversion for " & $to)
  newUnaryOp(convOp, convertee)

proc genAsgn(w:WasmGen, lhsNode, rhs: PNode): WasmNode =
  #echo "lhs...", treeToYaml lhsnode
  #echo "rhs...", treeToYaml rhs
  var 
    lhsIndex: WasmNode
    lhsOffset = 0'i32 # TODO: a node?

  case lhsNode.kind
  of nkDerefExpr: 
    lhsIndex = newLoad(memLoadI32, 0'i32, 1'i32, newConst(lhsNode[0].sym.crazyLoc))
  of nkSym:
    if lhsNode.sym.typ.kind == tyRef:
      lhsIndex = newConst(lhsNode.sym.crazyLoc)
    else:
      lhsIndex = newLoad(memLoadI32, 0'i32, 1'i32, newConst(lhsNode.sym.crazyLoc))
  of nkDotExpr:
    #echo "genAsgn nkDotexpr ", treeToYaml lhsNode
    if lhsNode[0].kind == nkSym:
      lhsIndex = newLoad(memLoadI32, 0'i32, 1'i32, newConst(lhsNode[0].sym.crazyLoc))
      lhsOffset = lhsNode[1].sym.crazyLoc
      #lhsOffset = lhsNode[0].sym.crazyLoc+ lhsNode[1].sym.crazyLoc
    elif lhsNode[0].kind in {nkHiddenDeref}:
      #echo lhsNode[0][0].sym.name.s, lhsNode[0][0].sym.crazyLoc, " // ", lhsNode[1].sym.name.s, lhsNode[1].sym.crazyLoc
      lhsIndex = newLoad(
        memLoadI32,
        0'i32, 1'i32,
        newConst(lhsNode[0][0].sym.crazyLoc)
      )
      lhsOffset = lhsNode[1].sym.crazyLoc
    else:
      internalError("genAsgn nkDotExpr error")
  of nkBracketExpr:
    # lhs[i] = rhs
    if lhsNode[0].kind == nkSym:
      lhsIndex = newLoad(memLoadI32, 0'i32, 1'i32, newConst(lhsNode[0].sym.crazyLoc))
    elif lhsNode[0].kind in {nkHiddenDeref}:
      #echo lhsNode[0][0].sym.name.s, lhsNode[0][0].sym.crazyLoc, " // ", lhsNode[1].sym.name.s, lhsNode[1].sym.crazyLoc
      lhsIndex = newLoad(
        memLoadI32,
        0'i32, 1'i32,
        newConst(lhsNode[0][0].sym.crazyLoc)
      )
    else:
      internalError("genAsgn nkBracketExpr error")
    case lhsNode[1].kind:
      of nkIntLit:
        lhsOffset = (lhsNode[0].typ.lastSon.size * lhsNode[1].intVal).int32
      of nkSym:
        lhsIndex = newBinaryOp(
          ibAdd32,
          lhsIndex,
          newBinaryOp(
            ibMul32,
            newConst(lhsNode[0].typ.lastSon.size.int32),
            w.gen(lhsNode[1])
          )
        )
      else:
        internalError("# genAsgn nkBracketExpr missing branch " & $lhsNode[1].kind)

    echo render lhsIndex
    echo render lhsOffset
  else: 
    internalError("unhandled lhs kind " & $lhsNode.kind)
  
  case lhsNode.typ.kind:
  of tyInt,tyBool,tyInt32, tyEnum:
    result = newStore(memStoreI32, w.gen(rhs), lhsOffset, lhsIndex)
  of tyRef:
    echo lhsnode.kind, " tyRef->", repr lhsOffset
    result = newStore(memStoreI32, w.gen(rhs), lhsOffset, lhsIndex) #w.gen(lhsNode))
  of tyFloat32:
    echo "tyfl32genASgn", repr lhsOffset
    result = newStore(memStoreF32, w.gen(rhs), lhsOffset, lhsIndex)
  of tyFloat:
    var storeKind: WasmOpKind 
    if lhsNode.typ.size == 4:
      storeKind = memStoreF32
    elif lhsNode.typ.size == 8:
      storeKind = memStoreF64
    else:
      internalError("impossible float size" & $lhsNode.typ.size)
    result = newStore(storeKind, w.gen(rhs), lhsOffset, lhsIndex)
  of tyString:
    result = newStore(memStoreI32, w.gen(rhs), lhsOffset, lhsIndex) #w.gen(lhsNode))
  else:
    #echo treeToYaml rhs    
    internalError("# genAsgn missing case: " & $lhsNode.typ.kind)

proc genRaiseStmt(w: WasmGen, n: PNode): WasmNode =
  echo "#genRaiseStmt \n", treeToYaml n
  # TODO:
  result = newWANode(woUnreachable) # Trap
  #[w.m.add( # Take an I32 `x` and I32 `y` and return their sum
    newFnType(ValueType.I32, WasmType.Func, ValueType.I32, ValueType.I32)
  )
  w.m.add(
    newFnBody(
      newBinaryOp(
        ibAdd32,
        newGet(woGetLocal, 0),
        newGet(woGetLocal, 1)
      )
    )
  )

  w.m.add(w.nextTotalFuncIdx)

  w.generatedProcs.add(s.mangleName,(w.nextFuncIdx.int,false))
  inc w.nextFuncIdx
  inc w.nextTotalFuncIdx]#



proc putTypeInfo(w:WasmGen, n:PNode) =
  #echo treeToYaml n
  let s = n.sym
  # what = n.sym.ast[0]
  echo "storing: ", s.name.s, "at ", w.memOffset
  let storeTypeInfo = newStore(
    memStoreI32,
    newConst(w.memOffset), w.memOffset, newConst(0'i32)
  )
  w.generatedTypeInfos.add(s.mangleName, w.memOffset)
  w.initExprs.add(storeTypeInfo)
  s.offset = w.memOffset
  w.memOffset += 4

proc putResultVar(w:WasmGen, n:PNode) =
  #echo treeToYaml n
  let s = n.sym
  # what = n.sym.ast[0]
  echo "storing: ", s.name.s, "at ", w.memOffset
  let storeResultVar = newStore(
    memStoreI32,
    newConst(0'i32), w.memOffset, newConst(0'i32)
  )
  w.initExprs.add(storeResultVar)
  s.offset = w.memOffset
  w.memOffset += 4
  

proc gen(w: WasmGen, n: PNode): WasmNode =
  echo $n.kind
  case n.kind:
  of nkNilLit:
    result = newConst(0'i32)
  of nkIntLit, nkInt32Lit, nkCharLit:
    result = newConst(n.intVal.int32)
  of nkFloat32Lit:
    result = newConst(n.floatVal.float32)
  of nkFloatLit, nkFloat64Lit:
    result = newConst(n.floatVal.float64)
  of nkSym:
    case n.sym.kind:
    of skParam:
      # Symbol is a param in a call
      echo "passed as param ", $n.sym.name.s, " pos:", n.sym.position
      return newGet(woGetLocal, n.sym.position)
    of skType:
      # Symbol is a type
      if not w.generatedTypeInfos.hasKey(mangleName(n.sym)):
        echo "generating skType"
        w.putTypeInfo(n)
    of skResult:
      #echo "#skResult"
      w.putResultVar(n)
    else:
      echo "discard sk: " & $n.sym.kind & " name: " & n.sym.name.s
    if n.typ.skipTypes(abstractVarRange).kind in passedAsBackendPtr: #needsDeref
      when defined debug: 
        echo "# passedAsPtr ", n.typ.kind," name: ", n.sym.name.s 
        #echo treeToYaml n
        debug n.sym
        echo "# -> loc: ",n.sym.crazyLoc
      result = newConst(n.sym.crazyLoc) # WTF this is insane
    else:
      when defined debug: echo "# nkSym ", n.sym.name.s, " not passedAsPtr ", n.typ.kind
      result = newLoad(
        n.typ.getLoadKind(),
        n.sym.crazyLoc, 1'i32,
        newConst(0'i32)
      )
  of nkStrLit:
    # store the literal (in heap?)
    #var ofs = w.memOffset.int
    result = newConst(w.memOffset) # result points to literal    
    w.initExprs.add(newStore(memStoreI32, n.strVal, w.memOffset))
    #w.memOffset = ofs
  of nkBracket:
    result = newWANode(opGroup)
    for son in n.sons:
      add result.sons, w.gen(son)
  of nkCall,nkCommand,nkHiddenCallConv, nkPrefix:
    let s = n[namePos].sym
    #echo "debugging ", s.name.s
    #debug s
    #echo treeToYaml n
    if s.magic != mNone:
      result = w.useMagic(s, n)
    else:
      if not w.generatedProcs.hasKey(s.mangleName):
        #echo "----- ", s.name.s ," -----\n",typeToYaml s.typ
        w.putProc(s)
      var args = newSeq[WasmNode]()
      for i in 1..<n.sons.len:
        args.add(gen(w,n[i]))
      let (idx, isImport) = w.generatedProcs[s.mangleName] 
      result = newCall(idx,args, isImport)
    #echo w.generatedProcs
  of nkAsgn: result = w.genAsgn(n[0], n[1])
  of nkVarSection: w.process(n)
  of nkInfix: result = genInfix(w,n)
  of nkIfStmt: result = genIfStmt(w,n)
  of nkElifBranch,nkElse: result = genIfBranch(w,n)
  of nkStmtList, nkStmtListExpr:
    let isExpr = not isEmptyType(n.typ)
    result = newWANode(opGroup)
    for i in countup(0, sonsLen(n) - 1 - isExpr.ord):
      let t = gen(w, n.sons[i])
      if not t.isNil:
        result.sons.add(t)
    if isExpr:
      result.sons.add( gen(w, lastSon(n)) )
  of nkWhileStmt: result = newWhileLoop( w.gen(n[0]), w.gen(n[1]) )
  of nkBlockStmt:
    # todo use the label in n[0]? (maybe a table for breaking)
    case n[1].kind:
    of nkWhileStmt:
      #echo treeToYaml n
      result = w.gen(n[1])
    of nkStmtList:
      result = w.gen(n[1])
    else:
      internalError("case to be implemented" & $n[1].kind)
  of nkRaiseStmt:
    result = w.genRaiseStmt(n)
  of nkDotExpr:
    result = w.genFieldAccess(n)
  of nkBracketExpr:
    result = w.genArrayAccess(n)
  of nkHiddenStdConv:
    #echo treeToYaml n
    let
      to = n.typ.kind
      frm = n[1].typ.kind
    result = genConv(frm, to, w.gen(n[1]))
  of nkDerefExpr:
    result = w.genDeref(n[0])
  of nkCommentStmt: discard # not used
  else:
    internalError("# no gen yet " & $n.kind)
    #echo treeToYaml n
#-------------putHeapPtr-------------------------------#
proc putHeapPtr(w:WasmGen) =
  w.m.add(newDataSeg(
    w.nextMemIdx,
    heapPtrLoc, # heaptr is stored in bytes [4..12) 
    w.memOffset.unsignedLEB128)
  )
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
  #skip system for now
  #if not w.s.flags.contains(sfSystemModule): process(w,n)
  process(w, n)
  result = n

proc myClose(graph: ModuleGraph; b: PPassContext, n: PNode): PNode =
  echo "# closing ", $n.kind, " from ", n.info.fileIndex.toFilename
  if passes.skipCodegen(n): return n
  result = myProcess(b, n)
  var w = WasmGen(b)
  w.putInitFunc
  w.putStartSec
  w.putHeapPtr
  let ext = "wasm"
  let f = w.s.info.fileIndex.toFilename
  let outfile =
    if options.outFile.len > 0:
      if options.outFile.isAbsolute: options.outFile
      else: getCurrentDir() / options.outFile
    else:
      changeFileExt(completeCFilePath(f), ext)
  writeFile(outfile, encode(w.m))
  writeFile(outfile.changeFileExt("json"), render(w.m))
  if w.s.flags.contains(sfMainModule): linkPass(outfile, w)

proc myOpenCached(graph: ModuleGraph; s: PSym, rd: PRodReader): PPassContext =
  # TODO: is this even true?
  internalError("symbol files are not possible with the WASM code generator")
  result = nil

proc myOpen(graph: ModuleGraph; s: PSym; cache: IdentCache): PPassContext =
  echo "# begin myOpen ",s.info.fileIndex.toFilename," s.name: ",$s.name.s
  var wg = newWasmGen(s)
  #TODO: other secs
  result = wg
  echo "# end myOpen ",s.info.fileIndex.toFilename," s.name: ",$s.name.s

const WasmGenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)

# TODO: do not generate procs if they come from a different module (checking sym.info?)

# TODO: WasmLinkPass that links modules together ( takes a list of WasmGen? )
# while moremodules:
#   check imports in table[module, symbol]
#   if imports.len>0: genImports
#   add imports to table[module, symbol]
#   link in js ( importObj: <name>: <name>.exports ) or something similar
#generate html file