import waenums, leb128
from math import ceil,log2
type
  bytes = string
  varuint32 = int
  varuint1 = int
  MemoryImmediate* = ref object
    align*: varuint32 # log2(alignment)
    offset*: varuint32 
  
  BrTargets* = ref object
    #count var uint32
    targetTable*: seq[varuint32]
    default*: varuint32
  
  WasmNode* = ref object
    case kind*: WasmOpKind
    of woBlock, woLoop, woIf:
      sig*: BlockType
    of woBr, woBrIf:
      relativeDepth*: varuint32
    of woBrTable:
      imm*: BrTargets
    of woCall:
      funcIndex* : varuint32
      isImport* : bool
    of woCallIndirect:
      typeIndex*: varuint32
      reserved*: varuint1 # 0 in MVP
    of woGetLocal, woSetLocal, woTeeLocal:
      localIndex*: varuint32
    of woGetGlobal, woSetGlobal:
      globalIndex*: varuint32
    of MemLoad, MemStore:
      memoryImm*: MemoryImmediate
    of memSize, memGrow:
      memReserved*: varuint1
    of Consts:
      value*: bytes # the bytes of the value
    #of woUnOp, woBinOp, woRelOp, woTestOp, woCvtOp:
    else:
      discard
    sons*: seq[WasmNode]  # If kind == opGroup, the operations are stored here.
                    # Otherwise this olds args ( eg for binaryOp etc )

proc a*(op:WasmNode):WasmNode = 
  if op.sons.len>0: op.sons[0]
  else: nil
proc b*(op:WasmNode):WasmNode =
  if op.sons.len>0: op.sons[1]
  else: nil
proc `a=`*(op:WasmNode,val:WasmNode) = 
  if op.sons.len>0:
    op.sons[0] = val
  else:
    op.sons.add(val)
proc `b=`*(op:WasmNode,val:WasmNode) =
  assert(op.sons.len>0, $op.sons.len)
  if op.sons.len>1:
    op.sons[0] = val
  elif op.sons.len>0:
    op.sons.add(val)

proc newWANode*(op:WasmOpKind):WasmNode =
  # assert op.kind arity == 0
  WasmNode(kind:op, sons: newSeq[WasmNode]())

proc newUnaryOp*(op:WasmOpKind,a:WasmNode):WasmNode =
  # assert op.kind == unaryOp
  result = newWANode(op)
  result.a = a

proc newBinaryOp*(op:WasmOpKind,a,b:WasmNode):WasmNode =
  assert(op in BinaryOp, $op)
  result = newWANode(op)
  result.a = a
  result.b = b

proc newCall*(idx:int,args:varargs[WasmNode], isImport: bool = false):WasmNode =
  result = newWANode(woCall)
  result.funcIndex = idx
  result.isImport = isImport
  result.sons = @args

proc newLoad*(op:WasmOpKind, offset, alignment: varuint32=1,idx:WasmNode):WasmNode =
  assert op in MemLoad
  result = newWANode(op)
  result.memoryImm = MemoryImmediate(
    align: ceil(log2(abs(alignment).float)).int,
    offset: offset
  )
  result.a = idx

proc newGet*(op:WasmOpKind,idx:int):WasmNode =
  assert op in {woGetGlobal,woGetLocal}
  result = newWANode(op)
  if op == woGetGlobal:
    result.globalIndex = idx
  else:
    result.localIndex = idx

proc newSet*(op:WasmOpKind,idx:int, what:WasmNode):WasmNode =
  assert op in {woSetLocal,woSetGlobal}
  result = newWANode(op)
  if op == woSetGlobal:
    result.globalIndex = idx
  else:
    result.localIndex = idx
  result.a = what

proc newReturn*(what:WasmNode): WasmNode {.inline.}=
  newUnaryOp(woReturn, what)

proc newAddress*(val:string):WasmNode =
  result = newWANode(constI32)
  result.value = val

proc newConst*[T:int32|int64|float32|float64|bytes](val:T):WasmNode =
  var op: WasmOpKind
  when T is int32 or T is int:
    op = constI32
  elif T is int64:
    op = constI64
  elif T is float32:
    op = constF32
  elif T is float64:
    op = constF64
  elif T is string:
    op = constI32
  else:
    # other branches shouldn't be possible
    assert(false,"Impossible type")
  result = newWANode(op)
  when T is SomeUnsignedInt:
    result.value = val.int32.unsignedLeb128
  elif T is SomeSignedInt:
    result.value = val.int32.signedLeb128
  elif T is bytes:
    result.value = val
  else:
    result.value = toBytes(val)

proc newStore*(kind:WasmOpKind, val:bytes, offset: var int32):WasmNode

proc newStoreAux(val:bytes,offset: var int32,res: var WasmNode) =
  res = newWANode(opGroup)
  # store as seq of 4byte store op
  for i in countup(0,val.len-1,4):
    #let dt = val.len-i
    var chunk = val[i..i+3]
    if i>=val.len-1:
      chunk.setLen(chunk.len+(i-val.len))
    res.sons.add(newStore(memStoreI32,chunk,offset))
    offset+=4

proc newStore*(kind:WasmOpKind, val:bytes, offset: var int32):WasmNode =
  assert kind in MemStore, $kind
  result = newWANode(kind)
  if  val.len>4:
    # can't just store the thing in one go
    newStoreAux(val,offset,result)
  else:
    result.sons.add( newConst( 0'i32 ) )
    result.sons.add( newConst(val) ) 
    result.memoryImm = MemoryImmediate(
      #align: ceil(log2(align.float)).int,
      offset: offset
    )
    offset+=4

proc newStore*(kind:WasmOpKind, what: varargs[WasmNode],offset: int32,index:WasmNode=newConst(0'i32)): WasmNode =
  assert kind in MemStore, $kind
  result = newWANode(kind)
  result.sons.add( index )
  result.sons.add( what ) 
  result.memoryImm = MemoryImmediate(
    #align: ceil(log2(align.float)).int,
    offset: offset
  )



proc newEnd*():WasmNode {.inline.} = newWANode(woEnd) 

proc newWhileLoop*(cond:WasmNode, inner:WasmNode):WasmNode =
  # https://github.com/rhmoller/wasm-by-hand/blob/master/src/controlflow.wat
  result = newWANode(woBlock)
  result.sig = BlockType.Pseudo
  var 
    brkcond = newWANode(woBrIf)  
    brret = newWANode(woBr)
    loop = newWANode(woLoop)
  
  brkcond.sons.add(newUnaryOp(itEqz32, cond))
  brkcond.relativeDepth = 1

  brret.relativeDepth = 0

  loop.sig = BlockType.Pseudo
  loop.sons.add( [brkcond, inner, brret, newEnd()] )

  result.sons.add([loop,newEnd()])

proc newElse*(then:WasmNode):WasmNode =
  result = newWANode(woElse)
  result.a = then
  
proc newIf*(cond:WasmNode, then:WasmNode #[, other:WasmNode=nil]#):WasmNode =
  # if condition is in the stack
  #result = newWANode(opGroup)
  #result.sons.add(cond)
  # the if block
  result = newWANode(woIf)
  result.sig = BlockType.Pseudo
  result.a = cond
  result.b = then
  #ifstmt.sons.add(then)
  #if other!=nil: # else block
  #  ifstmt.sons.add(newElse(other))
  #ifstmt.sons.add(newEnd())
  #result.sons.add(ifstmt)
