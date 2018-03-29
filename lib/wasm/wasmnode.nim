import wasmast, wasmleb128
from math import ceil,log2

proc newWANode*(op:WasmOpKind):WasmNode =
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

proc newAdd32*(a,b:WasmNode):WasmNode =
  result = newWANode(ibAdd32)
  result.a = a
  result.b = b

proc newMul32*(a,b:WasmNode):WasmNode =
  result = newWANode(ibMul32)
  result.a = a
  result.b = b

proc newCall*(idx:int,args:varargs[WasmNode], isImport: bool = false):WasmNode =
  result = newWANode(woCall)
  result.funcIndex = idx
  result.isImport = isImport
  result.sons = @args

proc newLoad*(op:WasmOpKind, offset, alignment: Natural=1,idx:WasmNode):WasmNode =
  assert op in MemLoad
  result = newWANode(op)
  result.align = ceil(log2(abs(alignment).float)).int
  result.offset = offset
  result.a = idx

proc newTeeLocal*(idx: int, what: WasmNode): WasmNode =
  result = newWANode(woTeeLocal)
  result.index = idx
  result.a = what

proc newGet*(op:WasmOpKind,idx:int):WasmNode =
  assert op in {woGetGlobal,woGetLocal}
  result = newWANode(op)
  result.index = idx
proc newSet*(op:WasmOpKind,idx:int, what:WasmNode):WasmNode =
  assert op in {woSetLocal,woSetGlobal}
  result = newWANode(op)
  result.index = idx
  result.a = what

proc newReturn*(what:WasmNode): WasmNode {.inline.}=
  newUnaryOp(woReturn, what)

proc newConst*(val:SomeSignedInt):WasmNode =
  when val is int64:
    result = newWANode(constI64)
  else:
    result = newWANode(constI32)
  result.intVal = val

proc newConst*(val:SomeUnsignedInt):WasmNode =
  when val is uint64:
    result = newWANode(constUI64)
  else:
    result = newWANode(constUI32)
  result.uintVal = val

proc newConst*(val:SomeReal):WasmNode =
  when val is float32:
    result = newWANode(constF32)
  else:
    result = newWANode(constF64)
  result.floatVal = val

proc newStore*(kind:WasmOpKind, what: varargs[WasmNode],offset: int32,index:WasmNode=newConst(0'i32)): WasmNode =
  assert kind in MemStore, $kind
  result = newWANode(kind)
  result.sons.add( index )
  result.sons.add( what ) 
  #align: ceil(log2(align.float)).int,
  result.offset = offset

proc newEnd*():WasmNode {.inline.} = newWANode(woEnd) 

proc newWhileLoop*(cond:WasmNode, inner:WasmNode):WasmNode =
  # https://github.com/rhmoller/wasm-by-hand/blob/master/src/controlflow.wat
  result = newWANode(woBlock)
  result.sig = ltPseudo
  var 
    brkcond = newWANode(woBrIf)  
    brret = newWANode(woBr)
    loop = newWANode(woLoop)
  
  brkcond.sons.add(newUnaryOp(itEqz32, cond))
  brkcond.relativeDepth = 1

  brret.relativeDepth = 0

  loop.sig = ltPseudo
  loop.sons.add( [brkcond, inner, brret, newEnd()] )

  result.sons.add([loop,newEnd()])

proc newElse*(then:WasmNode):WasmNode =
  result = newWANode(woElse)
  result.a = then
  
proc newIf*(cond:WasmNode, then:WasmNode #[, other:WasmNode=nil]#):WasmNode =
  result = newWANode(woIf)
  result.sig = ltPseudo
  result.a = cond
  result.b = then

proc newIfElse*(cond:WasmNode, then:WasmNode, other:WasmNode):WasmNode =
  result = newWANode(opList)
  result.sons.add(newIf(cond, then))
  if other.kind != woNop:
    result.sons.add(newElse(other))
  result.sons.add(newEnd())

proc newOpList*(ops: varargs[WasmNode]): WasmNode =
  result = newWANode(opList)
  result.sons = @ops