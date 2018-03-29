type
  WasmValueType* = enum
    vtNone,
    vtF64, vtF32
    vtI64, vtI32,
    ltPseudo

  WasmExternalKind* = enum
    ## A single-byte unsigned integer indicating the kind of definition being imported or defined:
    ekFunction ## indicating a Function import or definition
    ekTable  ## indicating a Table import or definition
    ekMemory ## indicating a Memory import or definition
    ekGlobal ## indicating a Global import or definition

  WasmOpKind* = enum
    # ControlFlow
    woUnreachable, woNop, woBlock, woLoop,
    woIf, woElse, woEnd, woBr, woBrIf,
    woBrTable, woReturn,
    # Call
    woCall, woCallIndirect,
    # 
    woDrop, woSelect,
    # Variable access
    #  Local
    woGetLocal, woSetLocal, woTeeLocal,
    #  Global
    woGetGlobal, woSetGlobal,
    # Memory related
    #  Load
    memLoadI32, memLoadI64, memLoadF32, memLoadF64,
    memLoad8S_I32, memLoad8U_I32, memLoad16S_I32, memLoad16U_I32,
    memLoad8S_I64, memLoad8U_I64, memLoad16S_I64, memLoad16U_I64,
    memLoad32S_I64, memLoad32U_I64,
    #  Store
    memStoreI32, memStoreI64, memStoreF32, memStoreF64,
    memStore8_I32, memStore16_I32, memStore8_I64, memStore16_I64,
    memStore32_I64,
    #  Grow, Size
    memSize, # Query memory size
    memGrow,
    # Const
    constI32, constI64, constF32, constF64,
    # Conversion
    # cv TO_FROM
    cvWrapI32_I64, cvTruncI32S_F32, cvTruncI32U_F32,
    cvTruncI32S_F64, cvTruncI32U_F64, 
    cvExtendI64S_I32, cvExtendI64U_I32, 
    cvTruncI64S_F32, cvTruncI64U_F32,
    cvTruncI64S_F64, cvTruncI64U_F64,
    cvConvertF32S_I32, cvConvertF32U_I32,
    cvConvertF32S_I64, cvConvertF32U_I64, cvDemoteF32_F64,
    cvConvertF64S_I32, cvConvertF64U_I32,
    cvConvertF64S_I64, cvConvertF64U_I64, cvPromoteF64_F32,
    # Reinterpret
    rpReinterpretF32_I32, rpReinterpretF64_I64,
    rpReinterpretI32_F32, rpReinterpretI64_F64,
    # Int
    #  Unary
    iuClz32, iuCtz32, iuPopCnt32,
    #  Binary
    ibAdd32, ibSub32, ibMul32, ibDivS32, 
    ibDivU32, ibRemS32, ibRemU32, ibAnd32,
    ibOr32, ibXor32, ibShl32, ibShrS32, 
    ibShrU32, ibRotl32, ibRotr32,
    #  Test
    itEqz32,
    #  Relation
    irEq32, irNe32, irLtU32, irLtS32,
    irGtU32, irGtS32, irLeU32, irLeS32,
    irGeU32, irGeS32,
    #  Unary
    iuClz64, iuCtz64, iuPopCnt64,
    #  Binary
    ibAdd64, ibSub64, ibMul64, ibDivS64, 
    ibDivU64, ibRemS64, ibRemU64, ibAnd64,
    ibOr64, ibXor64, ibShl64, ibShrS64, 
    ibShrU64, ibRotl64, ibRotr64,
    #  Test
    itEqz64,
    #  Relation
    irEq64, irNe64, irLtU64, irLtS64,
    irGtU64, irGtS64, irLeU64, irLeS64, 
    irGeU64, irGeS64,
    # Float
    #  Unary
    fuNeg32, fuAbs32, fuCeil32, fuFloor32, 
    fuTrunc32, fuNearest32, fuSqrt32,
    #  Binary
    fbAdd32, fbSub32, fbMul32, fbDiv32, 
    fbMin32, fbMax32, fbCopySign32,
    # Rel
    frEq32, frNe32, frLt32, frGt32, frLe32, frGe32,
    #  Unary
    fuNeg64, fuAbs64, fuCeil64, fuFloor64, 
    fuTrunc64, fuNearest64, fuSqrt64,
    #  Binary
    fbAdd64, fbSub64, fbMul64, fbDiv64,
    fbMin64, fbMax64, fbCopySign64,
    #  Rel
    frEq64, frNe64, frLt64, frGt64, frLe64, frGe64,

    opList, constUI32, constUI64
  
const 
  MemLoad* = {
    memLoadI32, memLoadI64, memLoadF32, memLoadF64,
    memLoad8S_I32, memLoad8U_I32, memLoad16S_I32, memLoad16U_I32,
    memLoad8S_I64, memLoad8U_I64, memLoad16S_I64, memLoad16U_I64,
    memLoad32S_I64, memLoad32U_I64
  }
  MemStore* = {
    memStoreI32, memStoreI64, memStoreF32, memStoreF64,
    memStore8_I32, memStore16_I32, memStore8_I64, memStore16_I64,
    memStore32_I64
  }
  Consts* = {constI32, constI64, constUI32, constUI64, constF32, constF64}

type
  WasmNode* = ref object
    case kind*: WasmOpKind
    of woBlock, woLoop, woIf:
      sig*: WasmValueType # vtNone === Pseudo
    of woBr, woBrIf:
      relativeDepth*: Natural
    of woBrTable:
      targetTable*: seq[Natural]
      default*: Natural
    of woCall:
      funcIndex* : Natural
      isImport* : bool
    of woCallIndirect:
      typeIndex*: Natural
      reserved*: Natural # 0 in MVP
    of woGetLocal, woSetLocal, woTeeLocal, woGetGlobal, woSetGlobal:
      index*: Natural
    of MemLoad, MemStore:
      align*: Natural # log2(alignment)
      offset*: Natural
    of memSize, memGrow:
      memReserved*: Natural
    of constI32, constI64:
      intVal*: BiggestInt
    of constUI32, constUI64:
      uintVal*: BiggestUInt
    of constF32, constF64:
      floatVal*: BiggestFloat
    else:
      discard
    sons*: seq[WasmNode]  # If kind == opList, the actual operations are stored here.
                    # Otherwise this olds args ( eg for binaryOp etc )

const
  UnaryOp* = {
    cvWrapI32_I64, cvTruncI32S_F32, cvTruncI32U_F32,
    cvTruncI32S_F64, cvTruncI32U_F64, 
    cvExtendI64S_I32, cvExtendI64U_I32, 
    cvTruncI64S_F32, cvTruncI64U_F32,
    cvTruncI64S_F64, cvTruncI64U_F64,
    cvConvertF32S_I32, cvConvertF32U_I32,
    cvConvertF32S_I64, cvConvertF32U_I64, cvDemoteF32_F64,
    cvConvertF64S_I32, cvConvertF64U_I32,
    cvConvertF64S_I64, cvConvertF64U_I64, cvPromoteF64_F32,
    rpReinterpretF32_I32, rpReinterpretF64_I64,
    rpReinterpretI32_F32, rpReinterpretI64_F64,
    iuClz32, iuCtz32, iuPopCnt32,
    itEqz32, itEqz64,
    iuClz64, iuCtz64, iuPopCnt64,
    fuNeg32, fuAbs32, fuCeil32, fuFloor32, 
    fuTrunc32, fuNearest32, fuSqrt32,
    fuNeg64, fuAbs64, fuCeil64, fuFloor64, 
    fuTrunc64, fuNearest64, fuSqrt64
  }

  BinaryOp* = {
    ibAdd32, ibSub32, ibMul32, ibDivS32, 
    ibDivU32, ibRemS32, ibRemU32, ibAnd32,
    ibOr32, ibXor32, ibShl32, ibShrS32, 
    ibShrU32, ibRotl32, ibRotr32,
    #  Relation
    irEq32, irNe32, irLtU32, irLtS32,
    irGtU32, irGtS32, irLeU32, irLeS32,
    irGeU32, irGeS32,
    #  Binary
    ibAdd64, ibSub64, ibMul64, ibDivS64, 
    ibDivU64, ibRemS64, ibRemU64, ibAnd64,
    ibOr64, ibXor64, ibShl64, ibShrS64, 
    ibShrU64, ibRotl64, ibRotr64,
    #  Relation
    irEq64, irNe64, irLtU64, irLtS64,
    irGtU64, irGtS64, irLeU64, irLeS64, 
    irGeU64, irGeS64,
    # Float
    #  Binary
    fbAdd32, fbSub32, fbMul32, fbDiv32, 
    fbMin32, fbMax32, fbCopySign32,
    # Rel
    frEq32, frNe32, frLt32, frGt32, frLe32, frGe32,
    #  Binary
    fbAdd64, fbSub64, fbMul64, fbDiv64,
    fbMin64, fbMax64, fbCopySign64,
    #  Rel
    frEq64, frNe64, frLt64, frGt64, frLe64, frGe64
  }

proc a*(op:WasmNode):WasmNode = 
  assert(op.sons.len>0, $op.sons.len)
  op.sons[0]

proc b*(op:WasmNode):WasmNode =
  assert(op.sons.len>1, $op.sons.len)
  op.sons[1]
  
proc `a=`*(op:WasmNode,val:WasmNode) = 
  if op.sons.len>0:
    op.sons[0] = val
  else:
    op.sons.add(val)
proc `b=`*(op:WasmNode,val:WasmNode) =
  assert(op.sons.len>0, $op.sons.len)
  if op.sons.len>1:
    op.sons[1] = val
  else:
    op.sons.add(val)