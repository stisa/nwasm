type 
  SecCode* {.pure.} = enum
    Custom
    Type # Function signature declarations
    Import # Import declarations
    Function # Function declarations
    Table # Indirect function table and other tables
    Memory # Memory attributes
    Global # Global declarations
    Export # Exports
    Start # Start function declaration
    Element # Elements section
    Code # Function bodies (code)
    Data # Data segments

  WasmType* {.pure.} = enum
    Pseudo #(i.e., the byte 0x40) pseudo type for representing an empty block_type
    Func #(i.e., the byte 0x60) func
    AnyFunc #(i.e., the byte 0x70) anyfunc
    F64 #(i.e., the byte 0x7c) f64
    F32  #(i.e., the byte 0x7d) f32
    I64 #(i.e., the byte 0x7e) i64
    I32 #(i.e., the byte 0x7f) i32

  ValueType* {.pure.} = enum
    None
    F64
    F32
    I64
    I32

  BlockType* {.pure.} = enum
    Pseudo
    F64
    F32
    I64
    I32

  ElemType* {.pure.} = enum
    AnyFunc = -0x10

  ExternalKind *{.pure.} = enum
    ## A single-byte unsigned integer indicating the kind of definition being imported or defined:
    Function ## indicating a Function import or definition
    Table  ## indicating a Table import or definition
    Memory ## indicating a Memory import or definition
    Global ## indicating a Global import or definition

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
    memLoad132S_I64, memLoad32U_I64,
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

    opGroup # this is used to make a group of ops, like those required for storing a string of len>4bytes
            # it should be ignored by the encoder.
const 
  MemLoad* = {
    memLoadI32, memLoadI64, memLoadF32, memLoadF64,
    memLoad8S_I32, memLoad8U_I32, memLoad16S_I32, memLoad16U_I32,
    memLoad8S_I64, memLoad8U_I64, memLoad16S_I64, memLoad16U_I64,
    memLoad132S_I64, memLoad32U_I64
  }
#  Store
  MemStore* = {
    memStoreI32, memStoreI64, memStoreF32, memStoreF64,
    memStore8_I32, memStore16_I32, memStore8_I64, memStore16_I64,
    memStore32_I64
  }
  Consts* = {constI32, constI64, constF32, constF64}

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