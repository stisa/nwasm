import watypes, waenums, leb128, wanodes
proc encode(s: SecCode): string =
  case s:
  of SecCode.Custom : result =   "\0"
  of SecCode.Type : result =     "\1" # Function signature declarations
  of SecCode.Import : result =   "\2" # Import declarations
  of SecCode.Function : result = "\3" # Function declarations
  of SecCode.Table : result =    "\4" # Indirect function table and other tables
  of SecCode.Memory : result =   "\5" # Memory attributes
  of SecCode.Global : result =   "\6" # Global declarations
  of SecCode.Export : result =   "\7" # Exports
  of SecCode.Start : result =    "\8" # Start function declaration
  of SecCode.Element : result =  "\9" # Elements section
  of SecCode.Code : result =     "\10" # Function bodies (code)
  of SecCode.Data : result =     "\11" # Data segments

proc encode(w: WasmType): string =
  case w:
  of WasmType.Pseudo : result = "\64" #(i.e., the byte 0x40) pseudo type for representing an empty block_type
  of WasmType.Func : result = "\96" #(i.e., the byte 0x60) func
  of WasmType.AnyFunc : result = "\112" #(i.e., the byte 0x70) anyfunc
  of WasmType.F64 : result = "\124" #(i.e., the byte 0x7c) f64
  of WasmType.F32 : result = "\125"  #(i.e., the byte 0x7d) f32
  of WasmType.I64 : result = "\126" #(i.e., the byte 0x7e) i64
  of WasmType.I32 : result = "\127" #(i.e., the byte 0x7f) i32

proc encode(v:ValueType): string =
  case v:
  of ValueType.None: result = "\0"
  of ValueType.F64 : result = "\124"
  of ValueType.F32 : result = "\125" 
  of ValueType.I64 : result = "\126"
  of ValueType.I32 : result = "\127"

proc encode(b:BlockType): string =
  case b:
  of BlockType.Pseudo : result = "\64"
  of BlockType.F64 : result = "\124"
  of BlockType.F32 : result = "\125" 
  of BlockType.I64 : result = "\126"
  of BlockType.I32 : result = "\127"

proc encode(e: ElemType): string =
  case e:
  of ElemType.AnyFunc : result = "\112"

proc encode(e:ExternalKind): string =
  ## A single-byte unsigned integer indicating the kind of definition being imported or defined:
  case e:
  of ExternalKind.Function : result = "\0" ## indicating a Function import or definition
  of ExternalKind.Table : result = "\1" ## indicating a Table import or definition
  of ExternalKind.Memory : result = "\2" ## indicating a Memory import or definition
  of ExternalKind.Global : result = "\3" ## indicating a Global import or definition

proc encode(w: WasmOpKind): string = 
  # ControlFlow
  case w:
  of WasmOpKind.woUnreachable : result = "\0"
  of WasmOpKind.woNop : result = "\1"
  of WasmOpKind.woBlock : result = "\2"
  of WasmOpKind.woLoop : result = "\3"
  of WasmOpKind.woIf : result = "\4"
  of WasmOpKind.woElse : result = "\5"
  of WasmOpKind.woEnd : result = "\11"
  of WasmOpKind.woBr : result = "\12"
  of WasmOpKind.woBrIf : result = "\13"
  of WasmOpKind.woBrTable : result = "\14"
  of WasmOpKind.woReturn : result = "\15"
  # Call
  of WasmOpKind.woCall : result = "\16"
  of WasmOpKind.woCallIndirect : result = "\17"
  of WasmOpKind.woDrop : result = "\26"
  of WasmOpKind.woSelect : result = "\27"
  # Variable access
  #  Local
  of WasmOpKind.woGetLocal : result = "\32"
  of WasmOpKind.woSetLocal : result = "\33"
  of WasmOpKind.woTeeLocal : result = "\34"
  #  Global
  of WasmOpKind.woGetGlobal : result = "\35"
  of WasmOpKind.woSetGlobal : result = "\36"
  # Memory related
  #  Load
  of WasmOpKind.memLoadI32 : result = "\40"
  of WasmOpKind.memLoadI64 : result = "\41"
  of WasmOpKind.memLoadF32 : result = "\42"
  of WasmOpKind.memLoadF64 : result = "\43"
  of WasmOpKind.memLoad8S_I32 : result = "\44"
  of WasmOpKind.memLoad8U_I32 : result = "\45"
  of WasmOpKind.memLoad16S_I32 : result = "\46"
  of WasmOpKind.memLoad16U_I32 : result = "\47"
  of WasmOpKind.memLoad8S_I64 : result = "\48"
  of WasmOpKind.memLoad8U_I64 : result = "\49"
  of WasmOpKind.memLoad16S_I64 : result = "\50"
  of WasmOpKind.memLoad16U_I64 : result = "\51"
  of WasmOpKind.memLoad132S_I64 : result = "\52"
  of WasmOpKind.memLoad32U_I64 : result = "\53"
  #  Store
  of WasmOpKind.memStoreI32 : result = "\54"
  of WasmOpKind.memStoreI64 : result = "\55"
  of WasmOpKind.memStoreF32 : result = "\56"
  of WasmOpKind.memStoreF64 : result = "\57"
  of WasmOpKind.memStore8_I32 : result = "\58"
  of WasmOpKind.memStore16_I32 : result = "\59"
  of WasmOpKind.memStore8_I64 : result = "\60"
  of WasmOpKind.memStore16_I64 : result = "\61"
  of WasmOpKind.memStore32_I64 : result = "\62"
  #  Grow, Size
  of WasmOpKind.memSize : result = "\63"
  # Query memory size
  of WasmOpKind.memGrow : result = "\64"
  # Const
  of WasmOpKind.constI32 : result = "\65"
  of WasmOpKind.constI64 : result = "\66"
  of WasmOpKind.constF32 : result = "\67"
  of WasmOpKind.constF64 : result = "\68"
  # Conversion
  # cv TO_FROM
  of WasmOpKind.cvWrapI32_I64 : result = "\167"
  of WasmOpKind.cvTruncI32S_F32 : result = "\168"
  of WasmOpKind.cvTruncI32U_F32 : result = "\169"
  of WasmOpKind.cvTruncI32S_F64 : result = "\170"
  of WasmOpKind.cvTruncI32U_F64 : result = "\171"
  of WasmOpKind.cvExtendI64S_I32 : result = "\172"
  of WasmOpKind.cvExtendI64U_I32 : result = "\173"
  of WasmOpKind.cvTruncI64S_F32 : result = "\174"
  of WasmOpKind.cvTruncI64U_F32 : result = "\175"
  of WasmOpKind.cvTruncI64S_F64 : result = "\176"
  of WasmOpKind.cvTruncI64U_F64 : result = "\177"
  of WasmOpKind.cvConvertF32S_I32 : result = "\178"
  of WasmOpKind.cvConvertF32U_I32 : result = "\179"
  of WasmOpKind.cvConvertF32S_I64 : result = "\180"
  of WasmOpKind.cvConvertF32U_I64 : result = "\181"
  of WasmOpKind.cvDemoteF32_F64 : result = "\182"
  of WasmOpKind.cvConvertF64S_I32 : result = "\183"
  of WasmOpKind.cvConvertF64U_I32 : result = "\184"
  of WasmOpKind.cvConvertF64S_I64 : result = "\185"
  of WasmOpKind.cvConvertF64U_I64 : result = "\186"
  of WasmOpKind.cvPromoteF64_F32 : result = "\187"
  # Reinterpret
  of WasmOpKind.rpReinterpretF32_I32 : result = "\189"
  of WasmOpKind.rpReinterpretF64_I64 : result = "\190"
  of WasmOpKind.rpReinterpretI32_F32 : result = "\191"
  of WasmOpKind.rpReinterpretI64_F64 : result = "\192"
  # Int
  #  Unary
  of WasmOpKind.iuClz32 : result = "\103"
  of WasmOpKind.iuCtz32 : result = "\104"
  of WasmOpKind.iuPopCnt32 : result = "\105"
  #  Binary
  of WasmOpKind.ibAdd32 : result = "\106"
  of WasmOpKind.ibSub32 : result = "\107"
  of WasmOpKind.ibMul32 : result = "\108"
  of WasmOpKind.ibDivS32 : result = "\109"
  of WasmOpKind.ibDivU32 : result = "\110"
  of WasmOpKind.ibRemS32 : result = "\111"
  of WasmOpKind.ibRemU32 : result = "\112"
  of WasmOpKind.ibAnd32 : result = "\113"
  of WasmOpKind.ibOr32 : result = "\114"
  of WasmOpKind.ibXor32 : result = "\115"
  of WasmOpKind.ibShl32 : result = "\116"
  of WasmOpKind.ibShrS32 : result = "\117"
  of WasmOpKind.ibShrU32 : result = "\118"
  of WasmOpKind.ibRotl32 : result = "\119"
  of WasmOpKind.ibRotr32 : result = "\120"
  #  Test
  of WasmOpKind.itEqz32 : result = "\69"
  #  Relation
  of WasmOpKind.irEq32 : result = "\70"
  of WasmOpKind.irNe32 : result = "\71"
  of WasmOpKind.irLtU32 : result = "\73"
  of WasmOpKind.irLtS32 : result = "\72"
  of WasmOpKind.irGtU32 : result = "\75"
  of WasmOpKind.irGtS32 : result = "\74"
  of WasmOpKind.irLeU32 : result = "\77"
  of WasmOpKind.irLeS32 : result = "\76"
  of WasmOpKind.irGeU32 : result = "\79"
  of WasmOpKind.irGeS32 : result = "\78"
  #  Unary
  of WasmOpKind.iuClz64 : result = "\121"
  of WasmOpKind.iuCtz64 : result = "\122"
  of WasmOpKind.iuPopCnt64 : result = "\123"
  #  Binary
  of WasmOpKind.ibAdd64 : result = "\124"
  of WasmOpKind.ibSub64 : result = "\125"
  of WasmOpKind.ibMul64 : result = "\126"
  of WasmOpKind.ibDivS64 : result = "\127"
  of WasmOpKind.ibDivU64 : result = "\128"
  of WasmOpKind.ibRemS64 : result = "\129"
  of WasmOpKind.ibRemU64 : result = "\130"
  of WasmOpKind.ibAnd64 : result = "\131"
  of WasmOpKind.ibOr64 : result = "\132"
  of WasmOpKind.ibXor64 : result = "\133"
  of WasmOpKind.ibShl64 : result = "\134"
  of WasmOpKind.ibShrS64 : result = "\135"
  of WasmOpKind.ibShrU64 : result = "\136"
  of WasmOpKind.ibRotl64 : result = "\137"
  of WasmOpKind.ibRotr64 : result = "\138"
  #  Test
  of WasmOpKind.itEqz64 : result = "\80"
  #  Relation
  of WasmOpKind.irEq64 : result = "\81"
  of WasmOpKind.irNe64 : result = "\82"
  of WasmOpKind.irLtU64 : result = "\84"
  of WasmOpKind.irLtS64 : result = "\83"
  of WasmOpKind.irGtU64 : result = "\86"
  of WasmOpKind.irGtS64 : result = "\85"
  of WasmOpKind.irLeU64 : result = "\88"
  of WasmOpKind.irLeS64 : result = "\87"
  of WasmOpKind.irGeU64 : result = "\90"
  of WasmOpKind.irGeS64 : result = "\89"
  # Float
  #  Unary
  of WasmOpKind.fuNeg32 : result = "\140"
  of WasmOpKind.fuAbs32 : result = "\139"
  of WasmOpKind.fuCeil32 : result = "\141"
  of WasmOpKind.fuFloor32 : result = "\142"
  of WasmOpKind.fuTrunc32 : result = "\143"
  of WasmOpKind.fuNearest32 : result = "\144"
  of WasmOpKind.fuSqrt32 : result = "\145"
  #  Binary
  of WasmOpKind.fbAdd32 : result = "\146"
  of WasmOpKind.fbSub32 : result = "\147"
  of WasmOpKind.fbMul32 : result = "\148"
  of WasmOpKind.fbDiv32 : result = "\149"
  of WasmOpKind.fbMin32 : result = "\150"
  of WasmOpKind.fbMax32 : result = "\151"
  of WasmOpKind.fbCopySign32 : result = "\152"
  # Rel
  of WasmOpKind.frEq32 : result = "\91"
  of WasmOpKind.frNe32 : result = "\92"
  of WasmOpKind.frLt32 : result = "\93"
  of WasmOpKind.frGt32 : result = "\94"
  of WasmOpKind.frLe32 : result = "\95"
  of WasmOpKind.frGe32 : result = "\96"
  #  Unary
  of WasmOpKind.fuNeg64 : result = "\154"
  of WasmOpKind.fuAbs64 : result = "\153"
  of WasmOpKind.fuCeil64 : result = "\155"
  of WasmOpKind.fuFloor64 : result = "\156"
  of WasmOpKind.fuTrunc64 : result = "\157"
  of WasmOpKind.fuNearest64 : result = "\158"
  of WasmOpKind.fuSqrt64 : result = "\159"
  #  Binary
  of WasmOpKind.fbAdd64 : result = "\160"
  of WasmOpKind.fbSub64 : result = "\161"
  of WasmOpKind.fbMul64 : result = "\162"
  of WasmOpKind.fbDiv64 : result = "\163"
  of WasmOpKind.fbMin64 : result = "\164"
  of WasmOpKind.fbMax64 : result = "\165"
  of WasmOpKind.fbCopySign64 : result = "\166"
  #  Rel
  of WasmOpKind.frEq64 : result = "\97"
  of WasmOpKind.frNe64 : result = "\98"
  of WasmOpKind.frLt64 : result = "\99"
  of WasmOpKind.frGt64 : result = "\100"
  of WasmOpKind.frLe64 : result = "\101"
  of WasmOpKind.frGe64 : result = "\102"
  of WasmOpKind.opGroup: result = "opgroup"

proc encode*(ft:FuncType):string =
  result = encode ft.form
  add result, ft.params.len.int32.unsignedLEB128
  for p in ft.params:
    add result, encode p
  add result, ft.returns.len.int32.unsignedLEB128
  if ft.returns.len>0:
    add result, encode ft.returns[0]

proc encode*(le:LocalEntry):string = le.count.int32.unsignedLEB128 & encode(le.vtype)

proc encode*(x:uint32):string =
  # Little endian.
  # eg 0x6d736100 -> 00 61 73 6d
  result = ""
  result.setLen 4
  when defined(js):
    var 
      bt : uint32= 0
      long = x
    for i in 0..3 :
      bt = long and 0xff
      result[i] = chr(bt)
      long = (long - bt) div 256
    #console.log(result)
  else:
    result[3] = cast[char](x shr 24)
    result[2] = cast[char](x shr 16)
    result[1] = cast[char](x shr  8)
    result[0] = cast[char](x shr  0)
  
proc encode*(x:int32):string =
  # Little endian.
  # eg 0x6d736100 -> 00 61 73 6d
  result = ""
  result.setLen 4
  when defined(js):
    var 
      bt : int32= 0
      long = x
    for i in 0..3 :
      bt = long and 0xff
      result[i] = chr(bt)
      long = (long - bt) div 256
  else:
    result[3] = cast[char](x shr 24)
    result[2] = cast[char](x shr 16)
    result[1] = cast[char](x shr  8)
    result[0] = cast[char](x shr  0)

proc encode*(x:int64):string =
  # Little endian.
  # eg 0x6d736100 -> 00 61 73 6d
  result = ""
  result.setLen 8
  when defined(js):
    var 
      bt : int64= 0
      long = x
    for i in 0..7 :
      bt = long and 0xff
      result[i] = chr(bt)
      long = (long - bt) div 256
  else:
    result[7] = cast[char](x shr 56)
    result[6] = cast[char](x shr 48)
    result[5] = cast[char](x shr 40)
    result[4] = cast[char](x shr 32)
    result[3] = cast[char](x shr 24)
    result[2] = cast[char](x shr 16)
    result[1] = cast[char](x shr  8)
    result[0] = cast[char](x shr  0)

proc encode*(f:float32|float64):string =
  # Maybe something less unsafe might be a good idea
  result = ""
  result.setLen(sizeof(f))
  when not defined js:
    copymem(
        (pointer)addr result[0], 
        (pointer)unsafeaddr(f),
        sizeof(f)
    )
  else:
    # untested
    var 
      aout: array[8,char] # fixme
      farr: array[1,float]
    {.emit: "var farr;".}
    if f is float32:
      {.emit: "farr = new Float32Array(1);".}
    elif f is float64:
      {.emit: "farr = new Float64Array(1);".}
    {.emit:"""
    farr[1] = `f`;
    var `aout` = new Int8Array(farr.buffer);
    """.}
    for i in 0..<result.len:
      result[i] = aout[i]


proc encode(mi: MemoryImmediate): string =
  mi.align.int32.unsignedLEB128 & mi.offset.int32.unsignedLEB128

proc encode*(op: WasmNode): string

var totalImports* : int = 0 # FIXME: we need to know how many imports there are to
                            # recalculate function index (because of hoisting import)

proc encode*(op: WasmNode): string =
  # check operator...
  #echo "# encode opcode",ord op.kind
  result = ""
  #echo "encode is nil: ", op.isNil
  case op.kind:
  of woNop, woUnreachable:
    result = encode(op.kind)
  of UnaryOp:
    result = encode(op.a) & encode(op.kind)
  of BinaryOp:
    result = encode(op.a) & encode(op.b) & encode(op.kind)
  #of UnaryOp:
  #  result = encodeOperand(op.a) & $op.kind
  of woCall:
    for o in op.sons:
      add result, encode(o)
    add result, encode(op.kind)
    #if op.isImport:
    #  add result, op.funcIndex.int32.unsignedLEB128
    #else:
    #  add result, (totalImports+op.funcIndex).int32.unsignedLEB128
    #echo "# BefwoCall ", $op.funcIndex, " isImport:", op.isImport, " totalImp: ", totalImports
    if not op.isImport:
      op.funcIndex = op.funcIndex+totalImports
    add result, op.funcIndex.int32.unsignedLEB128
    #echo "# AftwoCall ", $op.funcIndex, " isImport:", op.isImport, " totalImp: ", totalImports
  of Consts:
    result = encode(op.kind)
    # Consts value is already encoded
    result.add(op.value)
  of MemLoad, MemStore:
    for o in op.sons:
      #assert(not o.isNil)
      if not o.isNil: add result, encode(o)
    add result , encode(op.kind)
    add result, encode(op.memoryImm)
  of woGetGlobal:
    add result, encode(op.kind)
    add result, op.globalIndex.int32.unsignedLEB128
  of woGetLocal:
    add result, encode(op.kind)
    add result, op.localIndex.int32.unsignedLEB128
  of woSetGlobal:
    add result, encode(op.a)    
    add result, encode(op.kind)
    add result, op.globalIndex.int32.unsignedLEB128
  of woSetLocal:
    add result, encode(op.a)    
    add result, encode(op.kind)
    add result, op.localIndex.int32.unsignedLEB128
  of woBlock,woLoop:
    add result, encode(op.kind)
    add result, encode(op.sig)
    for son in op.sons: add result, encode(son)    
  of woBrIf:
    for son in op.sons: add result, encode(son)
    add result, encode(op.kind)
    add result, op.relativeDepth.int32.unsignedLEB128
  of woEnd: add result, encode(op.kind)
  of woBr:
    add result, encode(op.kind)
    add result, op.relativeDepth.int32.unsignedLEB128
  of opGroup:
    #echo "----v----"
    #echo repr op.sons
    for o in op.sons:
      assert(not o.isNil)
      add result, encode(o)
  of woIf:
    add result, encode(op.a)
    add result, encode(op.kind)
    add result, encode(op.sig)
    add result, encode(op.b)
  of woElse:
    add result, encode(op.kind)
    add result, encode(op.a)
  of woReturn:
    add result, encode(op.a)
    add result, encode(op.kind)
  else:
    doAssert(false,"unencoded op " & $op.kind)


proc encode*(fb:FunctionBody):string =
  var 
    temp = "" # fb.locals.len.int32.unsignedLEB128
    localsCount = 0
  for loc in fb.locals:
    if loc.vtype != ValueType.None:
      add temp, encode(loc)
      localsCount += loc.count
      #echo "atLeastAlocal"
  for op in fb.code:
    add temp, encode(op)
  add temp, '\11' # end marker
  # TODO: Rework to avoid allocations?
  let locCountEncoded = localsCount.int32.unsignedLEB128
  result = (locCountEncoded.len+temp.len).int32.signedLEB128
  add result, locCountEncoded
  add result, temp

proc encode*(fs:FunctionSection):string =
  result = encode(SecCode.Function)
  var temp = fs.entries.len.int32.unsignedLEB128
  for i in 0..<fs.entries.len:
    echo "# fsect",i
    # the actual function type index is:
    #   num imports ( because of hoisting... )
    # + actual index
    add temp, (i+totalImports).int32.unsignedLEB128
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(ts:TypeSection):string =
  result = encode(SecCode.Type)
  var temp = ts.entries.len.int32.unsignedLEB128
  for entry in ts.entries:
    add temp, encode(entry)
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(ie:ImportEntry):string = 
  result = ie.module.len.int32.unsignedLEB128
  add result, ie.module
  add result, ie.field.len.int32.unsignedLEB128
  add result, ie.field
  add result, encode(ie.kind)
  #when not defined js:
  #  if ord(ie.kind) == 0:
  #    # C bug?: `add result, $ie.kind` doesn't append if '\0'
  #    add result, "\0"
  case ie.kind:
  of ExternalKind.Function :
    add result, ie.typeindex.int32.unsignedLEB128
  else:
    echo "wrong kind importentry"
  #of ExternalKind.Table:
  #  ttype*: TableType
  #of ExternalKind.Memory:
  #  mtype*: MemoryType
  #of ExternalKind.Global:
  #  gtype*: GlobalType

proc encode* (isec:ImportSection):string =
  result = encode SecCode.Import
  var temp = isec.entries.len.int32.unsignedLEB128
  # We encode the section in temp
  for entry in isec.entries:
    add temp, encode(entry)
  # then we take it's length and finally add the block
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(ee:ExportEntry):string = 
  result = ee.field.len.int32.signedLEB128
  add result, ee.field
  add result, encode ee.kind
  #when not defined js:
  #  if ord(ee.kind) == 0:
  #    # C bug?: add result, $ee.kind doesn't append if '\0'
  #    add result, "\0"
  # FIXME: hoisting imports means we need to calc index
  if not(ee.isImported) and ee.kind == ExternalKind.Function:
    ee.index = ee.index+totalImports
    #add result, (totalImports+ee.index).int32.unsignedLEB128    
  #else:
  add result, ee.index.int32.unsignedLEB128
    
proc encode*(es:ExportSection):string =
  result = encode SecCode.Export
  var temp = es.entries.len.int32.unsignedLEB128
  # We encode the section in temp
  for entry in es.entries:
    add temp, encode(entry)
  # then we take it's length and finally add the block
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(cs:CodeSection):string =
  result = encode SecCode.Code
  var temp = cs.entries.len.int32.unsignedLEB128
  #echo "#codesec\n",repr temp
  for bd in cs.entries:
    #echo "#codesec2\n",repr bd
    add temp, encode(bd)
  #echo "#codesec3\n",repr temp
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode(ll:ResizableLimits):string =
  result = ll.flags.int32.unsignedLEB128
  add result, ll.initial.int32.unsignedLEB128
  if ll.flags == 1:
    add result, ll.maximum.int32.unsignedLEB128

proc encode(tt:TableType):string =
  result = encode tt.elementType
  add result, encode(tt.limits)

proc encode(ts:TableSection):string =
  result = encode SecCode.Table
  var temp = ts.entries.len.int32.unsignedLEB128
  #echo repr temp
  for el in ts.entries:
    add temp, encode(el)
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode(mt:MemoryType):string =
  result = encode(mt.limits)

proc encode(ms:MemorySection):string =
  result = encode SecCode.Memory
  var temp = ms.entries.len.int32.unsignedLEB128
  #echo repr temp
  for el in ms.entries:
    add temp, encode(el)
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode(dt:DataSegment):string =
  result = dt.index.int32.unsignedLEB128
  add result, encode(dt.offset)
  add result, '\11' # 0x0b end marker
  add result, dt.data.len.int32.unsignedLEB128
  add result, dt.data
  
proc encode(ds:DataSection):string =
  result = encode SecCode.Data
  var temp = ds.entries.len.int32.unsignedLEB128
  #echo repr temp
  for el in ds.entries:
    add temp, encode(el)
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(ss:StartSection):string =
  result = encode SecCode.Start
  var temp = ss.index.int32.unsignedLEB128
  add result, temp.len.int32.unsignedLEB128
  add result, temp

proc encode*(m:Module):string = 
  result = encode(m.magic.int32) & encode(m.version.int32)
  if not m.types.isnil and m.types.entries.len>0: #1
    add result, encode(m.types)
  else:
    echo "type sec is nil"
  if not m.imports.isnil and m.imports.entries.len>0: #2
    add result, encode(m.imports)
  else:
    echo "import sec is nil"
  if not m.functions.isnil and m.functions.entries.len>0: #3
    add result, encode(m.functions)
  else:
    echo "func sec is nil"
  if not m.tables.isnil and m.tables.entries.len>0: #4
    add result, encode(m.tables)
  else:
    echo "table sec is nil"
  if not m.memory.isnil and m.memory.entries.len>0: #5
    add result, encode(m.memory)
  else:
    echo "memory sec is nil"
  #if not m.globals.isnil: #6 TODO: when mvp allows exporting mutable global vars
  #  add result, encode(m.globals)
  #else:
  #  echo "global sec is nil"
  if not m.exports.isNil and m.exports.entries.len>0: #7
    add result, encode(m.exports)
  else:
    echo "export sec is nil"
  if not m.start.isnil: #8
    add result, encode(m.start)
  else:
    echo "start sec is nil"
  #if not m.elements.isnil: #9
  #  add result, encode(m.elements)
  #else:
  #  echo "element sec is nil"
  if not m.codes.isnil and m.codes.entries.len>0: #10
    add result, encode(m.codes)
  else:
    echo "code sec is nil"
  if not m.datas.isnil and m.datas.entries.len>0: #11
      add result, encode(m.datas)
  else:
    echo "data sec is nil"
  if not m.custom.isnil: #4
    echo "custom sec is nil"
  
  