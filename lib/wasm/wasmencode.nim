import wasmstructure, wasmast, wasmleb128
import sequtils
# This module does a further conversion, from our 
# custom wasm ast to wasm binary.
# Once the ast reaches this module, it has to be complete.
# Note: wasm imports are hoisted in the function index.
# This means that given:
#  import1, import2
#  func1, func2
# The actual function indices are given by (numImports+localFnIndex), eg :
#  import1: 0, import2: 1,
#  func1: 2, func2: 3

proc toBytes*(val:SomeNumber):seq[byte] =
  # Maybe something less unsafe might be a good idea
  result = newSeq[byte](sizeof(val))
  copymem(
      (pointer)addr result[0], 
      (pointer)unsafeaddr(val),
      sizeof(val)
  )

proc toBytes*(s: string): seq[byte] =
  assert(not s.isNil)
  result = newSeq[byte](s.len)
  copymem(
    (pointer)addr result[0], 
    (pointer)unsafeaddr(s[0]),
    s.len
  )


proc encode(vt:WasmValueType): byte =
  case vt:
  of vtI32: result = 0x7f.byte
  of vtI64: result = 0x7e.byte
  of vtF32: result = 0x7d.byte
  of vtF64: result = 0x7c.byte
  of ltPseudo: result = 0x40.byte
  of vtNone: assert(vt!=vtNone)

proc encode(ek:WasmExternalKind): byte =
  case ek:
  of ekFunction: result = 0.byte
  of ekTable: result = 1.byte
  of ekMemory: result = 2.byte
  of ekGlobal: result = 3.byte
  
proc encode(w: WasmOpKind): byte = 
  # This doesn't make a lot of sense.
  # Should be in the enum definition, and this proc just inlines w.byte
  # ControlFlow
  case w:
  of WasmOpKind.woUnreachable : result = 0x00.byte
  of WasmOpKind.woNop : result = 0x01.byte
  of WasmOpKind.woBlock : result = 0x02.byte
  of WasmOpKind.woLoop : result = 0x03.byte
  of WasmOpKind.woIf : result = 0x04.byte
  of WasmOpKind.woElse : result = 0x05.byte
  of WasmOpKind.woEnd : result = 0x0b.byte
  of WasmOpKind.woBr : result = 0x0c.byte
  of WasmOpKind.woBrIf : result = 0x0d.byte
  of WasmOpKind.woBrTable : result = 0x0e.byte
  of WasmOpKind.woReturn : result = 0x0f.byte
  # Call
  of WasmOpKind.woCall : result = 0x10.byte
  of WasmOpKind.woCallIndirect : result = 0x11.byte
  of WasmOpKind.woDrop : result = 0x1a.byte
  of WasmOpKind.woSelect : result = 0x1b.byte
  # Variable access
  #  Local
  of WasmOpKind.woGetLocal : result = 0x20.byte
  of WasmOpKind.woSetLocal : result = 0x21.byte
  of WasmOpKind.woTeeLocal : result = 0x22.byte
  #  Global
  of WasmOpKind.woGetGlobal : result = 0x23.byte
  of WasmOpKind.woSetGlobal : result = 0x24.byte
  # Memory related
  #  Load
  of WasmOpKind.memLoadI32 : result = 0x28.byte
  of WasmOpKind.memLoadI64 : result = 0x29.byte
  of WasmOpKind.memLoadF32 : result = 0x2a.byte
  of WasmOpKind.memLoadF64 : result = 0x2b.byte
  of WasmOpKind.memLoad8S_I32 : result = 0x2c.byte
  of WasmOpKind.memLoad8U_I32 : result = 0x2d.byte
  of WasmOpKind.memLoad16S_I32 : result = 0x2e.byte
  of WasmOpKind.memLoad16U_I32 : result = 0x2f.byte
  of WasmOpKind.memLoad8S_I64 : result = 0x30.byte
  of WasmOpKind.memLoad8U_I64 : result = 0x31.byte
  of WasmOpKind.memLoad16S_I64 : result = 0x32.byte
  of WasmOpKind.memLoad16U_I64 : result = 0x33.byte
  of WasmOpKind.memLoad32S_I64 : result = 0x34.byte
  of WasmOpKind.memLoad32U_I64 : result = 0x35.byte
  #  Store
  of WasmOpKind.memStoreI32 : result = 0x36.byte
  of WasmOpKind.memStoreI64 : result = 0x37.byte
  of WasmOpKind.memStoreF32 : result = 0x38.byte
  of WasmOpKind.memStoreF64 : result = 0x39.byte
  of WasmOpKind.memStore8_I32 : result = 0x3a.byte
  of WasmOpKind.memStore16_I32 : result = 0x3b.byte
  of WasmOpKind.memStore8_I64 : result = 0x3c.byte
  of WasmOpKind.memStore16_I64 : result = 0x3d.byte
  of WasmOpKind.memStore32_I64 : result = 0x3e.byte
  #  Grow, Size
  of WasmOpKind.memSize : result = 0x3f.byte
  # Query memory size
  of WasmOpKind.memGrow : result = 0x40.byte
  # Const
  of WasmOpKind.constI32, constUI32 : result = 0x41.byte
  of WasmOpKind.constI64, constUI64 : result = 0x42.byte
  of WasmOpKind.constF32 : result = 0x43.byte
  of WasmOpKind.constF64 : result = 0x44.byte
  # Conversion
  # cv TO_FROM
  of WasmOpKind.cvWrapI32_I64 : result = 0xa7.byte
  of WasmOpKind.cvTruncI32S_F32 : result = 0xa8.byte
  of WasmOpKind.cvTruncI32U_F32 : result = 0xa9.byte
  of WasmOpKind.cvTruncI32S_F64 : result = 0xaa.byte
  of WasmOpKind.cvTruncI32U_F64 : result = 0xab.byte
  of WasmOpKind.cvExtendI64S_I32 : result = 0xac.byte
  of WasmOpKind.cvExtendI64U_I32 : result = 0xad.byte
  of WasmOpKind.cvTruncI64S_F32 : result = 0xae.byte
  of WasmOpKind.cvTruncI64U_F32 : result = 0xaf.byte
  of WasmOpKind.cvTruncI64S_F64 : result = 0xb0.byte
  of WasmOpKind.cvTruncI64U_F64 : result = 0xb1.byte
  of WasmOpKind.cvConvertF32S_I32 : result = 0xb2.byte
  of WasmOpKind.cvConvertF32U_I32 : result = 0xb3.byte
  of WasmOpKind.cvConvertF32S_I64 : result = 0xb4.byte
  of WasmOpKind.cvConvertF32U_I64 : result = 0xb5.byte
  of WasmOpKind.cvDemoteF32_F64 : result = 0xb6.byte
  of WasmOpKind.cvConvertF64S_I32 : result = 0xb7.byte
  of WasmOpKind.cvConvertF64U_I32 : result = 0xb8.byte
  of WasmOpKind.cvConvertF64S_I64 : result = 0xb9.byte
  of WasmOpKind.cvConvertF64U_I64 : result = 0xba.byte
  of WasmOpKind.cvPromoteF64_F32 : result = 0xbb.byte
  # Reinterpret
  of WasmOpKind.rpReinterpretF32_I32 : result = 0xbc.byte
  of WasmOpKind.rpReinterpretF64_I64 : result = 0xbd.byte
  of WasmOpKind.rpReinterpretI32_F32 : result = 0xbe.byte
  of WasmOpKind.rpReinterpretI64_F64 : result = 0xbf.byte
  # Int
  #  Unary
  of WasmOpKind.iuClz32 : result = 0x67.byte
  of WasmOpKind.iuCtz32 : result = 0x68.byte
  of WasmOpKind.iuPopCnt32 : result = 0x69.byte
  #  Binary
  of WasmOpKind.ibAdd32 : result = 0x6a.byte
  of WasmOpKind.ibSub32 : result = 0x6b.byte
  of WasmOpKind.ibMul32 : result = 0x6c.byte
  of WasmOpKind.ibDivS32 : result = 0x6d.byte
  of WasmOpKind.ibDivU32 : result = 0x6e.byte
  of WasmOpKind.ibRemS32 : result = 0x6f.byte
  of WasmOpKind.ibRemU32 : result = 0x70.byte
  of WasmOpKind.ibAnd32 : result = 0x71.byte
  of WasmOpKind.ibOr32 : result = 0x72.byte
  of WasmOpKind.ibXor32 : result = 0x73.byte
  of WasmOpKind.ibShl32 : result = 0x74.byte
  of WasmOpKind.ibShrS32 : result = 0x75.byte
  of WasmOpKind.ibShrU32 : result = 0x76.byte
  of WasmOpKind.ibRotl32 : result = 0x77.byte
  of WasmOpKind.ibRotr32 : result = 0x78.byte
  #  Test
  of WasmOpKind.itEqz32 : result = 0x45.byte
  #  Relation
  of WasmOpKind.irEq32 : result = 0x46.byte
  of WasmOpKind.irNe32 : result = 0x47.byte
  of WasmOpKind.irLtU32 : result = 0x49.byte
  of WasmOpKind.irLtS32 : result = 0x48.byte
  of WasmOpKind.irGtU32 : result = 0x4b.byte
  of WasmOpKind.irGtS32 : result = 0x4a.byte
  of WasmOpKind.irLeU32 : result = 0x4d.byte
  of WasmOpKind.irLeS32 : result = 0x4c.byte
  of WasmOpKind.irGeU32 : result = 0x4f.byte
  of WasmOpKind.irGeS32 : result = 0x4e.byte
  #  Unary
  of WasmOpKind.iuClz64 : result = 0x79.byte
  of WasmOpKind.iuCtz64 : result = 0x7a.byte
  of WasmOpKind.iuPopCnt64 : result = 0x7b.byte
  #  Binary
  of WasmOpKind.ibAdd64 : result = 0x7c.byte
  of WasmOpKind.ibSub64 : result = 0x7d.byte
  of WasmOpKind.ibMul64 : result = 0x7e.byte
  of WasmOpKind.ibDivS64 : result = 0x7f.byte
  of WasmOpKind.ibDivU64 : result = 0x80.byte
  of WasmOpKind.ibRemS64 : result = 0x81.byte
  of WasmOpKind.ibRemU64 : result = 0x82.byte
  of WasmOpKind.ibAnd64 : result = 0x83.byte
  of WasmOpKind.ibOr64 : result = 0x84.byte
  of WasmOpKind.ibXor64 : result = 0x85.byte
  of WasmOpKind.ibShl64 : result = 0x86.byte
  of WasmOpKind.ibShrS64 : result = 0x87.byte
  of WasmOpKind.ibShrU64 : result = 0x88.byte
  of WasmOpKind.ibRotl64 : result = 0x89.byte
  of WasmOpKind.ibRotr64 : result = 0x8a.byte
  #  Test
  of WasmOpKind.itEqz64 : result = 0x50.byte
  #  Relation
  of WasmOpKind.irEq64 : result = 0x51.byte
  of WasmOpKind.irNe64 : result = 0x52.byte
  of WasmOpKind.irLtU64 : result = 0x54.byte
  of WasmOpKind.irLtS64 : result = 0x53.byte
  of WasmOpKind.irGtU64 : result = 0x56.byte
  of WasmOpKind.irGtS64 : result = 0x55.byte
  of WasmOpKind.irLeU64 : result = 0x58.byte
  of WasmOpKind.irLeS64 : result = 0x57.byte
  of WasmOpKind.irGeU64 : result = 0x5a.byte
  of WasmOpKind.irGeS64 : result = 0x59.byte
  # Float
  #  Unary
  of WasmOpKind.fuNeg32 : result = 0x8c.byte
  of WasmOpKind.fuAbs32 : result = 0x8b.byte
  of WasmOpKind.fuCeil32 : result = 0x8d.byte
  of WasmOpKind.fuFloor32 : result = 0x8e.byte
  of WasmOpKind.fuTrunc32 : result = 0x8f.byte
  of WasmOpKind.fuNearest32 : result = 0x90.byte
  of WasmOpKind.fuSqrt32 : result = 0x91.byte
  #  Binary
  of WasmOpKind.fbAdd32 : result = 0x92.byte
  of WasmOpKind.fbSub32 : result = 0x93.byte
  of WasmOpKind.fbMul32 : result = 0x94.byte
  of WasmOpKind.fbDiv32 : result = 0x95.byte
  of WasmOpKind.fbMin32 : result = 0x96.byte
  of WasmOpKind.fbMax32 : result = 0x97.byte
  of WasmOpKind.fbCopySign32 : result = 0x98.byte
  # Rel
  of WasmOpKind.frEq32 : result = 0x5b.byte
  of WasmOpKind.frNe32 : result = 0x5c.byte
  of WasmOpKind.frLt32 : result = 0x5d.byte
  of WasmOpKind.frGt32 : result = 0x5e.byte
  of WasmOpKind.frLe32 : result = 0x5f.byte
  of WasmOpKind.frGe32 : result = 0x60.byte
  #  Unary
  of WasmOpKind.fuNeg64 : result = 0x99.byte
  of WasmOpKind.fuAbs64 : result = 0x9a.byte
  of WasmOpKind.fuCeil64 : result = 0x9b.byte
  of WasmOpKind.fuFloor64 : result = 0x9c.byte
  of WasmOpKind.fuTrunc64 : result = 0x9d.byte
  of WasmOpKind.fuNearest64 : result = 0x9e.byte
  of WasmOpKind.fuSqrt64 : result = 0x9f.byte
  #  Binary
  of WasmOpKind.fbAdd64 : result = 0xa0.byte
  of WasmOpKind.fbSub64 : result = 0xa1.byte
  of WasmOpKind.fbMul64 : result = 0xa2.byte
  of WasmOpKind.fbDiv64 : result = 0xa3.byte
  of WasmOpKind.fbMin64 : result = 0xa4.byte
  of WasmOpKind.fbMax64 : result = 0xa5.byte
  of WasmOpKind.fbCopySign64 : result = 0xa6.byte
  #  Rel
  of WasmOpKind.frEq64 : result = 0x61.byte
  of WasmOpKind.frNe64 : result = 0x62.byte
  of WasmOpKind.frLt64 : result = 0x63.byte
  of WasmOpKind.frGt64 : result = 0x64.byte
  of WasmOpKind.frLe64 : result = 0x65.byte
  of WasmOpKind.frGe64 : result = 0x66.byte
  of WasmOpKind.opList: doAssert(w!=opList)

var totalImports = 0
proc encode(op: WasmNode): seq[byte] =
  # check operator...
  #echo "# encode opcode", $op.kind
  result = newSeq[byte]()
  #echo "encode is nil: ", op.isNil
  case op.kind:
  of woNop, woUnreachable:
    result = @[encode(op.kind)]
  of UnaryOp:
    result = encode(op.a) & encode(op.kind)
  of BinaryOp:
    result = encode(op.a) & encode(op.b) & encode(op.kind)
  of woCall:
    for o in op.sons:
      add result, encode(o)
    add result, encode(op.kind)
    if not op.isImport:
      op.funcIndex = op.funcIndex + totalImports
    add result, op.funcIndex.int32.unsignedLEB128
  of constI32, constI64:
    result = @[encode(op.kind)]
    result.add(op.intVal.int32.signedLEB128)
  of constUI32, constUI64:
    result = @[encode(op.kind)]
    result.add(op.uintVal.uint32.unsignedLEB128)
  of constF32:
    result = @[encode(op.kind)]
    result.add(op.floatVal.float32.toBytes)
  of constF64:
    result = @[encode(op.kind)]
    result.add(op.floatVal.float64.toBytes)
  of MemLoad, MemStore:
    for o in op.sons:
      if not o.isNil: add result, encode(o)
    add result , encode(op.kind)
    add result, op.align.int32.unsignedLEB128 & op.offset.int32.unsignedLEB128
  of woGetGlobal, woGetLocal:
    add result, encode(op.kind)
    add result, op.index.int32.unsignedLEB128
  of woSetGlobal, woSetLocal, woTeeLocal:
    add result, encode(op.a)    
    add result, encode(op.kind)
    add result, op.index.int32.unsignedLEB128
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
  of opList:
    for o in op.sons:
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
  

proc encode(i: WAsmImport): tuple[t:seq[byte],i:seq[byte]] =
  result = (@[], @[])
  # types part
  result.t.add(0x60.byte) # FIXME: support type kind of other than `func`
  result.t.add(i.typ.params.len.int32.unsignedLeb128)
  if i.typ.params.len>0:
    for par in i.typ.params:
      result.t.add(encode(par))
  if i.typ.res == vtNone:
    result.t.add(0'i32.unsignedLeb128)
  else:
    result.t.add(1'i32.unsignedLeb128 & encode(i.typ.res))
  # imports part
  result.i.add(i.module.len.int32.unsignedLeb128)
  result.i.add(i.module.toBytes)
  result.i.add(i.name.len.int32.unsignedLeb128)
  result.i.add(i.name.toBytes)
  result.i.add(encode(i.kind)) # import kind
  case i.kind:
  of ekFunction: result.i.add(i.id.int32.unsignedLeb128)
  else: assert(i.kind!=ekFunction)


proc encode(imports: seq[WAsmImport]): tuple[num: int, it: seq[byte], i: seq[byte]] =
  result = (imports.len, @[], @[])
  for i in imports:
    let enc = encode(i)
    result.it.add(enc.t)
    result.i.add(enc.i)

proc encode(exports: seq[WasmExport]): tuple[num: int, e: seq[byte]] =
  result = (exports.len, @[])
  for e in exports:
    result.e.add(e.name.len.int32.unsignedLeb128)
    result.e.add(e.name.toBytes)
    result.e.add(encode(e.kind))
    result.e.add(e.id.int32.unsignedLeb128)

proc encode(mem: WasmMemory): tuple[num: int, m: seq[byte]] =
  result = (mem.id, @[])
  if mem.id == 0: return

  result.m.add(0'i32.unsignedLeb128)
  result.m.add(mem.pages.int32.unsignedLeb128)

proc encode(data: seq[WasmData]): tuple[num: int, d: seq[byte]] =
  result = (data.len, @[])
  for d in data:
    # header
    result.d.add(0'i32.unsignedLeb128) # memory in which to store data
    result.d.add(0x41.byte) # i32.const
    result.d.add(d.index.int32.signedLeb128) # i32.literal `d.id`
    result.d.add(0x0b.byte) # end
    result.d.add(d.payload.len.int32.unsignedLeb128) # segment length
    # segment
    result.d.add(d.payload)

proc encode(f: WasmFunction): tuple[t:seq[byte],f:seq[byte]] =
  result = (@[], @[])
  # types part
  result.t.add(0x60.byte)
  result.t.add(f.typ.params.len.int32.unsignedLeb128)
  if f.typ.params.len>0:
    for par in f.typ.params:
      result.t.add(encode(par))
  if f.typ.res == vtNone:
    result.t.add(0'i32.unsignedLeb128)
  else:
    result.t.add(1'i32.unsignedLeb128 & encode(f.typ.res))
  
  # functions (code) part
  var temp = newSeq[byte]()
  # local variable declarations
  if f.locals.isNil:
    temp.add(0'i32.unsignedLeb128)
  else:
    temp.add(f.locals.len.int32.unsignedLeb128)
    for local in f.locals:
      temp.add(1'i32.unsignedLeb128)
      temp.add(encode(local))
  
  temp.add(encode(f.body)) # actual code in the function
  temp.add(0x0b.byte) # end marker

  result.f.add((temp.len).int32.unsignedLeb128) # total length of body and decl
  result.f.add(temp)


proc encode(functions: seq[WasmFunction]): tuple[num: int, ft: seq[byte], f: seq[byte]] =
  result = (functions.len, @[], @[])
  for f in functions:
    let enc = encode(f)
    result.ft.add(enc.t)
    result.f.add(enc.f)

proc encode*(m: WAsmModule): seq[byte] =
  #result = newSeq[byte]()
  # magic=0x6d736100, version= 0x00000001
  # TODO: at least version should be settable, maybe property of WasmModule.
  result = concat(0x6d736100.int32.toBytes, 0x00000001.int32.toBytes)
  let (importNum,importedTypes, imports) = encode(m.imports)
  for i in m.imports:
    if i.exported: m.exports.add(newExport(i.id, i.kind, i.mangledName))
  
  totalImports = importNum # this is needed to correct the calc of woCall indexes
  
  for f in m.functions.mitems: # imports have priority in function index space
    f.hoistedIndex = f.id+importNum
    if f.exported: m.exports.add(newExport(f.hoistedIndex, ekFunction, f.name))
  let (exportNum, exports) = encode(m.exports)
  let (memNum, memory) = encode(m.memory)
  let (dataNum, data) = encode(m.data)
  let (fnNum,fnTypes, functions) = encode(m.functions)

  # 1 TypeSection
  result.add(1'i32.unsignedLeb128) # Type section
  result.add((1+importedTypes.len+fnTypes.len).int32.unsignedLeb128)  # length of type section in bytes
                                                          # +1 because the num of types
  result.add((importNum+fnNum).int32.unsignedLeb128) # num of types
  result.add(importedTypes) # import types data
  result.add(fnTypes) # function types data
  
  # 2 ImportSection
  result.add(2'i32.unsignedLeb128) # Import section
  result.add((1+imports.len).int32.unsignedLeb128)  # length of import section in bytes
                                                    # +1 because the num of imports
  result.add(importNum.int32.unsignedLeb128) # num of import
  result.add(imports) # imports data
  
  # 3 FunctionSection
  # this part just needs to add indexes from importNum to fnNum
  # because types are guaranteed to be ordered (iT1,...,iTn,fT1,...,fTn)
  result.add(3'i32.unsignedLeb128)
  result.add((1+fnNum).int32.unsignedLeb128) # TODO: is this always true?
  result.add(fnNum.int32.unsignedLEB128) # num of (non imported) functions
  for idx in importNum..<(importNum+fnNum):
    result.add(idx.int32.unsignedLeb128)

  # 4 Table

  # 5 MemorySection
  result.add(5'i32.unsignedLeb128)
  result.add((1+memory.len).int32.unsignedLeb128)  
  result.add(memNum.int32.unsignedLeb128)
  result.add(memory)
  
  # 6 GlobalSection
  # TODO:
  
  # 7 ExportSection
  result.add(7'i32.unsignedLeb128) # Export section
  result.add((1+exports.len).int32.unsignedLeb128)  # length of import section in bytes
                                                    # +1 because the num of imports
  result.add(exportNum.int32.unsignedLeb128) # num of import
  result.add(exports) # imports data

  # 8 StartSection

  # 9 ElementSection

  # 10 CodeSection
  result.add(10'i32.unsignedLeb128)
  result.add((1+functions.len).int32.unsignedLeb128)
  result.add(fnNum.int32.unsignedLeb128)
  result.add(functions)

  # 11 DataSection
  result.add(11'i32.unsignedLeb128)
  result.add((1+data.len).int32.unsignedLeb128)
  result.add(dataNum.int32.unsignedLeb128)
  result.add(data)

proc writeTo*(m: seq[byte], name:string) =
  assert(not m.isNil)
  var res = newString(m.len)
  copymem(
    (pointer)addr res[0], 
    (pointer)unsafeaddr(m[0]),
    m.len
  )
  writeFile(name, res)

when false:
  import wasmnode
  var 
    m = newModule("test")
    i = newImport(0,ekFunction, "glue", "log",  newType(vtF32, vtI32), exported=true)
    i2 = newImport(1,ekFunction, "glue", "assert", newType(vtNone, vtI32, vtF32, vtF32))
    #e = newExport(0,ekFunction, "log")
    mem = newMemory(1)
    d = newData(4, ['\0'.byte, 'a'.byte, 's'.byte, 'm'.byte])

    add2 = newBinaryOp(ibAdd32, newGet(woGetLocal, 0), newConst(2'i32))
    andecho = newCall(0, add2, true)
    t = newType(vtI32, vtI32)
    f = newFunction(0, t, andecho, name="add2andecho", exported=true)

  add m.imports, i
  add m.imports, i2
  #add m.exports, e
  m.memory = mem
  add m.data, d
  add m.functions, f
  
  m.encode.writeTo("./test/test.wasm")

#[
  (module
    (type (;0;) (func (param i32) (result f32)))
    (type (;1;) (func (param i32 f32 f32)))
    (type (;2;) (func (param i32) (result i32)))
    (import "glue" "log" (func (;0;) (type 0)))
    (import "glue" "assert" (func (;1;) (type 1)))
    (func (;2;) (type 2) (param i32) (result i32)
      get_local 0
      i32.const 2
      i32.add
      call 0)
    (memory (;0;) 1)
    (export "log" (func 0))
    (export "add2andecho" (func 2))
    (data (i32.const 4) "\00asm")
  )
]#