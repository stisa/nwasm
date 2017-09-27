import watypes, waenums, wanodes, strutils

proc render(s: SecCode): string {.inline.} = "{ \"$1\": " % $s

proc render(w: WasmType): string {.inline.} = "\"" & $w & "\""

proc render(v:ValueType): string {.inline.} = $v

proc render(b:BlockType): string {.inline.} = "\"" & $b & "\""

proc render(e: ElemType): string {.inline.} = "\"" & $e & "\""

proc render(e:ExternalKind): string {.inline.} = "\"" & $e & "\""

proc render(w: WasmOpKind): string {.inline.} = "\"" & $w & "\""

proc render*(ft:FuncType):string =
  var params: string = if ft.params.isNil or ft.params.len == 0: "\"None\"" else: ""
  for p in ft.params:
    add params, "\"" & $ft.params.len & "x" & render(p) & "\""
  let returns = if ft.returns.len>0:
      render(ft.returns[0])
    else: "\"None\""
  
  result = """
  {
    "kind": $1,
    "params": $2,
    "return": $3
  },
""" % [render(ft.form), params, returns]

proc render*(le:LocalEntry):string =
  "\"count\": $1, \"type\": $2\n" % [$le.count, render(le.vtype)]

proc render*(x:SomeNumber):string {.inline.} = $x

proc render(mi: MemoryImmediate): string =
  "\"align\": $1, \"offset\": $2" % [$mi.align, $mi.offset]

proc render*(op: WasmNode): string

var totalImports* : int = 0 # FIXME: we need to know how many imports there are to
                            # recalculate function index (because of hoisting import)

proc render*(op: WasmNode): string =
  result = ""
  case op.kind:
  of woNop, woUnreachable:
    result = render(op.kind)
  of UnaryOp:
    result = "{\"kind\": $1, \"arg\": $2}" % [render(op.kind), render(op.a)]
  of BinaryOp:
    result = "{\"kind\": $1, \"args\": [$2, $3]}" % [render(op.kind), render(op.a), render(op.b)]
  of woCall:
    var tmp = ""
    for o in op.sons:
      add tmp, render(o) & ","
    if not op.isImport:
      op.funcIndex = op.funcIndex+totalImports
    result = "{\"kind\": $1, \"idx\": $3, \"args\": [$2]}" % [
      render(op.kind), tmp, $op.funcIndex
    ]
    
  of Consts:
    result = "{\"kind\": $1, \"value\": $2}" % [render(op.kind), op.value.escape]
  of MemLoad, MemStore:
    var tmp = ""
    for o in op.sons:
      if not o.isNil: add tmp, render(o)
    result = """{"kind": $1, "what": $2, "memImm": {$3}}""" % [
      render(op.kind), tmp, render(op.memoryImm)
    ]
  of woGetGlobal:
    add result, render(op.kind)
    add result, $op.globalIndex
  of woGetLocal:
    add result, render(op.kind)
    add result, $op.localIndex
  of woSetGlobal:
    add result, render(op.a)    
    add result, render(op.kind)
    add result, $op.globalIndex
  of woSetLocal:
    add result, render(op.a)    
    add result, render(op.kind)
    add result, $op.localIndex
  of woBlock,woLoop:
    add result, render(op.kind)
    add result, render(op.sig)
    for son in op.sons: add result, render(son)    
  of woBrIf:
    for son in op.sons: add result, render(son)
    add result, render(op.kind)
    add result, $op.relativeDepth
  of woEnd: add result, render(op.kind)
  of woBr:
    add result, render(op.kind)
    add result, $op.relativeDepth
  of opGroup:
    for o in op.sons:
      assert(not o.isNil)
      add result, render(o)
  of woIf:
    add result, render(op.a)
    add result, render(op.kind)
    add result, render(op.sig)
    add result, render(op.b)
  of woElse:
    add result, render(op.kind)
    add result, render(op.a)
  of woReturn:
    add result, render(op.a)
    add result, render(op.kind)
  else:
    doAssert(false,"unrenderd op " & $op.kind)

proc render*(fb:FunctionBody):string =
  result = ""
  for loc in fb.locals:
    if loc.vtype != ValueType.None:
      add result, "\n  " & render(loc)
  for op in fb.code:
    add result, "  " & render(op)

proc render*(fs:FunctionSection):string =
  result = render(SecCode.Function)
  for i in fs.entries:
    add result, " " & $i & "," 
  add result, "},\n"
  
proc render*(ts:TypeSection):string =
  result = render(SecCode.Type)
  add result, "[\n"
  for entry in ts.entries:
    add result, render(entry)
  add result, "  ]\n},\n"
proc render*(ie:ImportEntry):string = 
  result = """
  {
    "module": "$1",
    "name": "$2",
    "kind": $3,  
""" % [ie.module, ie.field, render(ie.kind)]
  case ie.kind:
  of ExternalKind.Function :
    add result, "    \"importId\": $1\n  }," % $ie.typeindex
  else:
    doAssert(false,"TODO: implement other kinds for importentry")
  #of ExternalKind.Table:
  #  ttype*: TableType
  #of ExternalKind.Memory:
  #  mtype*: MemoryType
  #of ExternalKind.Global:
  #  gtype*: GlobalType
  add result, "\n"

proc render* (isec:ImportSection):string =
  result = render SecCode.Import
  add result, "[\n"
  for entry in isec.entries:
    add result, render(entry)
  add result, "  ]\n},\n"

proc render*(ee:ExportEntry):string = 
  if not(ee.isImported) and ee.kind == ExternalKind.Function:
    ee.index = ee.index+totalImports
  result = """
  {
    "field": "$1",
    "kind": $2,
    "exportId": $3
  },
""" % [ee.field, render(ee.kind), $ee.index]

proc render*(es:ExportSection):string =
  result = render(SecCode.Export) & "[\n"
  for entry in es.entries:
    add result, render(entry)
  add result, "  ]\n},\n"

proc render*(cs:CodeSection):string =
  result = render(SecCode.Code) & "[\n"
  for bd in cs.entries:
    add result, render(bd)
  add result, "\n  ]\n},\n"

proc render(ll:ResizableLimits):string =
  result = """  {
    "hasMax": $1,
    "initial": $1
  },
""" %  [$ll.flags, $ll.initial]
  if ll.flags == 1:
    add result, $ll.maximum

proc render(tt:TableType):string =
  result = render tt.elementType
  add result, render(tt.limits)

proc render(ts:TableSection):string =
  result = render SecCode.Table
  for el in ts.entries:
    add result, render(el)

proc render(mt:MemoryType):string =
  result = render(mt.limits)

proc render(ms:MemorySection):string =
  result = render(SecCode.Memory) & "[\n"
  for el in ms.entries:
    add result, render(el)
  add result, "  ]\n},\n"

proc render(dt:DataSegment):string =
  result = """
  {
    "idx": $1,
    "offs": $2,
    "data": $3
  },
""" % [ $dt.index, render(dt.offset), dt.data.escape]

proc render(ds:DataSection):string =
  result = render(SecCode.Data) & "[\n"
  for el in ds.entries:
    add result, render(el)
  add result, "  ]\n}\n"

proc render*(ss:StartSection):string =
  result = render SecCode.Start
  add result, $ss.index
  add result, "\n"

proc render*(m:Module):string = 
  result = "{\"Module\":[\n"# & m.magic.toHex() & m.version.toHex() & "\n"
  if not m.types.isnil and m.types.entries.len>0: #1
    add result, render(m.types)
  else:
    echo "type sec is nil"
  if not m.imports.isnil and m.imports.entries.len>0: #2
    add result, render(m.imports)
  else:
    echo "import sec is nil"
  if not m.functions.isnil and m.functions.entries.len>0: #3
    add result, render(m.functions)
  else:
    echo "func sec is nil"
  if not m.tables.isnil and m.tables.entries.len>0: #4
    add result, render(m.tables)
  else:
    echo "table sec is nil"
  if not m.memory.isnil and m.memory.entries.len>0: #5
    add result, render(m.memory)
  else:
    echo "memory sec is nil"
  #if not m.globals.isnil: #6 TODO: when mvp allows exporting mutable global vars
  #  add result, render(m.globals)
  #else:
  #  echo "global sec is nil"
  if not m.exports.isNil and m.exports.entries.len>0: #7
    add result, render(m.exports)
  else:
    echo "export sec is nil"
  if not m.start.isnil: #8
    add result, render(m.start)
  else:
    echo "start sec is nil"
  #if not m.elements.isnil: #9
  #  add result, render(m.elements)
  #else:
  #  echo "element sec is nil"
  if not m.codes.isnil and m.codes.entries.len>0: #10
    add result, render(m.codes)
  else:
    echo "code sec is nil"
  if not m.datas.isnil and m.datas.entries.len>0: #11
      add result, render(m.datas)
  else:
    echo "data sec is nil"
  if not m.custom.isnil: #4
    echo "TODO: implement custom section"
  else:
    echo "custom sec is nil"
  add result, "]}"
  