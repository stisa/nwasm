import wasmstructure, wasmast
import strutils

# Render our custom wasm ast to a json-like readable format.
# This is a naive implementation with lots of string copying, so it's slow.

proc render*(n: WasmNode, indlv = 0): string

proc render(v: WasmValueType): string = "\"" & ($v)[2..^1] & "\""
proc render(v: seq[WasmValueType]): string =
  if v.isNil: return "\"None\""
  result = ""
  for i, p in v:
    if i == v.len-1:
      result.add(render p)
    else:
      result.add(render(p) & ", ")

proc render(v: WasmExternalKind): string = "\"" & ($v)[2..^1] & "\""
proc render(bytes: seq[byte],hex:bool = false): string =
  if bytes.isNil: return "[]"
  result = ""
  if hex:
    for b in countdown(bytes.len-1,0,1):
      add result,  bytes[b].toHex
  else:
    for b in countdown(bytes.len-1,0,1):
      add result,  bytes[b].char
    result = result.escape
  
proc render(t: WasmType, indlv = 0): string =
  var 
    pars: string = ""
    res: string = render t.res
  for i, p in t.params:
    if i == t.params.len-1:
      pars.add(render p)
    else:
      pars.add(render(p) & ", ")

  result = """{ "params": $1, "result": $2 }""" % [pars, res]
  result = result.indent(indlv)

proc render*(sn: seq[WasmNode], indlv = 0): string =
  if sn.isNil or sn.len == 0: return "[]"
  var sons = ""

  for i, s in sn:
    add sons, render(s,indlv)
    if i != sn.len-1: add sons, ",\n"

  result = """[
$1
  ]""" % sons
  


proc render*(n: WasmNode, indlv = 0): string =
  case n.kind:
  of woBlock, woLoop, woIf: 
    #sig*: WasmValueType # vtNone === Pseudo
    result = """{
  "opcode": $1, 
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  of woBr, woBrIf:
    #relativeDepth*: Natural
    result = """{
  "opcode": $1, 
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  of woBrTable:
    #targetTable*: seq[Natural]
    #default*: Natural
    result = """{
  "opcode": $1, 
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  of woCall:
    result = """{
  "opcode": $1,
  "fnIndex": $3, "imported": $4
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv), $n.funcIndex, $n.isImport]
    #funcIndex* : Natural
    #isImport* : bool
  of woCallIndirect:
    #typeIndex*: Natural
    #reserved*: Natural # 0 in MVP
    result = """{
  "opcode": $1, 
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  of woGetLocal, woSetLocal, woTeeLocal, woGetGlobal, woSetGlobal:
    result = """{
  "opcode": $1,
  "index": $3,
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv), $n.index]
    #index*: Natural
  of MemLoad, MemStore:
    #align*: Natural # log2(alignment)
    #offset*: Natural
    result = """{
  "opcode": $1,
  "align": $3, "offset": $4,
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv), $n.align, $n.offset]
  of memSize, memGrow: 
    #memReserved*: Natural
    result = """{
  "opcode": $1,
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  of constI32, constI64:
    result = """{
  "opcode": $1,
  "value": $3,
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv), $n.intVal]
  of constUI32, constUI64:
    result = """{
  "opcode": $1,
  "value": $3,
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv), $n.uintVal]
  of constF32, constF64:
    result = """{
  "opcode": $1,
  "value": $3,
  "sons": $2
  }""" % [$n.kind, render(n.sons, indlv), $n.floatVal]
  else:
    result = """{
  "opcode": $1, 
  "sons": $2
}""" % [$n.kind, render(n.sons, indlv)]
  result = result.indent(indlv)

proc render*(i: WAsmImport, indlv = 0): string =
  result = """{
  "module": "$1", "name": "$2",
  "id": $3, "kind": $4, "exported": $6,
  "type": $5
}""" % [i.module, i.name, $i.id, render(i.kind), render(i.typ), $i.exported]
  result = result.indent(indlv)

proc render*(e: WAsmExport, indlv = 0): string =
  result = """{
  name": "$1",
  "id": $2, "kind": $3
}""" % [e.name, $e.id, render(e.kind)]
  result = result.indent(indlv)

proc render*(m: WAsmMemory, indlv = 0): string =
  result = """{ "id": $1, "pages": $2 }""" % [$m.id, $m.pages]
  result = result.indent(indlv)

proc render*(d: WAsmData, indlv = 0): string =
  result = """{ "index": $1, "payload": "$2" }""" % [$d.index, render(d.payload)]
  result = result.indent(indlv)

proc render*(f: WAsmFunction, indlv = 0): string =
  result = """{
  "name": "$1", "id": $2, "hoistedIndex": $6,
  "type": $3, "locals": [$4], "exported": $7,
  "body": [
$5
  ]
}""" % [f.name, $f.id, render(f.typ), render(f.locals), 
  render(f.body, indlv), $f.hoistedIndex, $f.exported]

  result = result.indent(indlv)

proc render*(m: WAsmModule, indlv = 0): string = 
  var
    imports: string = ""
    functions: string = ""
    exports: string = ""
    memory: string = ""
    data: string = ""
  for i, im in m.imports:
    add imports, render(im, indlv+4)
    if i != m.imports.len-1: add imports, ",\n"

  for i, e in m.exports:
    add exports, render(e, indlv+4)
    if i != m.exports.len-1: add exports, ",\n"

  add memory, render(m.memory, indlv)
  for i, d in m.data:
    add data, render(d,indlv+4)
    if i != m.data.len-1: add data, ",\n"
  for i, f in m.functions:
    add functions, render(f,indlv+4)
    if i != m.functions.len-1: add functions, ",\n"
  result = """
{ 
  "module": "$1",
  "imports": [
$2
  ],
  "functions": [
$3
  ],
  "data": [
$4
  ],
  "memory": $5,
  "exports": [
$6
  ]
}""" % [m.name, imports, functions, data, memory, exports]
  result = result.indent(indlv)

when isMainModule:
  import wanodes
  var 
    m = newModule("test")
    i = newImport(0,ekFunction, "glue", "log", newType(vtNone, vtI32, vtF32, vtF32))
    i2 = newImport(0,ekFunction, "glue", "assert", newType(vtNone, vtI32, vtF32, vtF32))
    e = newExport(0,ekFunction, "log")
    mem = newMemory(0)
    d = newData(4, ['\0'.byte, 'a'.byte, 's'.byte, 'm'.byte])

    add2 = newBinaryOp(ibAdd32, newGet(woGetLocal, 0), newConst(2'i32))
    andecho = newCall(0, add2, true)
    t = newType(vtI32, vtI32)
    f = newFunction(0, t, andecho, name="add2andecho")
  echo render i
  echo render e
  echo render mem
  echo render d
  echo render f
  add m.imports, i
  add m.imports, i2
  add m.exports, e
  m.memory = mem
  add m.data, d
  add m.functions, f
  var j = render m
  echo j
  