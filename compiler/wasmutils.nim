import
  ../Nim/compiler/[ast, astalgo, types, sighashes, msgs, wordrecg, trees, ropes]

import md5

from wasmast import WasmValueType, WasmOpKind

from strutils import toHex, Digits

proc getPragmaStmt*(n: PNode, w: TSpecialWord): PNode =
  case n.kind
  of nkStmtList:
    for i in 0 .. < n.len:
      result = getPragmaStmt(n[i], w)
      if result != nil: break
  of nkPragma:
    for i in 0 .. < n.len:
      if whichPragma(n[i]) == w: return n[i]
  else: discard

proc stmtsContainPragma*(n: PNode, w: TSpecialWord): bool =
  result = getPragmaStmt(n, w) != nil

proc mapType*(tt:PType):WasmValueType =
  let t = if not tt.isNil: tt.skipTypes(abstractVarRange) else: tt
  if t.isNil: return vtNone
  case t.kind:
  #of tyBool,tyInt,tyInt32,tyUInt32,tyUInt,tyUInt8,tyInt16,
  #  tyString, tyPointer, tySequence, tyArray, tyProc,
  #  tyOrdinal, tyVar, tyOpenArray, tyObject, tyChar:
  of tyBool,tyChar, tyInt..tyInt32, tyUint..tyUint32,
    tyString, tyPtr, tyRef, tyPointer, tyObject, tySet,
    tySequence, tyEnum:
    result = vtI32
  of tyFloat32:
    result = vtF32
  of tyFloat, tyFloat64:
    result = vtF64
  else:
    internalError("unmapped type kind " & $t.kind)

proc mapStoreKind*(tt:PType): WasmOpKind =
  case mapType(tt):
  of vtI32: result = memStoreI32
  of vtI64: result = memStoreI32 # no 64 bit in wasm
  of vtF32: result = memStoreF32
  of vtF64: result = memStoreF64
  else:
    internalError("unmapped store for type: " & $tt.kind)

proc mapLoadKind*(tt:PType): WasmOpKind =
  case mapType(tt):
  of vtI32: result = memLoadI32
  of vtI64: result = memLoadI32
  of vtF32: result = memLoadF32
  of vtF64: result = memLoadF64
  else:
    internalError("unmapped load " & $mapType(tt) & " for type: " & $tt.kind)
    


proc alignTo4*(n:Natural): Natural = (n + 3) and not(0x03)
  # Next multiple of 4 or return n if n is already a multiple of 4

proc mangle*(name: string): string =
  result = newStringOfCap(name.len)
  var start = 0
  if name[0] in Digits:
    result.add("X" & name[0])
    start = 1
  var requiresUnderscore = false
  template special(x) =
    result.add x
    requiresUnderscore = true
  for i in start..(name.len-1):
    let c = name[i]
    case c
    of 'a'..'z', '0'..'9', 'A'..'Z':
      add(result, c)
    of '_':
      # we generate names like 'foo_9' for scope disambiguations and so
      # disallow this here:
      if i > 0 and i < name.len-1 and name[i+1] in Digits:
        discard
      else:
        add(result, c)
    of '$': special "dollar"
    of '%': special "percent"
    of '&': special "amp"
    of '^': special "roof"
    of '!': special "emark"
    of '?': special "qmark"
    of '*': special "star"
    of '+': special "plus"
    of '-': special "minus"
    of '/': special "slash"
    of '=': special "eq"
    of '<': special "lt"
    of '>': special "gt"
    of '~': special "tilde"
    of ':': special "colon"
    of '.': special "dot"
    of '@': special "at"
    of '|': special "bar"
    else:
      add(result, "X" & toHex(ord(c), 2))
      requiresUnderscore = true
  if requiresUnderscore:
    result.add "_"

proc mangleName*(s:PSym):string = 
  echo "# mangleName ", s.name.s, " " , s.kind # ,"\n",typeToyaml s.typ
  result = (s.name.s & $s.hashProc).mangle
  #result = (s.name.s & "_" & s.typ.typeToString).mangle
  #[case s.kind:
  of skType:
    result = s.name.s.mangle & $s.typ.kind
  of skProc:
    result = s.name.s.mangle
    for s in s.typ.n:
  else:
    result = s.name.s.mangle
    for tson in s.typ.sons:
      if not tson.isNil:
        result.add("_" & $tson.kind)]#