import
  ../Nim/compiler/[ast, astalgo, types, msgs, wordrecg, trees, ropes]

from wasmast import WasmValueType

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
  of tyBool,tyInt,tyInt32,tyUInt32,tyUInt,tyUInt8,tyInt16,
    tyString, tyPointer, tySequence, tyArray, tyProc,
    tyOrdinal, tyVar, tyOpenArray, tyObject, tyChar:
    result = vtI32
  of tyFloat32:
    result = vtF32
  of tyFloat, tyFloat64:
    result = vtF64
  else:
    internalError("unmapped type kind " & $t.kind)

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
