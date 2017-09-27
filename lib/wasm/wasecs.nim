import watypes, waenums, waencodes, leb128, wanodes
from math import log2,ceil

#proc pad(s:var string) =
#  doAssert(s.len <= 4)
#  if s.len mod 4 == 0: return # fine
#  else: s.setLen(4)

proc newImportSec*(entries: varargs[ImportEntry]):ImportSection =
  ImportSection(entries: @entries)

proc newFnImportEntry*(module:string,field:string,typeindex:int):ImportEntry = 
  ImportEntry(
    kind:ExternalKind.Function, module:module, 
    field:field, typeindex: typeindex
  )

proc newTableImportEntry*(module:string,field:string,tt:TableType):ImportEntry = 
  ImportEntry(kind:ExternalKind.Table, module:module, field:field, ttype: tt)

proc newMemoryImportEntry*(module:string,field:string,mt:MemoryType):ImportEntry = 
  ImportEntry(kind:ExternalKind.Memory, module:module, field:field, mtype: mt)

proc newGlobalImportEntry*(module:string,field:string,gt:GlobalType):ImportEntry = 
  ImportEntry(kind:ExternalKind.Global, module:module, field:field, gtype: gt)

proc newFnType*(kind:WasmType, params:varargs[ValueType]): FuncType =
  FuncType(
    form: kind,
    params: @params
  )

proc newFnType*(ret:ValueType=ValueType.None,kind:WasmType, 
  params:varargs[ValueType]): FuncType =
  # FIXME: need rt:bool to differentiate return value from varargs
  if ret == ValueType.None:
    FuncType(
      form: kind,
      params: @params
    )
  else:
    FuncType(
      form: kind,
      params: @params,
      returns: @[ret]
    )
    
proc newLocalEntry*(c:int,kind:ValueType):LocalEntry =
  LocalEntry(count: c, vtype: kind)

proc newFnBody*(code:varargs[WasmNode],locals:varargs[LocalEntry]):FunctionBody =
  FunctionBody(locals: @locals, code: @code)

proc newFnSec*(findices:varargs[Natural]):FunctionSection =
  FunctionSection(entries: @findices)
proc newTypeSec*(entries: varargs[FuncType]):TypeSection =
  TypeSection( entries: @entries)

proc newExportEntry*(field:string,kind:ExternalKind,index:int, isImported:bool=false):ExportEntry =
  ExportEntry(field:field, kind:kind, index:index, isImported: isImported)

proc newExportSec*(eentries:varargs[ExportEntry]):ExportSection =
  ExportSection(entries: @eentries)

proc newCodeSec*(fbodies:varargs[FunctionBody]):CodeSection =
  CodeSection( entries: @fbodies )

proc newDataSec*(dseg:varargs[DataSegment]):DataSection =
  DataSection( entries: @dseg )

proc newStartSec*(fidx:Natural):StartSection =
  StartSection(index: fidx)

proc newModule*(magic=0x6d736100, version= 0x00000001):Module=
  new result
  result.magic = magic
  result.version = version

proc newDataSeg*[T](idx: Natural,offs:int32, val:T): DataSegment =
  var dt:bytes
  when T is SomeInteger:
    dt = encode(val)
  elif T is SomeReal:
    dt = encode(val)
  elif T is string:
    dt = val
  else:
    dt = $val
  result = DataSegment(index : idx, data: dt, offset: newConst(offs))

proc add*(es: var ExportSection, ee:varargs[ExportEntry]) =
  if es == nil:
    es = newExportSec(ee)
  else: es.entries.add(@ee)

proc add*(fs: var FunctionSection, typeindx:varargs[Natural]) =
  fs.entries.add(@typeindx)

proc add*(ts: var TypeSection, typ:varargs[FuncType]) =
  if ts == nil:
    ts = newTypeSec(typ)
  else: ts.entries.add(@typ)

proc add*(cs: var CodeSection, fb: varargs[FunctionBody]) =
  if cs == nil:
    cs = newCodeSec(fb)
  else: cs.entries.add(@fb)

proc add*[T](m: var Module, what: varargs[T]) =
  when T is ExportEntry:
    m.exports.add(what)
  elif T is Natural:
    m.functions.add(what)
  elif T is FuncType:
    m.types.add(what)
  elif T is FunctionBody:
    m.codes.add(what)
  elif T is ImportEntry:
    m.imports.entries.add(what)
  elif T is DataSegment:
    m.datas.entries.add(what)
  else:
    assert(false,"invalid what type")
