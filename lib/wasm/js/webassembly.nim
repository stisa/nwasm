import jsffi,promises
export jsffi

type
  Module* {.importcpp.} = ref object of RootObj
  WasmExports* = JsObject
  Instance* {.importcpp.} = ref object of RootObj
    exports*: WasmExports
  ResultObject* {.importcpp.} = ref object of WasmExports
    module*: Module
    instance*: Instance

  ArrayBuffer {.importcpp.} = ref object of WasmExports

proc instantiate*(binary:ArrayBuffer):Promise[ResultObject] {.importcpp: "WebAssembly.instantiate(#)", discardable.}
proc instantiate*(binary:ArrayBuffer,importObj:WasmExports): Promise[ResultObject] {.importcpp: "WebAssembly.instantiate(#,@)".}
proc instantiate*( m:Module,importObj:WasmExports): Promise[ResultObject] {.importcpp: "WebAssembly.instantiate(#,@)".}
proc instantiate*(m:Module): Promise[ResultObject] {.importcpp: "WebAssembly.instantiate(#)"}

proc newModule*(binary:ArrayBuffer): Module {.importcpp: "new WebAssembly.Module(#)".}
proc customSections*(m:Module, sectionname:cstring):seq[cstring] {.importcpp: "WebAssembly.Module.customSections(#,@)".}
proc exports*(m:Module):seq[WasmExports] {.importcpp: "WebAssembly.Module.exports(#)".}
proc imports*(m:Module):seq[WasmExports] {.importcpp: "WebAssembly.Module.imports(#)".}

proc newInstance*(m:Module): Instance {.importcpp: "new WebAssembly.Instance(#)"}
proc newInstance*(m:Module,importObj:WasmExports): Instance {.importcpp: "new WebAssembly.Instance(#,@)"}


# Conveniences
proc instantiate*(binary:string):Promise[ResultObject] {.discardable.} =
  var buffer: ArrayBuffer
  {.emit: "`buffer` = new Uint8Array(`binary`.slice(0,-1));\n".} # THe slice is to discard the '\0' that ends a nim string.
  result = instantiate(buffer)

when ismainmodule:
  import jsconsole
  var code : string = "\0asm\1\0\0\0\1\7\1`\2\1\3\2\1\0\7\7\1\3add\0\0\10\11\1\9\1\2 \0 \1j\11"
    
  var wasmexports : WasmExports

  proc exps(r:ResultObject)= 
    wasmexports= r.instance.exports
    echo cast[int](wasmexports.add(1,2))
  instantiate(code).then(exps)