import wasm

var mymod = newModule()

var ft = newFnType(ValueType.I32,WasmType.Func,ValueType.I32, ValueType.I32)
mymod.types = newTypeSec(ft)

mymod.functions = newFnSec(0)

var ee = newExportEntry("add",ExternalKind.Function,0)
mymod.exports = newExportSec(ee)

var fb = newFnBody(
  newBinaryOp(
    # Add
    ibAdd32,
    newGet(
      # Load param 0
      woGetLocal,
      0
    ),
    newGet(
      # Load param 1
      woGetLocal,
      1
    )
  )
)

mymod.codes = newCodeSec(fb)


writefile("add.wasm",encode(mymod))