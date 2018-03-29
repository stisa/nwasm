import ospaths

const basePath {.strdefine.} : string = ".."
withDir "test":
  var file: string = ""
  if paramCount() == 3: file = paramStr(3)
  else: file = "test.nim"
  echo "Running " & $file
  echo "------------------------------------"
  exec basePath / "bin" / "nim_temp " & " wasm -d:wast -r " & file
  echo "------------------------------------"
  exec "wasm2wast nimcache/" / file.changeFileExt("wasm")