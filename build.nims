import ospaths

const basePath {.strdefine.} : string = ".."
exec "koch" & " temp -d:debug"

withDir "test":
  var file: string = "test"
  if paramCount() == 3: file = paramStr(3)
  file = file.changeFileExt("nim")
  echo "Running " & $file
  echo "------------------------------------"
  exec basePath / "bin" / "nim_temp" & " wasm -d:wast -r " & file
  echo "------------------------------------"
  exec "wasm2wast nimcache/" / file.changeFileExt("wasm")