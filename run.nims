import ospaths

withDir "tests"/"wasm":
  var file: string = ""
  if paramCount() == 3: file = paramStr(3)
  else: file = "test.nim"
  echo "Running " & $file
  echo "------------------------------------"
  exec ".." / ".." / "bin" / "nim_temp " & " wasm -d:wast -r " & file
  echo "------------------------------------"
  #exec "wasm2wast nimcache/" / file.changeFileExt("wasm")