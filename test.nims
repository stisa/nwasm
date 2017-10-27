import ospaths
const basePath {.strdefine.} : string = "."
withDir "test":
  for file in listfiles("."):
    if splitfile(file).ext == ".nim" and splitfile(file).name != "test":
      echo "Testing " & $file
      echo "------------------------------------"
      exec basePath / "Nim" / "bin" / "nim_temp.exe" & " wasm -d:wast -r " & file
      