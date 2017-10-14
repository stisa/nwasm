#proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log* [T](x:T) {.used, header:"glue", importc:"log".}

type A = enum
  aa, ab, ac

var 
  a = aa
  b = a < ab
log b