proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log[T](x:T) {.header:"glue", importc:"log".}
var c = 123
log c