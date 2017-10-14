proc log[T](x:T) {.header:"glue", importc:"log".}
var c = 1234
var d = succ c
log d #== 1235
var e = pred c 
log e #== 1233
inc(d)
log d #== 1236
dec(e)
log e #== 1232