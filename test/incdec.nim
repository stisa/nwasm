proc log[T](x:T) {.header:"glue", importc:"log".}
proc check[T](x:T) {.header:"glue", importc:"assert".}
var c = 1234
var d = succ c
check d == 1235
var e = pred c 
check e == 1233
inc(d)
check d == 1236
dec(e)
check e == 1232
