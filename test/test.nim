#[proc log[T](x:T) {.header:"glue", importc:"log".}

var a = 12.3
proc d*(a:float = 12.0):float =
  var x = [a+12.4, a]
  x[0]

log d()
log a]#
proc log[T](x:T) {.header:"glue", importc:"log".}
proc check[T](x:T) {.header:"glue", importc:"assert".}
var ar  = [0.0'f32,1,2]
var t = 0
while t<3:
  check(t.float32 == ar[t])
  inc t