proc log[T](x:T) {.header:"glue", importc:"log".}

var a = 12.3
proc d*(a:float):float =
  var x = [a+12.4, a]
  x[0]

log d(a)
log a