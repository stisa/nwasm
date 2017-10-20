proc log[T](x:T) {.header:"glue", importc:"log".}

var a = 12.3
proc d(a:float):float =
  result = a+12.4

  result = result*result

  result = result/23

  log a

log d(a)