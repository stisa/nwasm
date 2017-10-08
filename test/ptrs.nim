proc log[T](x:T) {.header:"glue", importc:"log".}

var
  x = 123.34
  py = addr x
  ppy = addr py

log x
log py
log ppy
log py[]
log ppy[]