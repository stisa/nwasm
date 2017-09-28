proc log[T](x:T) {.header:"glue", importc:"log".}

var c = newSeq[int](3)
log c[1]
c[1] = 12
log c[1]