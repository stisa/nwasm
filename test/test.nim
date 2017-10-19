
proc log[T](x:T) {.header:"glue", importc:"log".}
var c = newSeq[int](3)
log(c[1]) # == 0)
c[1] = 123
c[2] = 456
log(c[1]) # == 123)
var d : seq[float]
newSeq(d, 3)
log(d[1]) # == 0)
d[1] = 12.3
d[2] = 45.6
log(d[1]) # == 123)
log c[2]