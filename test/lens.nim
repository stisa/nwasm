proc log[T](x:T) {.header:"glue", importc:"log".}
var c = newSeq[int](3)
c[1] = 123
c[2] = 456
var d : seq[float]
newSeq(d, 4)
d[1] = 12.3
d[2] = 45.6

var e = [1,2,3]

var f = "hell"

log len(c) #3
log len(d) #4
log len(e) #3
log len(f) #4