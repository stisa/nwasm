proc check[T](x:T) {.header:"glue", importc:"assert".}
var c = newSeq[int](3)
c[1] = 123
c[2] = 456
var d : seq[float]
newSeq(d, 4)
d[1] = 12.3
d[2] = 45.6

var e = [1,2,3]

var f = "hell"

check len(c) == 3
check len(d) == 4
check len(e) == 3
check len(f) == 4