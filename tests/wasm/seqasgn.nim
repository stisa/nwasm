proc log[T](x:T) {.header:"glue", importc:"log".}
proc check[T](x:T) {.header:"glue", importc:"assert".}
var c = newSeq[int](3)
check(c[1] == 0)
c[1] = 123
check(c[1] == 123)