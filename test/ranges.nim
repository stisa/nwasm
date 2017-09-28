proc check[T](x:T) {.header:"glue", importc:"assert".}

var sl = 1..12
check(sl.a == 1)
check(sl.b == 12)
var sl2 = ..4
check(sl2.b == 4)