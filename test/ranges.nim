proc check[T](x:T) {.header:"glue", importc:"assert".}

var sl = 1..12
check(sl.a == 1)
check(sl.b == 12)
var sl2 = ..4
check(sl2.b == 4)

var ar  = [0.0'f32,1,2]
var b = 1..12
type R = object
  a: range[1..12]
check ar is array
check ar[1] is float32
check 10 in b
check(not(-1 in b))
var r = R(a:3)
check r.a == 3