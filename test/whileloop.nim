proc check[T](x:T) {.header:"glue", importc:"assert".}

var ar  = [0.0'f32,1,2]
var t = 0
while t<3:
  check(t.float32 == ar[t])
  inc t