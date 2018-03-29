proc check[T](x:T) {.header:"glue", importc:"assert".}

type A = object
  b: int
  c: float32

var x = A(c: 1.2)

var y = A(b:13, c: 1.4)

check(x.c - 1.2 < 0.01) # due to imprecision in promoting f32 to f64
check(y.b == 13)