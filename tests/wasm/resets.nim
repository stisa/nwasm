proc check[T](x:T) {.header:"glue", importc:"assert".}

type A = ref object
  id: int
  f: float32

var 
  b : A
new b
check(b.id == 0)
b.id = 13
b.f = 3.14'f32
check(b.id == 13)
check(b.f == 3.14'f32)
reset b
check(b.id == 0)
check(b.f == 0.0'f32)
