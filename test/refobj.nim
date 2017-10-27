proc check[T](x:T) {.header:"glue", importc:"assert".}

type A = ref object
  id: int
  f: float32

var 
  b : A
  c : A
new b
check(b.id==0)
b.id = 13
check(b.id == 13)
new c
c = b
c.f = 3.14'f32
check(c.id == 13)
check(c.f == 3.14'f32)
