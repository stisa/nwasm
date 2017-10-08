#proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log[T](x:T) {.header:"glue", importc:"log".}

type A = object
  b: int
  c: float32

var x = A(c: 1.2)

var y = A(b:13, c: 1.4)
var z = addr x
log(x.c) # == 1.2)
log(y.b) # == 13)
log(z.c)
#[
type A = ref object
  id: int
  f: float32

var 
  b : A
new b
log(b.id) # == 0)
b.id = 13
b.f = 3.14'f32
log(b.id) # == 13)
log(b.f) # == 3.14)
reset b
log(b.id) # == 0)
log(b.f) # == 0.0)
]#