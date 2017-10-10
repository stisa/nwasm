#proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log* [T](x:T) {.used, header:"glue", importc:"log".}

type A = ref object
  id: int
  f: float32
var 
  b : A
new b
log(b.f) # == 0)
b.id = 13

log(b.id) # == 13)
b.f = 3.14'f32
log(b.f) # == 3.14)
reset b
log(b.id) # == 0)
log(b.f) # == 0.0)
