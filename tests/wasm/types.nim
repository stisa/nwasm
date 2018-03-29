proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log[T](x:T) {.header:"glue", importc:"log".}

var
  a: int = -2147000000
  b: int8 = -45
  c: int16 = -31000
  d: int32 = 2147000001
  #int64
  
  #f: uint = 4294967290'u
  g: uint8 = 250
  h: uint16 = 34000
  #i: uint32 = 4294967291'u32
  #[uint64]#
  m: float = 3.14
  n: float32 = 1234.56789
  # o: float64

  p: bool = true
  q: char = 'A'
  r: string = "hello"
  r2: string
  #s: cstring = "world"
  #t: pointer = addr(a)

  #u: typedesc = type(q)
log a # -2147000000
log b # -45
log c # -31000
log d # 2147000001
# log e
#log f
log g
log h
#log i
log m # 3.14
log n # 1234.56789
log p # 1
log q # 65
log r # "hello"