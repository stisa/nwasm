proc check[T](x:T) {.header:"glue", importc:"assert".}
type A = enum
  aa, ab, ac

var 
  a = ab
  b = aa
check(not(a <= b))
check(a <= ab)

var
  pa = addr a
  pb = addr b
check(pa <= pb)

var
  sa = "hello"
  sb = "world"
check(sa <= sb)

var
  ca = 'x'
  cb = 'y'
check(ca <= cb)

var
  ba = true
  bb = false
check(not(ba <= bb))

var 
  ta = {aa,ab}
  tb = {ac}
check(not(ta == tb))

var
  ra : ref int
  rb : ref int
new ra
new rb
check(ra <= rb)
ra = rb
check(not(ra < rb))

#[Mising: EqProc]#
type R = object
  a: range[1..12]

var 
  ar  = [0.0'f32,1,2]
  r = R(a:2)
check(cmp(r.a, high(ar))==0)