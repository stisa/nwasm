#proc log(x:string) {.header:"glue", importc:"rawEcho".}
proc log* [T](x:T) {.used, header:"glue", importc:"log".}

type A = enum
  aa, ab, ac

var 
  a = ab
  b = aa
log(a == b)
log(a == ab)

var
  pa = addr a
  pb = addr b
log(pa == pb)

var
  sa = "hello"
  sb = "world"
log(sa == sb)

var
  ca = 'x'
  cb = 'y'
log(ca == cb)

var
  ba = true
  bb = false
log(ba == bb)

#[
var 
  ta = {aa,ab}
  tb = {ac}
log(ta == tb)
]#

var
  ra : ref int
  rb : ref int
new ra
new rb
log(ra == rb)
ra = rb
log(ra == rb)

#[Mising: EqProc]#

log(a <= b)
log(a <= ab)

log(pa <= pb)

log(sa <= sb)

log(ca <= cb)

log(ba <= bb)

#[
var 
ta = {aa,ab}
tb = {ac}
log(ta == tb)
]#

new ra
new rb
log(ra <= rb)
ra = rb
log(ra <= rb)