proc log(x:bool) {.header:"glue", importc:"log".}
var 
  i = 30
  t = i<=10
  x = i == 30
  b = true
log t
log x
log b