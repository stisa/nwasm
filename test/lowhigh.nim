proc check[T](x:T) {.header:"glue", importc:"assert".}

type A = ref object
  id: int
  f: float32

var 
  b : A
new b
check(high(b.id) == 2147483647)
check(low(b.id) == -2147483648 )

var ar  = [1.0'f32,2,3]

check(low(ar)==0) # 0
check(high(ar)==2) # 2

check(ar[low(ar)] == 1.0) # 1.0
check(ar[high(ar)] == 3.0) # 3.0