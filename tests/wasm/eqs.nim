proc check[T](x:T) {.header:"glue", importc:"assert".}

type
  Enum1 = enum
    Field1 = 3, Field2
  Enum2 = enum
    Place1, Place2 = 3
var
  e1 = Field1
  e2 = Enum1(Place2)
check(e1 == e2) # true

var 
  a = cast[pointer](0)
  b = cast[pointer](nil)
check(a == b) # true

var re1 : ref Enum1
new re1
re1[] = Field1
var re2 : ref Enum1
new re2
re2[] = Enum1(Place2)
check(not(re1==re2)) # false
