proc check(x:bool) {.header:"glue", importc:"assert".}

var t = true
var w = false
#inc t
check((not t) == false)
check((t and w) == false)
check((t or w) == true)
check((t xor w) == true)