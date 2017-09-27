proc log[T](x:T) {.header:"glue", importc:"console".}

var c = 123

log c
