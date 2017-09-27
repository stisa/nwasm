from math import log2,ceil

proc signedLEB128* (value:int32):string =
  # TODO: fix signed leb for value < -128
  var 
    val = value
    # log2 is expensive, could be replace by a lookup table
    more = true
    isNegative = (value < 0)
    b = 0'i32
    size = if isNegative:
        ceil(log2(-1.0*val.float)).int32
      else: ceil(log2(val.float)).int32

  while more:
    # get 7 least significant bits
    b = val and 127
    # left shift value 7 bits
    val = val shr 7
    if isNegative:
      # extend sign
      val = (val or ((not(0) shl (size - 7)))).int32
    # sign bit of byte is second high order bit
    if ((val == 0 and ((b and 0x40) == 0)) or ((val == -1 and ((b and 0x40) == 0x40)))):
        # calculation is complete
        more = false
    else:
        b = b or 0x80
    if result.isnil: result = $chr(b)
    else: add result, chr(b)

proc unsignedLEB128* (value:int32, padding:int=0):string =
  var
    val = value
    b = 0
    pad = padding
  # no padding unless specified
  b = val and 127
  val = val shr 7
  if val != 0 or pad > 0:
    b = b or 128
  result = $chr(b)
  dec pad
  
  while val != 0 or pad > -1:
    b = val and 127
    val = val shr 7
    if val != 0 or padding > 0:
      b = b or 128
    add result, chr(b)
    dec pad

proc toLEB128_S*(val:string):string =
  result = ""
  for el in val:
    add result, el.int32.signedLEB128

proc toLEB128*(val:string):string =
  result = ""
  for el in val:
    add result, el.int32.unsignedLEB128

proc toBytes*(val:SomeNumber):string =
  # Maybe something less unsafe might be a good idea
  result = ""
  result.setLen(sizeof(val))

  copymem(
      (pointer)addr result[0], 
      (pointer)unsafeaddr(val),
      sizeof(val)
  )