from math import log2,ceil

proc signedLEB128* (value:int32|uint32):seq[byte] =
  # TODO: fix signed leb for value < -128
  var 
    val: int32 = value
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
      val = (val or ((not(0'i32) shl (size - 7'i32)))).int32
    # sign bit of byte is second high order bit
    if ((val == 0 and ((b and 0x40) == 0)) or ((val == -1 and ((b and 0x40) == 0x40)))):
        # calculation is complete
        more = false
    else:
        b = b or 0x80
    if result.isnil: result = @[b.byte]
    else: add result, b.byte

proc unsignedLEB128* [T:int32|uint32](value: T, padding:int=0):seq[byte] =
  var
    val = value
    b = 0.T
    pad = padding
  # no padding unless specified
  b = val and 127
  val = val shr 7
  if val != 0 or pad > 0:
    b = b or 128
  result = @[b.byte]
  dec pad
  
  while val != 0 or pad > -1:
    b = val and 127
    val = val shr 7
    if val != 0 or padding > 0:
      b = b or 128
    add result, b.byte
    dec pad
