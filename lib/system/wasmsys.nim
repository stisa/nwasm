#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The compiler depends on the System module to work properly and the System
## module depends on the compiler. Most of the routines listed here use
## special compiler magic.
## Each module implicitly imports the System module; it must not be listed
## explicitly. Because of this there cannot be a user-defined module named
## ``system``.
##
## Module system
## =============
##

# That lonesome header above is to prevent :idx: entries from being mentioned
# in the global index as part of the previous header (Exception hierarchy).

type
  int* {.magic: Int.} ## default integer type; bitwidth depends on
                      ## architecture, but is always the same as a pointer
  int8* {.magic: Int8.} ## signed 8 bit integer type
  int16* {.magic: Int16.} ## signed 16 bit integer type
  int32* {.magic: Int32.} ## signed 32 bit integer type
  int64* {.magic: Int64.} ## signed 64 bit integer type
  uint* {.magic: UInt.} ## unsigned default integer type
  uint8* {.magic: UInt8.} ## unsigned 8 bit integer type
  uint16* {.magic: UInt16.} ## unsigned 16 bit integer type
  uint32* {.magic: UInt32.} ## unsigned 32 bit integer type
  uint64* {.magic: UInt64.} ## unsigned 64 bit integer type
  float* {.magic: Float.} ## default floating point type
  float32* {.magic: Float32.} ## 32 bit floating point type
  float64* {.magic: Float.} ## 64 bit floating point type

# 'float64' is now an alias to 'float'; this solves many problems

type # we need to start a new type section here, so that ``0`` can have a type
  bool* {.magic: Bool.} = enum ## built-in boolean type
    false = 0, true = 1

type
  char* {.magic: Char.} ## built-in 8 bit character type (unsigned)
  string* {.magic: String.} ## built-in string type
  cstring* {.magic: Cstring.} ## built-in cstring (*compatible string*) type
  pointer* {.magic: Pointer.} ## built-in pointer type, use the ``addr``
                              ## operator to get a pointer to a variable

  typedesc* {.magic: TypeDesc.} ## meta type to denote a type description

const
  on* = true    ## alias for ``true``
  off* = false  ## alias for ``false``

{.push warning[GcMem]: off, warning[Uninit]: off.}
{.push hints: off.}

proc `or` *(a, b: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `or` meta class

proc `and` *(a, b: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `and` meta class

proc `not` *(a: typedesc): typedesc {.magic: "TypeTrait", noSideEffect.}
  ## Constructs an `not` meta class

type
  Ordinal* {.magic: Ordinal.}[T] ## Generic ordinal type. Includes integer,
                                 ## bool, character, and enumeration types
                                 ## as well as their subtypes. Note `uint`
                                 ## and `uint64` are not ordinal types for
                                 ## implementation reasons
  `ptr`* {.magic: Pointer.}[T] ## built-in generic untraced pointer type
  `ref`* {.magic: Pointer.}[T] ## built-in generic traced pointer type

  `nil` {.magic: "Nil".}

  expr* {.magic: Expr, deprecated.} ## meta type to denote an expression (for templates)
                                    ## **Deprecated** since version 0.15. Use ``untyped`` instead.
  stmt* {.magic: Stmt, deprecated.} ## meta type to denote a statement (for templates)
                                    ## **Deprecated** since version 0.15. Use ``typed`` instead.

  void* {.magic: "VoidType".}   ## meta type to denote the absence of any type
  auto* {.magic: Expr.} ## meta type for automatic type determination
  any* = distinct auto ## meta type for any supported type
  untyped* {.magic: Expr.} ## meta type to denote an expression that
                           ## is not resolved (for templates)
  typed* {.magic: Stmt.}   ## meta type to denote an expression that
                           ## is resolved (for templates)

  SomeSignedInt* = int|int8|int16|int32|int64
    ## type class matching all signed integer types

  SomeUnsignedInt* = uint|uint8|uint16|uint32|uint64
    ## type class matching all unsigned integer types

  SomeInteger* = SomeSignedInt|SomeUnsignedInt
    ## type class matching all integer types

  SomeOrdinal* = int|int8|int16|int32|int64|bool|enum|uint8|uint16|uint32
    ## type class matching all ordinal types; however this includes enums with
    ## holes.

  SomeReal* = float|float32|float64
    ## type class matching all floating point number types

  SomeNumber* = SomeInteger|SomeReal
    ## type class matching all number types

proc defined*(x: untyped): bool {.magic: "Defined", noSideEffect, compileTime.}
  ## Special compile-time procedure that checks whether `x` is
  ## defined.
  ## `x` is an external symbol introduced through the compiler's
  ## `-d:x switch <nimc.html#compile-time-symbols>`_ to enable build time
  ## conditionals:
  ##
  ## .. code-block:: Nim
  ##   when not defined(release):
  ##     # Do here programmer friendly expensive sanity checks.
  ##   # Put here the normal code

when defined(nimalias):
  {.deprecated: [
    TSignedInt: SomeSignedInt,
    TUnsignedInt: SomeUnsignedInt,
    TInteger: SomeInteger,
    TReal: SomeReal,
    TNumber: SomeNumber,
    TOrdinal: SomeOrdinal].}

proc declared*(x: untyped): bool {.magic: "Defined", noSideEffect, compileTime.}
  ## Special compile-time procedure that checks whether `x` is
  ## declared. `x` has to be an identifier or a qualified identifier.
  ## This can be used to check whether a library provides a certain
  ## feature or not:
  ##
  ## .. code-block:: Nim
  ##   when not declared(strutils.toUpper):
  ##     # provide our own toUpper proc here, because strutils is
  ##     # missing it.

when defined(useNimRtl):
  {.deadCodeElim: on.}

proc definedInScope*(x: untyped): bool {.
  magic: "DefinedInScope", noSideEffect, deprecated, compileTime.}
  ## **Deprecated since version 0.9.6**: Use ``declaredInScope`` instead.

proc declaredInScope*(x: untyped): bool {.
  magic: "DefinedInScope", noSideEffect, compileTime.}
  ## Special compile-time procedure that checks whether `x` is
  ## declared in the current scope. `x` has to be an identifier.

proc `addr`*[T](x: var T): ptr T {.magic: "Addr", noSideEffect.} =
  ## Builtin 'addr' operator for taking the address of a memory location.
  ## Cannot be overloaded.
  ##
  ## .. code-block:: nim
  ##  var
  ##    buf: seq[char] = @['a','b','c']
  ##    p: pointer = buf[1].addr
  ##  echo cast[ptr char](p)[]    # b
  discard

proc unsafeAddr*[T](x: T): ptr T {.magic: "Addr", noSideEffect.} =
  ## Builtin 'addr' operator for taking the address of a memory
  ## location.  This works even for ``let`` variables or parameters
  ## for better interop with C and so it is considered even more
  ## unsafe than the ordinary ``addr``.  When you use it to write a
  ## wrapper for a C library, you should always check that the
  ## original library does never write to data behind the pointer that
  ## is returned from this procedure.
  ## Cannot be overloaded.
  discard

proc `type`*(x: untyped): typeDesc {.magic: "TypeOf", noSideEffect, compileTime.} =
  ## Builtin 'type' operator for accessing the type of an expression.
  ## Cannot be overloaded.
  discard

proc `not` *(x: bool): bool {.magic: "Not", noSideEffect.}
  ## Boolean not; returns true iff ``x == false``.

proc `and`*(x, y: bool): bool {.magic: "And", noSideEffect.}
  ## Boolean ``and``; returns true iff ``x == y == true``.
  ## Evaluation is lazy: if ``x`` is false,
  ## ``y`` will not even be evaluated.
proc `or`*(x, y: bool): bool {.magic: "Or", noSideEffect.}
  ## Boolean ``or``; returns true iff ``not (not x and not y)``.
  ## Evaluation is lazy: if ``x`` is true,
  ## ``y`` will not even be evaluated.
proc `xor`*(x, y: bool): bool {.magic: "Xor", noSideEffect.}
  ## Boolean `exclusive or`; returns true iff ``x != y``.

proc new*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``.

proc new*(T: typedesc): auto =
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it as result value.
  ##
  ## When ``T`` is a ref type then the resulting type will be ``T``,
  ## otherwise it will be ``ref T``.
  when (T is ref):
    var r: T
  else:
    var r: ref T
  new(r)
  return r


proc internalNew*[T](a: var ref T) {.magic: "New", noSideEffect.}
  ## leaked implementation detail. Do not use.

proc new*[T](a: var ref T, finalizer: proc (x: ref T) {.nimcall.}) {.
  magic: "NewFinalize", noSideEffect.}
  ## creates a new object of type ``T`` and returns a safe (traced)
  ## reference to it in ``a``. When the garbage collector frees the object,
  ## `finalizer` is called. The `finalizer` may not keep a reference to the
  ## object pointed to by `x`. The `finalizer` cannot prevent the GC from
  ## freeing the object. Note: The `finalizer` refers to the type `T`, not to
  ## the object! This means that for each object of type `T` the finalizer
  ## will be called!

proc reset*[T](obj: var T) {.magic: "Reset", noSideEffect.}
  ## resets an object `obj` to its initial (binary zero) value. This needs to
  ## be called before any possible `object branch transition`:idx:.

type
  range*{.magic: "Range".}[T] ## Generic type to construct range types.
  array*{.magic: "Array".}[I, T]  ## Generic type to construct
                                  ## fixed-length arrays.
  openArray*{.magic: "OpenArray".}[T]  ## Generic type to construct open arrays.
                                       ## Open arrays are implemented as a
                                       ## pointer to the array data and a
                                       ## length field.
  varargs*{.magic: "Varargs".}[T] ## Generic type to construct a varargs type.
  seq*{.magic: "Seq".}[T]  ## Generic type to construct sequences.
  set*{.magic: "Set".}[T]  ## Generic type to construct bit sets.

  UncheckedArray* {.unchecked.}[T] = array[0, T]
    ## Array with no bounds checking

when defined(nimHasOpt):
  type opt*{.magic: "Opt".}[T]

proc high*[T: Ordinal](x: T): T {.magic: "High", noSideEffect.}
  ## returns the highest possible index of an array, a sequence, a string or
  ## the highest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.
  ## ``high(int)`` is Nim's way of writing `INT_MAX`:idx: or `MAX_INT`:idx:.
  ##
  ## .. code-block:: nim
  ##  var arr = [1,2,3,4,5,6,7]
  ##  high(arr) #=> 6
  ##  high(2) #=> 9223372036854775807
  ##  high(int) #=> 9223372036854775807

proc high*[T: Ordinal](x: typeDesc[T]): T {.magic: "High", noSideEffect.}
proc high*[T](x: openArray[T]): int {.magic: "High", noSideEffect.}
proc high*[I, T](x: array[I, T]): I {.magic: "High", noSideEffect.}
proc high*[I, T](x: typeDesc[array[I, T]]): I {.magic: "High", noSideEffect.}
proc high*(x: cstring): int {.magic: "High", noSideEffect.}
proc high*(x: string): int {.magic: "High", noSideEffect.}

proc low*[T: Ordinal](x: typeDesc[T]): T {.magic: "Low", noSideEffect.}
proc low*[T](x: openArray[T]): int {.magic: "Low", noSideEffect.}
proc low*[I, T](x: array[I, T]): I {.magic: "Low", noSideEffect.}
proc low*[T](x: T): T {.magic: "Low", noSideEffect.}
proc low*[I, T](x: typeDesc[array[I, T]]): I {.magic: "Low", noSideEffect.}
proc low*(x: cstring): int {.magic: "Low", noSideEffect.}
proc low*(x: string): int {.magic: "Low", noSideEffect.}
## returns the lowest possible index of an array, a sequence, a string or
  ## the lowest possible value of an ordinal value `x`. As a special
  ## semantic rule, `x` may also be a type identifier.
  ##
  ## .. code-block:: nim
  ##  var arr = [1,2,3,4,5,6,7]
  ##  low(arr) #=> 0
  ##  low(2) #=> -9223372036854775808
  ##  low(int) #=> -9223372036854775808

when defined(nimArrIdx):
  # :array|openarray|string|seq|cstring|tuple
  proc `[]`*[I: Ordinal;T](a: T; i: I): T {.
    noSideEffect, magic: "ArrGet".}
  proc `[]=`*[I: Ordinal;T,S](a: T; i: I;
    x: S) {.noSideEffect, magic: "ArrPut".}
  proc `=`*[T](dest: var T; src: T) {.noSideEffect, magic: "Asgn".}

type
  Slice*[T] = object ## builtin slice type
    a*, b*: T        ## the bounds

when defined(nimalias):
  {.deprecated: [TSlice: Slice].}

proc `..`*[T](a, b: T): Slice[T] {.noSideEffect, inline, magic: "DotDot".} =
  ## `slice`:idx: operator that constructs an interval ``[a, b]``, both `a`
  ## and `b` are inclusive. Slices can also be used in the set constructor
  ## and in ordinal case statements, but then they are special-cased by the
  ## compiler.
  result.a = a
  result.b = b

proc `..`*[T](b: T): Slice[T] {.noSideEffect, inline, magic: "DotDot".} =
  ## `slice`:idx: operator that constructs an interval ``[default(T), b]``
  result.b = b

when not defined(niminheritable):
  {.pragma: inheritable.}
when not defined(nimunion):
  {.pragma: unchecked.}

# comparison operators:
proc `==` *[Enum: enum](x, y: Enum): bool {.magic: "EqEnum", noSideEffect.}
  ## Checks whether values within the *same enum* have the same underlying value
  ##
  ## .. code-block:: nim
  ##  type
  ##    Enum1 = enum
  ##      Field1 = 3, Field2
  ##    Enum2 = enum
  ##      Place1, Place2 = 3
  ##  var
  ##    e1 = Field1
  ##    e2 = Enum1(Place2)
  ##  echo (e1 == e2) # true
  ##  echo (e1 == Place2) # raises error
proc `==` *(x, y: pointer): bool {.magic: "EqRef", noSideEffect.}
  ## .. code-block:: nim
  ##  var # this is a wildly dangerous example
  ##    a = cast[pointer](0)
  ##    b = cast[pointer](nil)
  ##  echo (a == b) # true due to the special meaning of `nil`/0 as a pointer
proc `==` *(x, y: string): bool {.magic: "EqStr", noSideEffect.}
  ## Checks for equality between two `string` variables

proc `==` *(x, y: char): bool {.magic: "EqCh", noSideEffect.}
  ## Checks for equality between two `char` variables
proc `==` *(x, y: bool): bool {.magic: "EqB", noSideEffect.}
  ## Checks for equality between two `bool` variables

proc `==` *[T](x, y: set[T]): bool {.magic: "EqSet", noSideEffect.}
  ## Checks for equality between two variables of type `set`
  ##
  ## .. code-block:: nim
  ##  var a = {1, 2, 2, 3} # duplication in sets is ignored
  ##  var b = {1, 2, 3}
  ##  echo (a == b) # true

proc `==` *[T](x, y: ref T): bool {.magic: "EqRef", noSideEffect.}
  ## Checks that two `ref` variables refer to the same item
proc `==` *[T](x, y: ptr T): bool {.magic: "EqRef", noSideEffect.}
  ## Checks that two `ptr` variables refer to the same item
proc `==` *[T: proc](x, y: T): bool {.magic: "EqProc", noSideEffect.}
  ## Checks that two `proc` variables refer to the same procedure

proc `<=` *[Enum: enum](x, y: Enum): bool {.magic: "LeEnum", noSideEffect.}
proc `<=` *(x, y: string): bool {.magic: "LeStr", noSideEffect.}
proc `<=` *(x, y: char): bool {.magic: "LeCh", noSideEffect.}
proc `<=` *[T](x, y: set[T]): bool {.magic: "LeSet", noSideEffect.}
proc `<=` *(x, y: bool): bool {.magic: "LeB", noSideEffect.}
proc `<=` *[T](x, y: ref T): bool {.magic: "LePtr", noSideEffect.}
proc `<=` *(x, y: pointer): bool {.magic: "LePtr", noSideEffect.}

proc `<` *[Enum: enum](x, y: Enum): bool {.magic: "LtEnum", noSideEffect.}
proc `<` *(x, y: string): bool {.magic: "LtStr", noSideEffect.}
proc `<` *(x, y: char): bool {.magic: "LtCh", noSideEffect.}
proc `<` *[T](x, y: set[T]): bool {.magic: "LtSet", noSideEffect.}
proc `<` *(x, y: bool): bool {.magic: "LtB", noSideEffect.}
proc `<` *[T](x, y: ref T): bool {.magic: "LtPtr", noSideEffect.}
proc `<` *[T](x, y: ptr T): bool {.magic: "LtPtr", noSideEffect.}
proc `<` *(x, y: pointer): bool {.magic: "LtPtr", noSideEffect.}

template `!=` * (x, y: untyped): untyped =
  ## unequals operator. This is a shorthand for ``not (x == y)``.
  not (x == y)

template `>=` * (x, y: untyped): untyped =
  ## "is greater or equals" operator. This is the same as ``y <= x``.
  y <= x

template `>` * (x, y: untyped): untyped =
  ## "is greater" operator. This is the same as ``y < x``.
  y < x

const
  appType* {.magic: "AppType"}: string = ""
    ## a string that describes the application type. Possible values:
    ## "console", "gui", "lib".

include "system/inclrtl"

const NoFakeVars* = defined(nimscript) ## true if the backend doesn't support \
  ## "fake variables" like 'var EBADF {.importc.}: cint'.

when not defined(JS):
  type
    TGenericSeq {.compilerproc, pure, inheritable.} = object
      len, reserved: int
      when defined(gogc):
        elemSize: int
    PGenericSeq {.exportc.} = ptr TGenericSeq
    # len and space without counting the terminating zero:
    NimStringDesc {.compilerproc, final.} = object of TGenericSeq
      data: UncheckedArray[char]
    NimString = ptr NimStringDesc

when not defined(JS) and not defined(nimscript):
  template space(s: PGenericSeq): int {.dirty.} =
    s.reserved and not (seqShallowFlag or strlitFlag)
  include "system/hti"

type
  byte* = uint8 ## this is an alias for ``uint8``, that is an unsigned
                ## int 8 bits wide.

  Natural* = range[0..high(int)]
    ## is an int type ranging from zero to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  Positive* = range[1..high(int)]
    ## is an int type ranging from one to the maximum value
    ## of an int. This type is often useful for documentation and debugging.

  RootObj* {.compilerProc, inheritable.} =
    object ## the root of Nim's object hierarchy. Objects should
            ## inherit from RootObj or one of its descendants. However,
            ## objects that have no ancestor are allowed.
  RootRef* = ref RootObj ## reference to RootObj

  RootEffect* {.compilerproc.} = object of RootObj ## \
    ## base effect class; each effect should
    ## inherit from `RootEffect` unless you know what
    ## you doing.
  TimeEffect* = object of RootEffect   ## Time effect.
  IOEffect* = object of RootEffect     ## IO effect.
  ReadIOEffect* = object of IOEffect   ## Effect describing a read IO operation.
  WriteIOEffect* = object of IOEffect  ## Effect describing a write IO operation.
  ExecIOEffect* = object of IOEffect   ## Effect describing an executing IO operation.

  Exception* {.compilerproc.} = object of RootObj ## \
    ## Base exception class.
    ##
    ## Each exception has to inherit from `Exception`. See the full `exception
    ## hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
    parent*: ref Exception ## parent exception (can be used as a stack)
    name*: cstring ## The exception's name is its Nim identifier.
                    ## This field is filled automatically in the
                    ## ``raise`` statement.
    msg* {.exportc: "message".}: string ## the exception's message. Not
                                        ## providing an exception message
                                        ## is bad style.
    trace: string
    up: ref Exception # used for stacking exceptions. Not exported!

  SystemError* = object of Exception ## \
    ## Abstract class for exceptions that the runtime system raises.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  IOError* = object of SystemError ## \
    ## Raised if an IO error occurred.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  EOFError* = object of IOError ## \
    ## Raised if an IO "end of file" error occurred.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  OSError* = object of SystemError ## \
    ## Raised if an operating system service failed.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
    errorCode*: int32 ## OS-defined error code describing this error.
  LibraryError* = object of OSError ## \
    ## Raised if a dynamic library could not be loaded.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ResourceExhaustedError* = object of SystemError ## \
    ## Raised if a resource request could not be fulfilled.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ArithmeticError* = object of Exception ## \
    ## Raised if any kind of arithmetic error occurred.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  DivByZeroError* = object of ArithmeticError ## \
    ## Raised for runtime integer divide-by-zero errors.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.

  OverflowError* = object of ArithmeticError ## \
    ## Raised for runtime integer overflows.
    ##
    ## This happens for calculations whose results are too large to fit in the
    ## provided bits.  See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  AccessViolationError* = object of Exception ## \
    ## Raised for invalid memory access errors
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  AssertionError* = object of Exception ## \
    ## Raised when assertion is proved wrong.
    ##
    ## Usually the result of using the `assert() template <#assert>`_.  See the
    ## full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ValueError* = object of Exception ## \
    ## Raised for string and object conversion errors.
  KeyError* = object of ValueError ## \
    ## Raised if a key cannot be found in a table.
    ##
    ## Mostly used by the `tables <tables.html>`_ module, it can also be raised
    ## by other collection modules like `sets <sets.html>`_ or `strtabs
    ## <strtabs.html>`_. See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  OutOfMemError* = object of SystemError ## \
    ## Raised for unsuccessful attempts to allocate memory.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  IndexError* = object of Exception ## \
    ## Raised if an array index is out of bounds.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.

  FieldError* = object of Exception ## \
    ## Raised if a record field is not accessible because its dicriminant's
    ## value does not fit.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  RangeError* = object of Exception ## \
    ## Raised if a range check error occurred.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  StackOverflowError* = object of SystemError ## \
    ## Raised if the hardware stack used for subroutine calls overflowed.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ReraiseError* = object of Exception ## \
    ## Raised if there is no exception to reraise.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ObjectAssignmentError* = object of Exception ## \
    ## Raised if an object gets assigned to its parent's object.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  ObjectConversionError* = object of Exception ## \
    ## Raised if an object is converted to an incompatible object type.
    ## You can use ``of`` operator to check if conversion will succeed.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatingPointError* = object of Exception ## \
    ## Base class for floating point exceptions.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatInvalidOpError* = object of FloatingPointError ## \
    ## Raised by invalid operations according to IEEE.
    ##
    ## Raised by ``0.0/0.0``, for example.  See the full `exception
    ## hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatDivByZeroError* = object of FloatingPointError ## \
    ## Raised by division by zero.
    ##
    ## Divisor is zero and dividend is a finite nonzero number.  See the full
    ## `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatOverflowError* = object of FloatingPointError ## \
    ## Raised for overflows.
    ##
    ## The operation produced a result that exceeds the range of the exponent.
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatUnderflowError* = object of FloatingPointError ## \
    ## Raised for underflows.
    ##
    ## The operation produced a result that is too small to be represented as a
    ## normal number. See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  FloatInexactError* = object of FloatingPointError ## \
    ## Raised for inexact results.
    ##
    ## The operation produced a result that cannot be represented with infinite
    ## precision -- for example: ``2.0 / 3.0, log(1.1)``
    ##
    ## **NOTE**: Nim currently does not detect these!  See the full
    ## `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  DeadThreadError* = object of Exception ## \
    ## Raised if it is attempted to send a message to a dead thread.
    ##
    ## See the full `exception hierarchy <manual.html#exception-handling-exception-hierarchy>`_.
  NilAccessError* = object of SystemError ## \
    ## Raised on dereferences of ``nil`` pointers.
    ##
    ## This is only raised if the ``segfaults.nim`` module was imported!

{.deprecated: [TObject: RootObj, PObject: RootRef, TEffect: RootEffect,
  FTime: TimeEffect, FIO: IOEffect, FReadIO: ReadIOEffect,
  FWriteIO: WriteIOEffect, FExecIO: ExecIOEffect,

  E_Base: Exception, ESystem: SystemError, EIO: IOError,
  EOS: OSError, EInvalidLibrary: LibraryError,
  EResourceExhausted: ResourceExhaustedError,
  EArithmetic: ArithmeticError, EDivByZero: DivByZeroError,
  EOverflow: OverflowError, EAccessViolation: AccessViolationError,
  EAssertionFailed: AssertionError, EInvalidValue: ValueError,
  EInvalidKey: KeyError, EOutOfMemory: OutOfMemError,
  EInvalidIndex: IndexError, EInvalidField: FieldError,
  EOutOfRange: RangeError, EStackOverflow: StackOverflowError,
  ENoExceptionToReraise: ReraiseError,
  EInvalidObjectAssignment: ObjectAssignmentError,
  EInvalidObjectConversion: ObjectConversionError,
  EDeadThread: DeadThreadError,
  EFloatInexact: FloatInexactError,
  EFloatUnderflow: FloatUnderflowError,
  EFloatingPoint: FloatingPointError,
  EFloatInvalidOp: FloatInvalidOpError,
  EFloatDivByZero: FloatDivByZeroError,
  EFloatOverflow: FloatOverflowError,
  ESynch: Exception
].}
  
proc unsafeNew*[T](a: var ref T, size: Natural) {.magic: "New", noSideEffect.}
## creates a new object of type ``T`` and returns a safe (traced)
## reference to it in ``a``. This is **unsafe** as it allocates an object
## of the passed ``size``. This should only be used for optimization
## purposes when you know what you're doing!

proc sizeof*[T](x: T): int {.magic: "SizeOf", noSideEffect.}
## returns the size of ``x`` in bytes. Since this is a low-level proc,
## its usage is discouraged - using ``new`` for the most cases suffices
## that one never needs to know ``x``'s size. As a special semantic rule,
## ``x`` may also be a type identifier (``sizeof(int)`` is valid).
##
## Limitations: If used within nim VM context ``sizeof`` will only work
## for simple types.
##
## .. code-block:: nim
##  sizeof('A') #=> 1
##  sizeof(2) #=> 8

when defined(nimtypedescfixed):
  proc sizeof*(x: typedesc): int {.magic: "SizeOf", noSideEffect.}

when defined wasm:
  discard #include wasmextra

proc `<`*[T](x: Ordinal[T]): T {.magic: "UnaryLt", noSideEffect.}
  ## unary ``<`` that can be used for nice looking excluding ranges:
  ##
  ## .. code-block:: nim
  ##   for i in 0 .. <10: echo i #=> 0 1 2 3 4 5 6 7 8 9
  ##
  ## Semantically this is the same as ``pred``.

proc succ*[T](x: Ordinal[T], y = 1): T {.magic: "Succ", noSideEffect.}
  ## returns the ``y``-th successor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc pred*[T](x: Ordinal[T], y = 1): T {.magic: "Pred", noSideEffect.}
  ## returns the ``y``-th predecessor of the value ``x``. ``T`` has to be
  ## an ordinal type. If such a value does not exist, ``EOutOfRange`` is raised
  ## or a compile time error occurs.

proc inc*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Inc", noSideEffect.}
  ## increments the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = succ(x, y)``.
  ##
  ## .. code-block:: nim
  ##  var i = 2
  ##  inc(i) #=> 3
  ##  inc(i, 3) #=> 6

proc dec*[T: Ordinal|uint|uint64](x: var T, y = 1) {.magic: "Dec", noSideEffect.}
  ## decrements the ordinal ``x`` by ``y``. If such a value does not
  ## exist, ``EOutOfRange`` is raised or a compile time error occurs. This is a
  ## short notation for: ``x = pred(x, y)``.
  ##
  ## .. code-block:: nim
  ##  var i = 2
  ##  dec(i) #=> 1
  ##  dec(i, 3) #=> -2

proc newSeq*[T](s: var seq[T], len: Natural) {.magic: "NewSeq", noSideEffect.}
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ## This is equivalent to ``s = @[]; setlen(s, len)``, but more
  ## efficient since no reallocation is needed.
  ##
  ## Note that the sequence will be filled with zeroed entries, which can be a
  ## problem for sequences containing strings since their value will be
  ## ``nil``. After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them. Example:
  ##
  ## .. code-block:: nim
  ##   var inputStrings : seq[string]
  ##   newSeq(inputStrings, 3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"

proc newSeq*[T](len = 0.Natural): seq[T] =
  ## creates a new sequence of type ``seq[T]`` with length ``len``.
  ##
  ## Note that the sequence will be filled with zeroed entries, which can be a
  ## problem for sequences containing strings since their value will be
  ## ``nil``. After the creation of the sequence you should assign entries to
  ## the sequence instead of adding them. Example:
  ##
  ## .. code-block:: nim
  ##   var inputStrings = newSeq[string](3)
  ##   inputStrings[0] = "The fourth"
  ##   inputStrings[1] = "assignment"
  ##   inputStrings[2] = "would crash"
  ##   #inputStrings[3] = "out of bounds"
  newSeq(result, len)

proc newSeqOfCap*[T](cap: Natural): seq[T] {.
  magic: "NewSeqOfCap", noSideEffect.} =
  ## creates a new sequence of type ``seq[T]`` with length 0 and capacity
  ## ``cap``.
  discard

proc len*[TOpenArray: openArray|varargs](x: TOpenArray): int {.
  magic: "LengthOpenArray", noSideEffect.}
proc len*(x: string): int {.magic: "LengthStr", noSideEffect.}
proc len*(x: cstring): int {.magic: "LengthStr", noSideEffect.}
proc len*(x: (type array)|array): int {.magic: "LengthArray", noSideEffect.}
proc len*[T](x: seq[T]): int {.magic: "LengthSeq", noSideEffect.}
  ## returns the length of an array, an openarray, a sequence or a string.
  ## This is roughly the same as ``high(T)-low(T)+1``, but its resulting type is
  ## always an int.
  ##
  ## .. code-block:: nim
  ##  var arr = [1,1,1,1,1]
  ##  len(arr) #=> 5
  ##  for i in 0..<arr.len:
  ##    echo arr[i] #=> 1,1,1,1,1

#[TODO:
# set routines:
proc incl*[T](x: var set[T], y: T) {.magic: "Incl", noSideEffect.}
  ## includes element ``y`` to the set ``x``. This is the same as
  ## ``x = x + {y}``, but it might be more efficient.
  ##
  ## .. code-block:: nim
  ##  var a = initSet[int](4)
  ##  a.incl(2) #=> {2}
  ##  a.incl(3) #=> {2, 3}

template incl*[T](s: var set[T], flags: set[T]) =
  ## includes the set of flags to the set ``x``.
  s = s + flags

proc excl*[T](x: var set[T], y: T) {.magic: "Excl", noSideEffect.}
  ## excludes element ``y`` to the set ``x``. This is the same as
  ## ``x = x - {y}``, but it might be more efficient.
  ##
  ## .. code-block:: nim
  ##  var b = {2,3,5,6,12,545}
  ##  b.excl(5)  #=> {2,3,6,12,545}

template excl*[T](s: var set[T], flags: set[T]) =
  ## excludes the set of flags to ``x``.
  s = s - flags

proc card*[T](x: set[T]): int {.magic: "Card", noSideEffect.}
  ## returns the cardinality of the set ``x``, i.e. the number of elements
  ## in the set.
  ##
  ## .. code-block:: nim
  ##  var i = {1,2,3,4}
  ##  card(i) #=> 4
]#
proc ord*[T](x: T): int {.magic: "Ord", noSideEffect.}
  ## returns the internal int value of an ordinal value ``x``.
  ##
  ## .. code-block:: nim
  ##  ord('A') #=> 65

proc chr*(u: range[0..255]): char {.magic: "Chr", noSideEffect.}
  ## converts an int in the range 0..255 to a character.
  ##
  ## .. code-block:: nim
  ##  chr(65) #=> A

# --------------------------------------------------------------------------
# built-in operators

when not defined(JS): # TODO:
  proc ze*(x: int8): int {.magic: "Ze8ToI", noSideEffect.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.
  proc ze*(x: int16): int {.magic: "Ze16ToI", noSideEffect.}
    ## zero extends a smaller integer type to ``int``. This treats `x` as
    ## unsigned.

  proc ze64*(x: int8): int64 {.magic: "Ze8ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
  proc ze64*(x: int16): int64 {.magic: "Ze16ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.

  proc ze64*(x: int32): int64 {.magic: "Ze32ToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned.
  proc ze64*(x: int): int64 {.magic: "ZeIToI64", noSideEffect.}
    ## zero extends a smaller integer type to ``int64``. This treats `x` as
    ## unsigned. Does nothing if the size of an ``int`` is the same as ``int64``.
    ## (This is the case on 64 bit processors.)

  proc toU8*(x: int): int8 {.magic: "ToU8", noSideEffect.}
    ## treats `x` as unsigned and converts it to a byte by taking the last 8 bits
    ## from `x`.
  proc toU16*(x: int): int16 {.magic: "ToU16", noSideEffect.}
    ## treats `x` as unsigned and converts it to an ``int16`` by taking the last
    ## 16 bits from `x`.
  proc toU32*(x: int64): int32 {.magic: "ToU32", noSideEffect.}
    ## treats `x` as unsigned and converts it to an ``int32`` by taking the
    ## last 32 bits from `x`.

# integer calculations:
proc `+` *(x: int): int {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int8): int8 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int16): int16 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int32): int32 {.magic: "UnaryPlusI", noSideEffect.}
proc `+` *(x: int64): int64 {.magic: "UnaryPlusI", noSideEffect.}
  ## Unary `+` operator for an integer. Has no effect.

proc `-` *(x: int): int {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int8): int8 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int16): int16 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int32): int32 {.magic: "UnaryMinusI", noSideEffect.}
proc `-` *(x: int64): int64 {.magic: "UnaryMinusI64", noSideEffect.}
  ## Unary `-` operator for an integer. Negates `x`.

proc `not` *(x: int): int {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int8): int8 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int16): int16 {.magic: "BitnotI", noSideEffect.}
proc `not` *(x: int32): int32 {.magic: "BitnotI", noSideEffect.}
  ## computes the `bitwise complement` of the integer `x`.

when defined(nimnomagic64):
  proc `not` *(x: int64): int64 {.magic: "BitnotI", noSideEffect.}
else:
  proc `not` *(x: int64): int64 {.magic: "BitnotI64", noSideEffect.}

proc `+` *(x, y: int): int {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int8): int8 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int16): int16 {.magic: "AddI", noSideEffect.}
proc `+` *(x, y: int32): int32 {.magic: "AddI", noSideEffect.}
  ## Binary `+` operator for an integer.

when defined(nimnomagic64):
  proc `+` *(x, y: int64): int64 {.magic: "AddI", noSideEffect.}
else:
  proc `+` *(x, y: int64): int64 {.magic: "AddI64", noSideEffect.}

proc `-` *(x, y: int): int {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int8): int8 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int16): int16 {.magic: "SubI", noSideEffect.}
proc `-` *(x, y: int32): int32 {.magic: "SubI", noSideEffect.}
  ## Binary `-` operator for an integer.

when defined(nimnomagic64):
  proc `-` *(x, y: int64): int64 {.magic: "SubI", noSideEffect.}
else:
  proc `-` *(x, y: int64): int64 {.magic: "SubI64", noSideEffect.}

proc `*` *(x, y: int): int {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int8): int8 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int16): int16 {.magic: "MulI", noSideEffect.}
proc `*` *(x, y: int32): int32 {.magic: "MulI", noSideEffect.}
  ## Binary `*` operator for an integer.

when defined(nimnomagic64):
  proc `*` *(x, y: int64): int64 {.magic: "MulI", noSideEffect.}
else:
  proc `*` *(x, y: int64): int64 {.magic: "MulI64", noSideEffect.}

proc `div` *(x, y: int): int {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int8): int8 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int16): int16 {.magic: "DivI", noSideEffect.}
proc `div` *(x, y: int32): int32 {.magic: "DivI", noSideEffect.}
  ## computes the integer division. This is roughly the same as
  ## ``floor(x/y)``.
  ##
  ## .. code-block:: Nim
  ##   1 div 2 == 0
  ##   2 div 2 == 1
  ##   3 div 2 == 1
  ##   7 div 5 == 1

when defined(nimnomagic64):
  proc `div` *(x, y: int64): int64 {.magic: "DivI", noSideEffect.}
else:
  proc `div` *(x, y: int64): int64 {.magic: "DivI64", noSideEffect.}

proc `mod` *(x, y: int): int {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int8): int8 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int16): int16 {.magic: "ModI", noSideEffect.}
proc `mod` *(x, y: int32): int32 {.magic: "ModI", noSideEffect.}
  ## computes the integer modulo operation (remainder).
  ## This is the same as
  ## ``x - (x div y) * y``.
  ##
  ## .. code-block:: Nim
  ##   (7 mod 5) == 2

when defined(nimnomagic64):
  proc `mod` *(x, y: int64): int64 {.magic: "ModI", noSideEffect.}
else:
  proc `mod` *(x, y: int64): int64 {.magic: "ModI64", noSideEffect.}

when defined(nimNewShiftOps):
  proc `shr` *(x: int, y: SomeInteger): int {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x: int8, y: SomeInteger): int8 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x: int16, y: SomeInteger): int16 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x: int32, y: SomeInteger): int32 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x: int64, y: SomeInteger): int64 {.magic: "ShrI", noSideEffect.}
    ## computes the `shift right` operation of `x` and `y`, filling
    ## vacant bit positions with zeros.
    ##
    ## .. code-block:: Nim
    ##   0b0001_0000'i8 shr 2 == 0b0000_0100'i8
    ##   0b1000_0000'i8 shr 8 == 0b0000_0000'i8
    ##   0b0000_0001'i8 shr 1 == 0b0000_0000'i8

  proc `shl` *(x: int, y: SomeInteger): int {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x: int8, y: SomeInteger): int8 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x: int16, y: SomeInteger): int16 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x: int32, y: SomeInteger): int32 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x: int64, y: SomeInteger): int64 {.magic: "ShlI", noSideEffect.}
    ## computes the `shift left` operation of `x` and `y`.
    ##
    ## .. code-block:: Nim
    ##  1'i32 shl 4 == 0x0000_0010
    ##  1'i64 shl 4 == 0x0000_0000_0000_0010
else:
  proc `shr` *(x, y: int): int {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x, y: int8): int8 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x, y: int16): int16 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x, y: int32): int32 {.magic: "ShrI", noSideEffect.}
  proc `shr` *(x, y: int64): int64 {.magic: "ShrI", noSideEffect.}

  proc `shl` *(x, y: int): int {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x, y: int8): int8 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x, y: int16): int16 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x, y: int32): int32 {.magic: "ShlI", noSideEffect.}
  proc `shl` *(x, y: int64): int64 {.magic: "ShlI", noSideEffect.}

proc `and` *(x, y: int): int {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int8): int8 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int16): int16 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int32): int32 {.magic: "BitandI", noSideEffect.}
proc `and` *(x, y: int64): int64 {.magic: "BitandI", noSideEffect.}
  ## computes the `bitwise and` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##  (0xffff'i16 and 0x0010'i16) == 0x0010

proc `or` *(x, y: int): int {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int8): int8 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int16): int16 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int32): int32 {.magic: "BitorI", noSideEffect.}
proc `or` *(x, y: int64): int64 {.magic: "BitorI", noSideEffect.}
  ## computes the `bitwise or` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##  (0x0005'i16 or 0x0010'i16) == 0x0015

proc `xor` *(x, y: int): int {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int8): int8 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int16): int16 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int32): int32 {.magic: "BitxorI", noSideEffect.}
proc `xor` *(x, y: int64): int64 {.magic: "BitxorI", noSideEffect.}
  ## computes the `bitwise xor` of numbers `x` and `y`.
  ##
  ## .. code-block:: Nim
  ##  (0x1011'i16 xor 0x0101'i16) == 0x1110

proc `==` *(x, y: int): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int8): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int16): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int32): bool {.magic: "EqI", noSideEffect.}
proc `==` *(x, y: int64): bool {.magic: "EqI", noSideEffect.}
  ## Compares two integers for equality.

proc `<=` *(x, y: int): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int8): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int16): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int32): bool {.magic: "LeI", noSideEffect.}
proc `<=` *(x, y: int64): bool {.magic: "LeI", noSideEffect.}
  ## Returns true iff `x` is less than or equal to `y`.

proc `<` *(x, y: int): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int8): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int16): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int32): bool {.magic: "LtI", noSideEffect.}
proc `<` *(x, y: int64): bool {.magic: "LtI", noSideEffect.}
  ## Returns true iff `x` is less than `y`.

type
  IntMax32 = int|int8|int16|int32

proc `+%` *(x, y: IntMax32): IntMax32 {.magic: "AddU", noSideEffect.}
proc `+%` *(x, y: int64): int64 {.magic: "AddU", noSideEffect.}
  ## treats `x` and `y` as unsigned and adds them. The result is truncated to
  ## fit into the result. This implements modulo arithmetic. No overflow
  ## errors are possible.

proc `-%` *(x, y: IntMax32): IntMax32 {.magic: "SubU", noSideEffect.}
proc `-%` *(x, y: int64): int64 {.magic: "SubU", noSideEffect.}
  ## treats `x` and `y` as unsigned and subtracts them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `*%` *(x, y: IntMax32): IntMax32 {.magic: "MulU", noSideEffect.}
proc `*%` *(x, y: int64): int64 {.magic: "MulU", noSideEffect.}
  ## treats `x` and `y` as unsigned and multiplies them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `/%` *(x, y: IntMax32): IntMax32 {.magic: "DivU", noSideEffect.}
proc `/%` *(x, y: int64): int64 {.magic: "DivU", noSideEffect.}
  ## treats `x` and `y` as unsigned and divides them. The result is
  ## truncated to fit into the result. This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `%%` *(x, y: IntMax32): IntMax32 {.magic: "ModU", noSideEffect.}
proc `%%` *(x, y: int64): int64 {.magic: "ModU", noSideEffect.}
  ## treats `x` and `y` as unsigned and compute the modulo of `x` and `y`.
  ## The result is truncated to fit into the result.
  ## This implements modulo arithmetic.
  ## No overflow errors are possible.

proc `<=%` *(x, y: IntMax32): bool {.magic: "LeU", noSideEffect.}
proc `<=%` *(x, y: int64): bool {.magic: "LeU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) <= unsigned(y)``.

proc `<%` *(x, y: IntMax32): bool {.magic: "LtU", noSideEffect.}
proc `<%` *(x, y: int64): bool {.magic: "LtU64", noSideEffect.}
  ## treats `x` and `y` as unsigned and compares them.
  ## Returns true iff ``unsigned(x) < unsigned(y)``.

# unsigned integer operations:
proc `not`*[T: SomeUnsignedInt](x: T): T {.magic: "BitnotI", noSideEffect.}
  ## computes the `bitwise complement` of the integer `x`.

when defined(nimNewShiftOps):
  proc `shr`*[T: SomeUnsignedInt](x: T, y: SomeInteger): T {.magic: "ShrI", noSideEffect.}
    ## computes the `shift right` operation of `x` and `y`.

  proc `shl`*[T: SomeUnsignedInt](x: T, y: SomeInteger): T {.magic: "ShlI", noSideEffect.}
    ## computes the `shift left` operation of `x` and `y`.
else:
  proc `shr`*[T: SomeUnsignedInt](x, y: T): T {.magic: "ShrI", noSideEffect.}
    ## computes the `shift right` operation of `x` and `y`.

  proc `shl`*[T: SomeUnsignedInt](x, y: T): T {.magic: "ShlI", noSideEffect.}
    ## computes the `shift left` operation of `x` and `y`.

proc `and`*[T: SomeUnsignedInt](x, y: T): T {.magic: "BitandI", noSideEffect.}
  ## computes the `bitwise and` of numbers `x` and `y`.

proc `or`*[T: SomeUnsignedInt](x, y: T): T {.magic: "BitorI", noSideEffect.}
  ## computes the `bitwise or` of numbers `x` and `y`.

proc `xor`*[T: SomeUnsignedInt](x, y: T): T {.magic: "BitxorI", noSideEffect.}
  ## computes the `bitwise xor` of numbers `x` and `y`.

proc `==`*[T: SomeUnsignedInt](x, y: T): bool {.magic: "EqI", noSideEffect.}
  ## Compares two unsigned integers for equality.

proc `+`*[T: SomeUnsignedInt](x, y: T): T {.magic: "AddU", noSideEffect.}
  ## Binary `+` operator for unsigned integers.

proc `-`*[T: SomeUnsignedInt](x, y: T): T {.magic: "SubU", noSideEffect.}
  ## Binary `-` operator for unsigned integers.

proc `*`*[T: SomeUnsignedInt](x, y: T): T {.magic: "MulU", noSideEffect.}
  ## Binary `*` operator for unsigned integers.

proc `div`*[T: SomeUnsignedInt](x, y: T): T {.magic: "DivU", noSideEffect.}
  ## computes the integer division. This is roughly the same as
  ## ``floor(x/y)``.
  ##
  ## .. code-block:: Nim
  ##  (7 div 5) == 1

proc `mod`*[T: SomeUnsignedInt](x, y: T): T {.magic: "ModU", noSideEffect.}
  ## computes the integer modulo operation (remainder).
  ## This is the same as
  ## ``x - (x div y) * y``.
  ##
  ## .. code-block:: Nim
  ##   (7 mod 5) == 2

proc `<=`*[T: SomeUnsignedInt](x, y: T): bool {.magic: "LeU", noSideEffect.}
  ## Returns true iff ``x <= y``.

proc `<`*[T: SomeUnsignedInt](x, y: T): bool {.magic: "LtU", noSideEffect.}
  ## Returns true iff ``unsigned(x) < unsigned(y)``.

# floating point operations:
proc `+` *(x: float32): float32 {.magic: "UnaryPlusF64", noSideEffect.}
proc `-` *(x: float32): float32 {.magic: "UnaryMinusF64", noSideEffect.}
proc `+` *(x, y: float32): float32 {.magic: "AddF64", noSideEffect.}
proc `-` *(x, y: float32): float32 {.magic: "SubF64", noSideEffect.}
proc `*` *(x, y: float32): float32 {.magic: "MulF64", noSideEffect.}
proc `/` *(x, y: float32): float32 {.magic: "DivF64", noSideEffect.}

proc `+` *(x: float): float {.magic: "UnaryPlusF64", noSideEffect.}
proc `-` *(x: float): float {.magic: "UnaryMinusF64", noSideEffect.}
proc `+` *(x, y: float): float {.magic: "AddF64", noSideEffect.}
proc `-` *(x, y: float): float {.magic: "SubF64", noSideEffect.}
proc `*` *(x, y: float): float {.magic: "MulF64", noSideEffect.}
proc `/` *(x, y: float): float {.magic: "DivF64", noSideEffect.}
  ## computes the floating point division

proc `==` *(x, y: float32): bool {.magic: "EqF64", noSideEffect.}
proc `<=` *(x, y: float32): bool {.magic: "LeF64", noSideEffect.}
proc `<`  *(x, y: float32): bool {.magic: "LtF64", noSideEffect.}

proc `==` *(x, y: float): bool {.magic: "EqF64", noSideEffect.}
proc `<=` *(x, y: float): bool {.magic: "LeF64", noSideEffect.}
proc `<`  *(x, y: float): bool {.magic: "LtF64", noSideEffect.}

# set operators TODO:
proc `*` *[T](x, y: set[T]): set[T] {.magic: "MulSet", noSideEffect.}
  ## This operator computes the intersection of two sets.
proc `+` *[T](x, y: set[T]): set[T] {.magic: "PlusSet", noSideEffect.}
  ## This operator computes the union of two sets.
proc `-` *[T](x, y: set[T]): set[T] {.magic: "MinusSet", noSideEffect.}
  ## This operator computes the difference of two sets.

proc contains*[T](x: set[T], y: T): bool {.magic: "InSet", noSideEffect.}
  ## One should overload this proc if one wants to overload the ``in`` operator.
  ## The parameters are in reverse order! ``a in b`` is a template for
  ## ``contains(b, a)``.
  ## This is because the unification algorithm that Nim uses for overload
  ## resolution works from left to right.
  ## But for the ``in`` operator that would be the wrong direction for this
  ## piece of code:
  ##
  ## .. code-block:: Nim
  ##   var s: set[range['a'..'z']] = {'a'..'c'}
  ##   writeLine(stdout, 'b' in s)
  ##
  ## If ``in`` had been declared as ``[T](elem: T, s: set[T])`` then ``T`` would
  ## have been bound to ``char``. But ``s`` is not compatible to type
  ## ``set[char]``! The solution is to bind ``T`` to ``range['a'..'z']``. This
  ## is achieved by reversing the parameters for ``contains``; ``in`` then
  ## passes its arguments in reverse order.

proc contains*[T](s: Slice[T], value: T): bool {.noSideEffect, inline.} =
  ## Checks if `value` is within the range of `s`; returns true iff
  ## `value >= s.a and value <= s.b`
  ##
  ## .. code-block:: Nim
  ##   assert((1..3).contains(1) == true)
  ##   assert((1..3).contains(2) == true)
  ##   assert((1..3).contains(4) == false)
  result = s.a <= value and value <= s.b

template `in` * (x, y: untyped): untyped {.dirty.} = contains(y, x)
  ## Sugar for contains
  ##
  ## .. code-block:: Nim
  ##   assert(1 in (1..3) == true)
  ##   assert(5 in (1..3) == false)
template `notin` * (x, y: untyped): untyped {.dirty.} = not contains(y, x)
  ## Sugar for not containing
  ##
  ## .. code-block:: Nim
  ##   assert(1 notin (1..3) == false)
  ##   assert(5 notin (1..3) == true)

proc `is` *[T, S](x: T, y: S): bool {.magic: "Is", noSideEffect.}
  ## Checks if T is of the same type as S
  ##
  ## .. code-block:: Nim
  ##   proc test[T](a: T): int =
  ##     when (T is int):
  ##       return a
  ##     else:
  ##       return 0
  ##
  ##   assert(test[int](3) == 3)
  ##   assert(test[string]("xyz") == 0)
template `isnot` *(x, y: untyped): untyped = not (x is y)
  ## Negated version of `is`. Equivalent to ``not(x is y)``.

proc `of` *[T, S](x: typeDesc[T], y: typeDesc[S]): bool {.magic: "Of", noSideEffect.}
proc `of` *[T, S](x: T, y: typeDesc[S]): bool {.magic: "Of", noSideEffect.}
proc `of` *[T, S](x: T, y: S): bool {.magic: "Of", noSideEffect.}
  ## Checks if `x` has a type of `y`
  ##
  ## .. code-block:: Nim
  ##   assert(FloatingPointError of Exception)
  ##   assert(DivByZeroError of Exception)

proc cmp*[T](x, y: T): int {.procvar.} =
  ## Generic compare proc. Returns a value < 0 iff x < y, a value > 0 iff x > y
  ## and 0 iff x == y. This is useful for writing generic algorithms without
  ## performance loss. This generic implementation uses the `==` and `<`
  ## operators.
  ##
  ## .. code-block:: Nim
  ##  import algorithm
  ##  echo sorted(@[4,2,6,5,8,7], cmp[int])
  if x == y: return 0
  if x < y: return -1
  return 1

# TODO: later, 2933
#proc cmp*(x, y: string): int {.noSideEffect, procvar.}
#  ## Compare proc for strings. More efficient than the generic version.
