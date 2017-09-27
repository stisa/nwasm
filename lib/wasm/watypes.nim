import waenums, wanodes

type 
  bytes* = string
  varuint32* = int
  varuint1* = int
  
  Module* = ref object
    magic*: int #uint32
    version*: int #uint32
    types*: TypeSection
    imports*: ImportSection
    functions*: FunctionSection
    tables*: TableSection
    memory*: MemorySection
    globals*: GlobalSection
    exports*: ExportSection
    start*: StartSection
    elements*: ElementSection
    codes*: CodeSection
    datas*:DataSection
    custom*: seq[CustomSection]
  
  # Sections  
  CustomSection* = ref object of RootObj
    name*: string
    #TODO:    

  TypeSection* = ref object
    entries*: seq[FuncType]
  ImportSection* = ref object
    entries*: seq[ImportEntry]
  FunctionSection* = ref object
    entries*: seq[Natural] 
  TableSection* = ref object
    entries*: seq[TableType]
  MemorySection* = ref object
    entries*: seq[MemoryType]
  GlobalSection* = ref object
    entries*: seq[GlobalVariable]
  ExportSection* = ref object
    entries*: seq[ExportEntry]
  StartSection* = ref object
    index*: varuint32
  ElementSection* = ref object
    entries*: seq[ElemSegment]
  CodeSection* = ref object
    entries*: seq[FunctionBody]
  DataSection* = ref object
    entries*: seq[DataSegment]

  FuncType* = ref object
    ## The description of a function signature.
    form* : WasmType # varint7
    params*: seq[ValueType] # varuint32
    returns*: seq[ValueType] #varuint1

  GlobalType* = ref object
    ## The description of a global variable.
    contentType* : ValueType
    mutability* : bool

  ResizableLimits* = ref object
    ## A packed tuple that describes the limits of a table or memory
    flags*: range[0..1 ]#varuint1 # 0 or 1 ( 1-> maximum field is present )
    initial*: varuint32
    maximum*: varuint32 #?

  TableType* = ref object
    ## The description of a table.
    elementType*: ElemType
    limits*: ResizableLimits

  MemoryType* = ref object
    ## The description of a memory.
    limits*: ResizableLimits

  #InitExpr* = Op
  #  # TODO: restrict to actual function

  GlobalVariable* = ref object
    gtype*: GlobalType
    init*: WasmNode

  ImportEntry* = ref object
    module*: string #module string of module_len bytes
    field*: string #field name string of field_len bytes
    case kind* : ExternalKind # the kind of definition being imported
    of ExternalKind.Function :
      typeindex* : varuint32
    of ExternalKind.Table:
      ttype*: TableType
    of ExternalKind.Memory:
      mtype*: MemoryType
    of ExternalKind.Global:
      gtype*: GlobalType

  ExportEntry* = ref object
    field*: string #field name string of field_len bytes
    kind* : ExternalKind # the kind of definition being exported
    index*: varuint32
    isImported*: bool

  ElemSegment* = ref object
    index*: varuint32
    offset*: WasmNode
    elems*: seq[varuint32]

  DataSegment* = ref object
    index*: varuint32
    offset*: WasmNode
    #    size*: int
    data*: bytes

  LocalEntry* = ref object
    count*: varuint32
    vtype*: ValueType

  FunctionBody* = ref object
    locals*: seq[LocalEntry]
    code*: seq[WasmNode]
    #ends*: char # 0x0b
