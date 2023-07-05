package nox

when ODIN_OS == .Windows do foreign import llvm "/../vendor/llvm/lib/windows/LLVM-C.lib"
when ODIN_OS == .Linux do foreign import llvm "libLLVMCore.a" // This is just a stub as we do not actually import 'libLLVMCore' here. This is done outside.

LLVMContextRef :: distinct rawptr;
LLVMAttributeRef :: distinct rawptr;
LLVMTargetRef :: distinct rawptr;
LLVMTargetMachineRef :: distinct rawptr;
LLVMTargetDataRef :: distinct rawptr;
LLVMModuleRef :: distinct rawptr;
LLVMPassManagerRef :: distinct rawptr;
LLVMBuilderRef :: distinct rawptr;
LLVMBasicBlockRef :: distinct rawptr;
LLVMValueRef :: distinct rawptr;
LLVMTypeRef :: distinct rawptr;
LLVMBool :: distinct b32;
LLVMAttributeIndex :: u32;

LLVMDIBuilderRef :: distinct rawptr;
LLVMMetadataRef :: distinct rawptr;

LLVMAttributeFunctionIndex: i32 = -1;

LLVMModuleFlagBehavior :: enum {
  Error,
  Warning,
  Require,
  Override,
  Append,
  AppendUnique,
}

LLVMTypeKind :: enum {
  Void,
  Half,
  Float,
  Double,
  X86_FP80,
  FP128,
  PPC_FP128,
  Label,
  Integer,
  Function,
  Struct,
  Array,
  Pointer,
  Vector,
  Metadata,
  X86_MMX,
  Token,
  ScalableVector,
  BFloat,
  X86_AMX,
}

LLVMIntPredicate :: enum {
  EQ = 32,
  NE,     
  UGT,    
  UGE,    
  ULT,    
  ULE,    
  SGT,    
  SGE,    
  SLT,    
  SLE,
}

LLVMRealPredicate :: enum {
  PredicateFalse,
  OEQ,           
  OGT,           
  OGE,           
  OLT,           
  OLE,           
  ONE,           
  ORD,           
  UNO,           
  UEQ,           
  UGT,           
  UGE,           
  ULT,           
  ULE,           
  UNE,           
  PredicateTrue,
}

LLVMLinkage :: enum {
  External,
  AvailableExternally,
  LinkOnceAny,
  LinkOnceODR,
  LinkOnceODRAutoHide,
  WeakAny,
  WeakODR,
  Appending,
  Internal,
  Private,
  DLLImport,
  DLLExport,
  ExternalWeak,
  Ghost,
  Common,
  LinkerPrivate,
  LinkerPrivateWeak,
};

LLVMCallConv :: enum {
  C             = 0,
  Fast          = 8,
  Cold          = 9,
  GHC           = 10,
  HiPE          = 11,
  WebKitJS      = 12,
  AnyReg        = 13,
  PreserveMost  = 14,
  PreserveAll   = 15,
  Swift         = 16,
  CXXFASTTLS    = 17,
  X86Stdcall    = 64,
  X86Fastcall   = 65,
  ARMAPCS       = 66,
  ARMAAPCS      = 67,
  ARMAAPCSVFP   = 68,
  MSP430INTR    = 69,
  X86ThisCall   = 70,
  PTXKernel     = 71,
  PTXDevice     = 72,
  SPIRFUNC      = 75,
  SPIRKERNEL    = 76,
  IntelOCLBI    = 77,
  X8664SysV     = 78,
  Win64         = 79,
  X86VectorCall = 80,
  HHVM          = 81,
  HHVMC         = 82,
  X86INTR       = 83,
  AVRINTR       = 84,
  AVRSIGNAL     = 85,
  AVRBUILTIN    = 86,
  AMDGPUVS      = 87,
  AMDGPUGS      = 88,
  AMDGPUPS      = 89,
  AMDGPUCS      = 90,
  AMDGPUKERNEL  = 91,
  X86RegCall    = 92,
  AMDGPUHS      = 93,
  MSP430BUILTIN = 94,
  AMDGPULS      = 95,
  AMDGPUES      = 96,
};

LLVMVerifierFailureAction :: enum {
  AbortProcess,
  PrintMessage,
  ReturnStatus,
}

LLVMCodeGenOptLevel :: enum {
  None,
  Less,
  Default,
  Aggressive,
}

LLVMRelocMode :: enum {
  Default,
  Static,
  PIC,
  DynamicNoPic,
  ROPI,
  RWPI,
  ROPI_RWPI,
}

LLVMCodeModel :: enum {
  Default,
  JITDefault,
  Tiny,
  Small,
  Kernel,
  Medium,
  Large,
}

LLVMCodeGenFileType :: enum {
  AssemblyFile,
  ObjectFile,
}

LLVMDIFlags :: enum {
  Zero = 0,
  Private = 1,
  Protected = 2,
  Public = 3,
  FwdDecl = 1 << 2,
  AppleBlock = 1 << 3,
  ReservedBit4 = 1 << 4,
  Virtual = 1 << 5,
  Artificial = 1 << 6,
  Explicit = 1 << 7,
  Prototyped = 1 << 8,
  ObjcClassComplete = 1 << 9,
  ObjectPointer = 1 << 10,
  Vector = 1 << 11,
  StaticMember = 1 << 12,
  LValueReference = 1 << 13,
  RValueReference = 1 << 14,
  Reserved = 1 << 15,
  SingleInheritance = 1 << 16,
  MultipleInheritance = 2 << 16,
  VirtualInheritance = 3 << 16,
  IntroducedVirtual = 1 << 18,
  BitField = 1 << 19,
  NoReturn = 1 << 20,
  TypePassByValue = 1 << 22,
  TypePassByReference = 1 << 23,
  EnumClass = 1 << 24,
  FixedEnum = EnumClass,
  Thunk = 1 << 25,
  NonTrivial = 1 << 26,
  BigEndian = 1 << 27,
  LittleEndian = 1 << 28,
  IndirectVirtualBase = (1 << 2) | (1 << 5),
  Accessibility = Private | Protected | Public,
  PtrToMemberRep = SingleInheritance | MultipleInheritance | VirtualInheritance,
}

LLVMDWARFSourceLanguage :: enum {
  C89,
  C,
  Ada83,
  C_plus_plus,
  Cobol74,
  Cobol85,
  Fortran77,
  Fortran90,
  Pascal83,
  Modula2,
  Java,
  C99,
  Ada95,
  Fortran95,
  PLI,
  ObjC,
  ObjC_plus_plus,
  UPC,
  D,
  Python,
  OpenCL,
  Go,
  Modula3,
  Haskell,
  C_plus_plus_03,
  C_plus_plus_11,
  OCaml,
  Rust,
  C11,
  Swift,
  Julia,
  Dylan,
  C_plus_plus_14,
  Fortran03,
  Fortran08,
  RenderScript,
  BLISS,
  Mips_Assembler,
  GOOGLE_RenderScript,
  BORLAND_Delphi,
}

LLVMDWARFEmissionKind :: enum {
  None = 0,
  Full,
  LineTablesOnly,
}

LLVMDWARFTypeEncoding :: enum {
	Address = 1,
	Boolean = 2,
	ComplexFloat = 3,
	Float = 4,
	Signed = 5,
	SignedChar = 6,
	Unsigned = 7,
	UnsignedChar = 8,
	ImaginaryFloat = 9,
	PackedDecimal = 10,
	NumericString = 11,
	Edited = 12,
	SignedFixed = 13,
	UnsignedFixed = 14,
	DecimalFloat = 15,
	Utf = 16,
	LoUser = 128,
	HiUser = 255,
}

LLVMDWARFTag :: enum {
  Structure = 0x13,
  Union = 0x17,
}

foreign llvm {
  LLVMInitializeAArch64TargetInfo :: proc() ---;
  LLVMInitializeAMDGPUTargetInfo :: proc() ---;
  LLVMInitializeARMTargetInfo :: proc() ---;
  LLVMInitializeAVRTargetInfo :: proc() ---;
  LLVMInitializeBPFTargetInfo :: proc() ---;
  LLVMInitializeHexagonTargetInfo :: proc() ---;
  LLVMInitializeLanaiTargetInfo :: proc() ---;
  LLVMInitializeMipsTargetInfo :: proc() ---;
  LLVMInitializeMSP430TargetInfo :: proc() ---;
  LLVMInitializeNVPTXTargetInfo :: proc() ---;
  LLVMInitializePowerPCTargetInfo :: proc() ---;
  LLVMInitializeRISCVTargetInfo :: proc() ---;
  LLVMInitializeSparcTargetInfo :: proc() ---;
  LLVMInitializeSystemZTargetInfo :: proc() ---;
  LLVMInitializeWebAssemblyTargetInfo :: proc() ---;
  LLVMInitializeX86TargetInfo :: proc() ---;
  LLVMInitializeXCoreTargetInfo :: proc() ---;

  LLVMInitializeAArch64Target :: proc() ---;
  LLVMInitializeAMDGPUTarget :: proc() ---;
  LLVMInitializeARMTarget :: proc() ---;
  LLVMInitializeAVRTarget :: proc() ---;
  LLVMInitializeBPFTarget :: proc() ---;
  LLVMInitializeHexagonTarget :: proc() ---;
  LLVMInitializeLanaiTarget :: proc() ---;
  LLVMInitializeMipsTarget :: proc() ---;
  LLVMInitializeMSP430Target :: proc() ---;
  LLVMInitializeNVPTXTarget :: proc() ---;
  LLVMInitializePowerPCTarget :: proc() ---;
  LLVMInitializeRISCVTarget :: proc() ---;
  LLVMInitializeSparcTarget :: proc() ---;
  LLVMInitializeSystemZTarget :: proc() ---;
  LLVMInitializeWebAssemblyTarget :: proc() ---;
  LLVMInitializeX86Target :: proc() ---;
  LLVMInitializeXCoreTarget :: proc() ---;

  LLVMInitializeAArch64TargetMC :: proc() ---;
  LLVMInitializeAMDGPUTargetMC :: proc() ---;
  LLVMInitializeARMTargetMC :: proc() ---;
  LLVMInitializeAVRTargetMC :: proc() ---;
  LLVMInitializeBPFTargetMC :: proc() ---;
  LLVMInitializeHexagonTargetMC :: proc() ---;
  LLVMInitializeLanaiTargetMC :: proc() ---;
  LLVMInitializeMipsTargetMC :: proc() ---;
  LLVMInitializeMSP430TargetMC :: proc() ---;
  LLVMInitializeNVPTXTargetMC :: proc() ---;
  LLVMInitializePowerPCTargetMC :: proc() ---;
  LLVMInitializeRISCVTargetMC :: proc() ---;
  LLVMInitializeSparcTargetMC :: proc() ---;
  LLVMInitializeSystemZTargetMC :: proc() ---;
  LLVMInitializeWebAssemblyTargetMC :: proc() ---;
  LLVMInitializeX86TargetMC :: proc() ---;
  LLVMInitializeXCoreTargetMC :: proc() ---;

  LLVMInitializeAArch64AsmPrinter :: proc() ---;
  LLVMInitializeAMDGPUAsmPrinter :: proc() ---;
  LLVMInitializeARMAsmPrinter :: proc() ---;
  LLVMInitializeAVRAsmPrinter :: proc() ---;
  LLVMInitializeBPFAsmPrinter :: proc() ---;
  LLVMInitializeHexagonAsmPrinter :: proc() ---;
  LLVMInitializeLanaiAsmPrinter :: proc() ---;
  LLVMInitializeMipsAsmPrinter :: proc() ---;
  LLVMInitializeMSP430AsmPrinter :: proc() ---;
  LLVMInitializeNVPTXAsmPrinter :: proc() ---;
  LLVMInitializePowerPCAsmPrinter :: proc() ---;
  LLVMInitializeRISCVAsmPrinter :: proc() ---;
  LLVMInitializeSparcAsmPrinter :: proc() ---;
  LLVMInitializeSystemZAsmPrinter :: proc() ---;
  LLVMInitializeWebAssemblyAsmPrinter :: proc() ---;
  LLVMInitializeX86AsmPrinter :: proc() ---;
  LLVMInitializeXCoreAsmPrinter :: proc() ---;

  LLVMInitializeAArch64AsmParser :: proc() ---;
  LLVMInitializeAMDGPUAsmParser :: proc() ---;
  LLVMInitializeARMAsmParser :: proc() ---;
  LLVMInitializeAVRAsmParser :: proc() ---;
  LLVMInitializeBPFAsmParser :: proc() ---;
  LLVMInitializeHexagonAsmParser :: proc() ---;
  LLVMInitializeLanaiAsmParser :: proc() ---;
  LLVMInitializeMipsAsmParser :: proc() ---;
  LLVMInitializeMSP430AsmParser :: proc() ---;
  LLVMInitializeNVPTXAsmParser :: proc() ---;
  LLVMInitializePowerPCAsmParser :: proc() ---;
  LLVMInitializeRISCVAsmParser :: proc() ---;
  LLVMInitializeSparcAsmParser :: proc() ---;
  LLVMInitializeSystemZAsmParser :: proc() ---;
  LLVMInitializeWebAssemblyAsmParser :: proc() ---;
  LLVMInitializeX86AsmParser :: proc() ---;
  LLVMInitializeXCoreAsmParser :: proc() ---;

  LLVMInitializeAArch64Disassembler :: proc() ---;
  LLVMInitializeAMDGPUDisassembler :: proc() ---;
  LLVMInitializeARMDisassembler :: proc() ---;
  LLVMInitializeAVRDisassembler :: proc() ---;
  LLVMInitializeBPFDisassembler :: proc() ---;
  LLVMInitializeHexagonDisassembler :: proc() ---;
  LLVMInitializeLanaiDisassembler :: proc() ---;
  LLVMInitializeMipsDisassembler :: proc() ---;
  LLVMInitializeMSP430Disassembler :: proc() ---;
  LLVMInitializeNVPTXDisassembler :: proc() ---;
  LLVMInitializePowerPCDisassembler :: proc() ---;
  LLVMInitializeRISCVDisassembler :: proc() ---;
  LLVMInitializeSparcDisassembler :: proc() ---;
  LLVMInitializeSystemZDisassembler :: proc() ---;
  LLVMInitializeWebAssemblyDisassembler :: proc() ---;
  LLVMInitializeX86Disassembler :: proc() ---;
  LLVMInitializeXCoreDisassembler :: proc() ---;
}

foreign llvm {
  LLVMShutdown :: proc() ---;
  LLVMDisposeMessage :: proc(message: cstring) ---;

  LLVMGetGlobalContext :: proc() -> LLVMContextRef ---;

  LLVMGetEnumAttributeKindForName :: proc(name: cstring, length: u64) -> u32 ---;
  LLVMCreateEnumAttribute :: proc(ctx: LLVMContextRef, kind_id: u32, value: u64) -> LLVMAttributeRef ---;
  LLVMCreateTypeAttribute :: proc(ctx: LLVMContextRef, kind_id: u32, type_ref: LLVMTypeRef) -> LLVMAttributeRef ---;

  LLVMGetDefaultTargetTriple :: proc() -> cstring ---;
  LLVMGetTargetFromTriple :: proc(triple: cstring, target: ^LLVMTargetRef, error_message: ^cstring) -> LLVMBool ---;
  LLVMCreateTargetMachine :: proc(target: LLVMTargetRef, triple: cstring, cpu: cstring, features: cstring, level: LLVMCodeGenOptLevel, reloc: LLVMRelocMode, code_model: LLVMCodeModel) -> LLVMTargetMachineRef ---;
  LLVMDisposeTargetMachine :: proc(machine: LLVMTargetMachineRef) ---;
  LLVMCreateTargetDataLayout :: proc(machine: LLVMTargetMachineRef) -> LLVMTargetDataRef ---;
  LLVMDisposeTargetData :: proc(data: LLVMTargetDataRef) ---;
  LLVMTargetMachineEmitToFile :: proc(machine: LLVMTargetMachineRef, module: LLVMModuleRef, filename: cstring, codegen: LLVMCodeGenFileType, error_message: ^cstring) -> LLVMBool ---;
  LLVMGetHostCPUFeatures :: proc() -> cstring ---;

  LLVMModuleCreateWithName :: proc(module_id: cstring) -> LLVMModuleRef ---;
  LLVMDisposeModule :: proc(module: LLVMModuleRef) ---;
  LLVMVerifyModule :: proc(module: LLVMModuleRef, action: LLVMVerifierFailureAction, out_message: ^cstring) -> LLVMBool ---;
  LLVMPrintModuleToFile :: proc(module: LLVMModuleRef, filename: cstring, error_message: ^cstring) ---;
  LLVMSetModuleDataLayout :: proc(module: LLVMModuleRef, data_layout: LLVMTargetDataRef) ---;
  LLVMSetTarget :: proc(module: LLVMModuleRef, triple: cstring) ---;
  LLVMAddModuleFlag :: proc(module: LLVMModuleRef, behaviour: LLVMModuleFlagBehavior, key: cstring, key_length: u64, value: LLVMMetadataRef) ---;

  LLVMCreatePassManager :: proc() -> LLVMPassManagerRef ---;
  LLVMRunPassManager :: proc(pm: LLVMPassManagerRef, module: LLVMModuleRef) -> LLVMBool ---;
  LLVMCreateFunctionPassManagerForModule :: proc(module: LLVMModuleRef) -> LLVMPassManagerRef ---;
  LLVMInitializeFunctionPassManager :: proc(fpm: LLVMPassManagerRef) -> LLVMBool ---;
  LLVMRunFunctionPassManager :: proc(fpm: LLVMPassManagerRef, function: LLVMValueRef) -> LLVMBool ---;
  LLVMFinalizeFunctionPassManager :: proc(fpm: LLVMPassManagerRef) -> LLVMBool ---;
  LLVMDisposePassManager :: proc(pm: LLVMPassManagerRef) ---;

  LLVMAddAnalysisPasses :: proc(machine: LLVMTargetMachineRef, pm: LLVMPassManagerRef) ---;
  LLVMAddConstantMergePass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddMergeFunctionsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddCalledValuePropagationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddDeadArgEliminationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddFunctionAttrsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddFunctionInliningPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddAlwaysInlinerPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddGlobalDCEPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddGlobalOptimizerPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddIPSCCPPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddStripDeadPrototypesPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddAggressiveDCEPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddDCEPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddBitTrackingDCEPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddAlignmentFromAssumptionsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddCFGSimplificationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddDeadStoreEliminationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddScalarizerPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddMergedLoadStoreMotionPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddGVNPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddNewGVNPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddIndVarSimplifyPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddInstructionCombiningPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddInstructionSimplifyPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddJumpThreadingPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLICMPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopDeletionPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopIdiomPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopRotatePass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopRerollPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopUnrollPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopUnrollAndJamPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLowerAtomicPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddMemCpyOptPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddPartiallyInlineLibCallsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddReassociatePass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddSCCPPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddScalarReplAggregatesPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddScalarReplAggregatesPassSSA :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddScalarReplAggregatesPassWithThreshold :: proc(pm: LLVMPassManagerRef, threshold: i32) ---;
  LLVMAddSimplifyLibCallsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddTailCallEliminationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddDemoteMemoryToRegisterPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddVerifierPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddCorrelatedValuePropagationPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddEarlyCSEPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddEarlyCSEMemSSAPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLowerExpectIntrinsicPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLowerConstantIntrinsicsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddTypeBasedAliasAnalysisPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddScopedNoAliasAAPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddUnifyFunctionExitNodesPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLowerSwitchPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddPromoteMemoryToRegisterPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddAddDiscriminatorsPass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddLoopVectorizePass :: proc(pm: LLVMPassManagerRef) ---;
  LLVMAddSLPVectorizePass :: proc(pm: LLVMPassManagerRef) ---;

  LLVMTypeOf :: proc(value: LLVMValueRef) -> LLVMTypeRef ---;
  LLVMGetTypeKind :: proc(type: LLVMTypeRef) -> LLVMTypeKind ---;
  LLVMSetValueName2 :: proc(value: LLVMValueRef, name: cstring, length: uint) ---;
  LLVMIsConstant :: proc(value: LLVMValueRef) -> LLVMBool ---;
  LLVMIsUndef :: proc(value: LLVMValueRef) -> LLVMBool ---;
  LLVMIsPoison :: proc(value: LLVMValueRef) -> LLVMBool ---;
  LLVMIsNull :: proc(value: LLVMValueRef) -> LLVMBool ---;

  LLVMAddFunction :: proc(module: LLVMModuleRef, name: cstring, function_type: LLVMTypeRef) -> LLVMValueRef ---;
  LLVMVerifyFunction :: proc(function: LLVMValueRef, action: LLVMVerifierFailureAction) -> LLVMBool---;
  LLVMAppendBasicBlock :: proc(function: LLVMValueRef, name: cstring) -> LLVMBasicBlockRef---;
  LLVMDeleteBasicBlock :: proc(block: LLVMBasicBlockRef) ---;
  LLVMGetFirstInstruction :: proc(block: LLVMBasicBlockRef) -> LLVMValueRef---;
  LLVMGetLastInstruction :: proc(block: LLVMBasicBlockRef) -> LLVMValueRef---;
  LLVMCountParams :: proc(function: LLVMValueRef) -> u32 ---;
  LLVMGetParam :: proc(function: LLVMValueRef, index: u32) -> LLVMValueRef ---;
  LLVMGetFirstParam :: proc(function: LLVMValueRef) -> LLVMValueRef ---;
  LLVMGetLastParam :: proc(function: LLVMValueRef) -> LLVMValueRef ---;
  LLVMGetNextParam :: proc(parameter: LLVMValueRef) -> LLVMValueRef ---;
  LLVMGetPreviousParam :: proc(parameter: LLVMValueRef) -> LLVMValueRef ---;
  LLVMSetFunctionCallConv :: proc(function: LLVMValueRef, call_conv: LLVMCallConv) ---;
  LLVMAddAttributeAtIndex :: proc(function: LLVMValueRef, index: LLVMAttributeIndex, attribute: LLVMAttributeRef) ---;
  LLVMLookupIntrinsicID :: proc(name: cstring, name_length: u64) -> u32 ---;
  LLVMGetIntrinsicDeclaration :: proc(module: LLVMModuleRef, id: u32, param_types: [^]LLVMTypeRef, param_count: u64) -> LLVMValueRef ---;
  LLVMIntrinsicGetType :: proc(ctx: LLVMContextRef, id: u32, param_types: [^]LLVMTypeRef, param_count: u64) -> LLVMTypeRef ---;

  LLVMAddGlobal :: proc(module: LLVMModuleRef, type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMSetInitializer:: proc(global_variable: LLVMValueRef, contant_value: LLVMValueRef) ---;
  LLVMSetGlobalConstant :: proc(global_variable: LLVMValueRef, is_constant: LLVMBool) ---;
  LLVMSetThreadLocal :: proc(global_variable: LLVMValueRef, is_thread_local: LLVMBool) ---;
  
  LLVMSetLinkage :: proc(global: LLVMValueRef, linkage: LLVMLinkage) ---;
  LLVMSetSection :: proc(global: LLVMValueRef, section: cstring) ---;

  LLVMCreateBuilder :: proc() -> LLVMBuilderRef ---;
  LLVMDisposeBuilder :: proc(builder: LLVMBuilderRef) ---;
  LLVMPositionBuilderAtEnd :: proc(builder: LLVMBuilderRef, block: LLVMBasicBlockRef) ---;
  LLVMSetCurrentDebugLocation2 :: proc(builder: LLVMBuilderRef, location: LLVMMetadataRef) ---;

  LLVMValueAsMetadata :: proc(value: LLVMValueRef) -> LLVMMetadataRef ---;
  LLVMIsATerminatorInst :: proc(instruction: LLVMValueRef) -> LLVMValueRef ---;

  LLVMBuildRetVoid :: proc(builder: LLVMBuilderRef) -> LLVMValueRef ---;
  LLVMBuildRet :: proc(builder: LLVMBuilderRef, return_value: LLVMValueRef) -> LLVMValueRef ---;
  LLVMBuildBr :: proc(builder: LLVMBuilderRef, destination: LLVMBasicBlockRef) -> LLVMValueRef ---;
  LLVMBuildCondBr :: proc(builder: LLVMBuilderRef, condition: LLVMValueRef, then_block: LLVMBasicBlockRef, else_block: LLVMBasicBlockRef) -> LLVMValueRef ---;
  LLVMBuildSwitch :: proc(builder: LLVMBuilderRef, v: LLVMValueRef, default: LLVMBasicBlockRef, num_cases: u32) -> LLVMValueRef ---;
  LLVMBuildAlloca :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildStore :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, pointer: LLVMValueRef) -> LLVMValueRef ---;
  LLVMBuildLoad2 :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, pointer_value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildGEP2 :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, pointer: LLVMValueRef, indices: [^]LLVMValueRef, num_indices: u32, name: cstring) -> LLVMValueRef ---;
  LLVMBuildInBoundsGEP2 :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, pointer: LLVMValueRef, indices: [^]LLVMValueRef, num_indices: u32, name: cstring) -> LLVMValueRef ---;
  LLVMBuildStructGEP2 :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, pointer: LLVMValueRef, index: u32, name: cstring) -> LLVMValueRef ---;
  LLVMBuildGlobalStringPtr :: proc(builder: LLVMBuilderRef, str: cstring, name: cstring) -> LLVMValueRef ---;
  LLVMBuildTrunc :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildZExt :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSExt :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFPToUI :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFPToSI :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildUIToFP :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSIToFP :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFPTrunc :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFPExt :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildPtrToInt :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildIntToPtr :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildBitCast :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, destination_type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildAdd :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNSWAdd :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNUWAdd :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFAdd :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSub :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNSWSub :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNUWSub :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFSub :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildMul :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNSWMul :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNUWMul :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFMul :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildUDiv :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildExactUDiv :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSDiv :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildExactSDiv :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFDiv :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildURem :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSRem :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFRem :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildShl :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildLShr :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildAShr :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildAnd :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildOr :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildXor :: proc(builder: LLVMBuilderRef, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNeg :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNSWNeg :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNUWNeg :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFNeg :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildNot :: proc(builder: LLVMBuilderRef, value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildICmp :: proc(builder: LLVMBuilderRef, operator: LLVMIntPredicate, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildFCmp :: proc(builder: LLVMBuilderRef, operator: LLVMRealPredicate, lhs: LLVMValueRef, rhs: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildPhi :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildCall2 :: proc(builder: LLVMBuilderRef, type: LLVMTypeRef, function: LLVMValueRef, args: [^]LLVMValueRef, num_args: u32, name: cstring) -> LLVMValueRef ---;
  LLVMBuildSelect :: proc(builder: LLVMBuilderRef, condition: LLVMValueRef, then_value: LLVMValueRef, else_value: LLVMValueRef, name: cstring) -> LLVMValueRef ---;
  LLVMBuildInsertValue :: proc(builder: LLVMBuilderRef, aggregate_value: LLVMValueRef, element_value: LLVMValueRef, index: u32, name: cstring) -> LLVMValueRef ---;
  LLVMBuildExtractValue :: proc(builder: LLVMBuilderRef, aggregate_value: LLVMValueRef, index: u32, name: cstring) -> LLVMValueRef ---;

  LLVMAddCase :: proc(switch_value: LLVMValueRef, on_value: LLVMValueRef, destination: LLVMBasicBlockRef) ---;
  LLVMAddIncoming :: proc(phi_value: LLVMValueRef, incoming_values: [^]LLVMValueRef, incoming_blocks: [^]LLVMBasicBlockRef, count: u32) ---;

  LLVMVoidType :: proc() -> LLVMTypeRef ---;
  LLVMInt1Type :: proc() -> LLVMTypeRef ---;
  LLVMInt8Type :: proc() -> LLVMTypeRef ---;
  LLVMInt16Type :: proc() -> LLVMTypeRef ---;
  LLVMInt32Type :: proc() -> LLVMTypeRef ---;
  LLVMInt64Type :: proc() -> LLVMTypeRef ---;
  LLVMInt128Type :: proc() -> LLVMTypeRef ---;
  LLVMHalfType :: proc() -> LLVMTypeRef ---;
  LLVMFloatType :: proc() -> LLVMTypeRef ---;
  LLVMDoubleType :: proc() -> LLVMTypeRef ---;
  LLVMPointerType :: proc(element_type: LLVMTypeRef, address_space: u32) -> LLVMTypeRef ---;
  LLVMArrayType :: proc(element_type: LLVMTypeRef, element_count: u32) -> LLVMTypeRef ---;
  LLVMStructType :: proc(element_types: [^]LLVMTypeRef, element_count: u32, packed: LLVMBool) -> LLVMTypeRef ---;
  LLVMStructCreateNamed :: proc(ctx: LLVMContextRef, name: cstring) -> LLVMTypeRef ---;
  LLVMStructSetBody :: proc(type: LLVMTypeRef, element_types: [^]LLVMTypeRef, element_count: u32, packed: LLVMBool) ---;
  LLVMFunctionType :: proc(return_type: LLVMTypeRef, param_types: [^]LLVMTypeRef, param_count: u32, is_var_arg: LLVMBool) -> LLVMTypeRef ---;

  LLVMGetUndef :: proc(type: LLVMTypeRef) -> LLVMValueRef ---;
  LLVMConstNull :: proc(type: LLVMTypeRef) -> LLVMValueRef ---;
  LLVMConstInt :: proc(int_type: LLVMTypeRef, n: u64, sign_extend: LLVMBool) -> LLVMValueRef ---;
  LLVMConstIntOfArbitraryPrecision :: proc(int_type: LLVMTypeRef, number_of_words: u32, words: [^]u64) -> LLVMValueRef ---;
  LLVMConstAllOnes :: proc(int_type: LLVMTypeRef) -> LLVMValueRef ---;
  LLVMConstReal :: proc(float_type: LLVMTypeRef, n: f64) -> LLVMValueRef ---;
  LLVMConstArray :: proc(element_type: LLVMTypeRef, constant_values: [^]LLVMValueRef, constant_count: u32) -> LLVMValueRef ---;
  LLVMConstStruct :: proc(constant_values: [^]LLVMValueRef, constant_count: u32, packed: LLVMBool) -> LLVMValueRef ---;
  LLVMConstNamedStruct :: proc(struct_type: LLVMTypeRef, constant_values: [^]LLVMValueRef, constant_count: u32) -> LLVMValueRef ---;
}

foreign llvm {
  LLVMCreateDIBuilder :: proc(module: LLVMModuleRef) -> LLVMDIBuilderRef ---;
  LLVMDisposeDIBuilder :: proc(builder: LLVMDIBuilderRef) ---;
  LLVMDIBuilderFinalize :: proc(builder: LLVMDIBuilderRef) ---;

  LLVMDIScopeGetFile :: proc(scope: LLVMMetadataRef) -> LLVMMetadataRef ---;

  LLVMDIBuilderCreateCompileUnit :: proc(
    builder: LLVMDIBuilderRef,
    lang: LLVMDWARFSourceLanguage,
    file: LLVMMetadataRef,
    producer: cstring,
    producer_length: u64,
    is_optimized: LLVMBool,
    flags: cstring,
    flags_length: u64,
    runtime_version: u32,
    split_name: cstring,
    split_name_length: u64,
    kind: LLVMDWARFEmissionKind,
    dwo_id: u32,
    split_debug_inlining: LLVMBool,
    debug_info_for_profiling: LLVMBool,
    sys_root: cstring,
    sys_root_length: u64,
    sdk: cstring,
    sdk_length: u64,
  ) -> LLVMMetadataRef---;
  LLVMDIBuilderCreateFile :: proc(builder: LLVMDIBuilderRef, filename: cstring, filename_length: u64, directory: cstring, directory_length: u64) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateFunction :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    linkage_name: cstring,
    linkage_name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    type: LLVMMetadataRef,
    is_local_to_unit: LLVMBool,
    is_definition: LLVMBool,
    scope_line: u32,
    flags: LLVMDIFlags,
    is_optimized: LLVMBool,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateDebugLocation :: proc(ctx: LLVMContextRef, line: u32, column: u32, scope: LLVMMetadataRef, inlined_at: LLVMMetadataRef) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateAutoVariable :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    type: LLVMMetadataRef,
    always_preserve: LLVMBool,
    flags: LLVMDIFlags,
    align_in_bits: u32,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateParameterVariable :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    argument_number: u32,
    file: LLVMMetadataRef,
    line_number: u32,
    type: LLVMMetadataRef,
    always_preserve: LLVMBool,
    flags: LLVMDIFlags,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateExpression :: proc(builder: LLVMDIBuilderRef, address: ^u64, length: u64) -> LLVMMetadataRef ---;

  LLVMDIBuilderCreateBasicType :: proc(builder: LLVMDIBuilderRef, name: cstring, name_length: u64, size_in_bits: u64, encoding: LLVMDWARFTypeEncoding, flags: LLVMDIFlags) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreatePointerType :: proc(
    builder: LLVMDIBuilderRef,
    pointee_type: LLVMMetadataRef,
    size_in_bits: u64,
    align_in_bits: u32,
    address_space: u32,
    name: cstring,
    name_length: u64,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateStructType :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    size_in_bits: u64,
    align_in_bits: u32,
    flags: LLVMDIFlags,
    derived_from: LLVMMetadataRef,
    elements: [^]LLVMMetadataRef,
    element_count: u32,
    runtime_lang: u32,
    vtable_holder: LLVMMetadataRef,
    uniqued_id: cstring,
    uniqued_id_length: u64,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateMemberType :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    size_in_bits: u64,
    align_in_bits: u32,
    offset_in_bits: u64,
    flags: LLVMDIFlags,
    type: LLVMMetadataRef,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateArrayType :: proc(
    builder: LLVMDIBuilderRef,
    size: u64,
    align_in_bits: u32,
    type: LLVMMetadataRef,
    subscripts: [^]LLVMMetadataRef,
    num_subscripts: u32,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateUnionType :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    size_in_bits: u64,
    align_in_bits: u32,
    flags: LLVMDIFlags,
    elements: [^]LLVMMetadataRef,
    element_count: u32,
    runtime_lang: u32,
    uniqued_id: cstring,
    uniqued_id_length: u64,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateEnumerationType :: proc(
    builder: LLVMDIBuilderRef,
    scope: LLVMMetadataRef,
    name: cstring,
    name_length: u64,
    file: LLVMMetadataRef,
    line_number: u32,
    size_in_bits: u64,
    align_in_bits: u32,
    elements: [^]LLVMMetadataRef,
    element_count: u32,
    class_type: LLVMMetadataRef,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateEnumerator :: proc(builder: LLVMDIBuilderRef, name: cstring, name_length: u64, value: i64, is_unsigned: LLVMBool) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateReplaceableCompositeType :: proc(
    builder: LLVMDIBuilderRef,
    tag: LLVMDWARFTag,
    name: cstring,
    name_length: u64,
    scope: LLVMMetadataRef,
    file: LLVMMetadataRef,
    line_number: u32,
    runtime_lang: u32,
    size_in_bits: u64,
    align_in_bits: u32,
    flags: LLVMDIFlags,
    uniqued_id: cstring,
    uniqued_id_length: u64,
  ) -> LLVMMetadataRef ---;
  LLVMDIBuilderCreateSubroutineType :: proc(builder: LLVMDIBuilderRef, file: LLVMMetadataRef, parameter_types: [^]LLVMMetadataRef, num_parameter_types: u32, flags: LLVMDIFlags) -> LLVMMetadataRef ---;
  
  LLVMDIBuilderGetOrCreateSubrange :: proc(builder: LLVMDIBuilderRef, lower_bound: i64, count: i64) -> LLVMMetadataRef ---;

  LLVMDIBuilderCreateLexicalBlock :: proc(builder: LLVMDIBuilderRef, scope: LLVMMetadataRef, file: LLVMMetadataRef, line: u32, column: u32) -> LLVMMetadataRef ---;

  LLVMMetadataReplaceAllUsesWith :: proc(temp_target_metadata: LLVMMetadataRef, replacement: LLVMMetadataRef) ---;

  LLVMSetSubprogram :: proc(function: LLVMValueRef, sub_program: LLVMMetadataRef) ---;
  LLVMInstructionSetDebugLoc :: proc(instruction: LLVMValueRef, location: LLVMMetadataRef) ---;
  LLVMDIBuilderInsertDeclareAtEnd :: proc(
    builder: LLVMDIBuilderRef,
    storage: LLVMValueRef,
    variable_info: LLVMMetadataRef,
    expr: LLVMMetadataRef,
    debug_location: LLVMMetadataRef,
    block: LLVMBasicBlockRef,
  ) -> LLVMValueRef ---;
  LLVMDIBuilderInsertDbgValueAtEnd :: proc(
    builder: LLVMDIBuilderRef,
    value: LLVMValueRef,
    variable_info: LLVMMetadataRef,
    expr: LLVMMetadataRef,
    debug_location: LLVMMetadataRef,
    block: LLVMBasicBlockRef,
  ) -> LLVMValueRef ---;
}

LLVMInitializeAllTargetInfos :: proc() {
  LLVMInitializeAArch64TargetInfo();
  LLVMInitializeAMDGPUTargetInfo();
  LLVMInitializeARMTargetInfo();
  LLVMInitializeAVRTargetInfo();
  LLVMInitializeBPFTargetInfo();
  LLVMInitializeHexagonTargetInfo();
  LLVMInitializeLanaiTargetInfo();
  LLVMInitializeMipsTargetInfo();
  LLVMInitializeMSP430TargetInfo();
  LLVMInitializeNVPTXTargetInfo();
  LLVMInitializePowerPCTargetInfo();
  LLVMInitializeRISCVTargetInfo();
  LLVMInitializeSparcTargetInfo();
  LLVMInitializeSystemZTargetInfo();
  LLVMInitializeWebAssemblyTargetInfo();
  LLVMInitializeX86TargetInfo();
  LLVMInitializeXCoreTargetInfo();
}

LLVMInitializeAllTargets :: proc() {
  LLVMInitializeAArch64Target();
  LLVMInitializeAMDGPUTarget();
  LLVMInitializeARMTarget();
  LLVMInitializeAVRTarget();
  LLVMInitializeBPFTarget();
  LLVMInitializeHexagonTarget();
  LLVMInitializeLanaiTarget();
  LLVMInitializeMipsTarget();
  LLVMInitializeMSP430Target();
  LLVMInitializeNVPTXTarget();
  LLVMInitializePowerPCTarget();
  LLVMInitializeRISCVTarget();
  LLVMInitializeSparcTarget();
  LLVMInitializeSystemZTarget();
  LLVMInitializeWebAssemblyTarget();
  LLVMInitializeX86Target();
  LLVMInitializeXCoreTarget();
}

LLVMInitializeAllTargetMCs :: proc() {
  LLVMInitializeAArch64TargetMC();
  LLVMInitializeAMDGPUTargetMC();
  LLVMInitializeARMTargetMC();
  LLVMInitializeAVRTargetMC();
  LLVMInitializeBPFTargetMC();
  LLVMInitializeHexagonTargetMC();
  LLVMInitializeLanaiTargetMC();
  LLVMInitializeMipsTargetMC();
  LLVMInitializeMSP430TargetMC();
  LLVMInitializeNVPTXTargetMC();
  LLVMInitializePowerPCTargetMC();
  LLVMInitializeRISCVTargetMC();
  LLVMInitializeSparcTargetMC();
  LLVMInitializeSystemZTargetMC();
  LLVMInitializeWebAssemblyTargetMC();
  LLVMInitializeX86TargetMC();
  LLVMInitializeXCoreTargetMC();
}

LLVMInitializeAllAsmPrinters :: proc() {
  LLVMInitializeAArch64AsmPrinter();
  LLVMInitializeAMDGPUAsmPrinter();
  LLVMInitializeARMAsmPrinter();
  LLVMInitializeAVRAsmPrinter();
  LLVMInitializeBPFAsmPrinter();
  LLVMInitializeHexagonAsmPrinter();
  LLVMInitializeLanaiAsmPrinter();
  LLVMInitializeMipsAsmPrinter();
  LLVMInitializeMSP430AsmPrinter();
  LLVMInitializeNVPTXAsmPrinter();
  LLVMInitializePowerPCAsmPrinter();
  LLVMInitializeRISCVAsmPrinter();
  LLVMInitializeSparcAsmPrinter();
  LLVMInitializeSystemZAsmPrinter();
  LLVMInitializeWebAssemblyAsmPrinter();
  LLVMInitializeX86AsmPrinter();
  LLVMInitializeXCoreAsmPrinter();
}

LLVMInitializeAllAsmParsers :: proc() {
  LLVMInitializeAArch64AsmParser();
  LLVMInitializeAMDGPUAsmParser();
  LLVMInitializeARMAsmParser();
  LLVMInitializeAVRAsmParser();
  LLVMInitializeBPFAsmParser();
  LLVMInitializeHexagonAsmParser();
  LLVMInitializeLanaiAsmParser();
  LLVMInitializeMipsAsmParser();
  LLVMInitializeMSP430AsmParser();
  LLVMInitializePowerPCAsmParser();
  LLVMInitializeRISCVAsmParser();
  LLVMInitializeSparcAsmParser();
  LLVMInitializeSystemZAsmParser();
  LLVMInitializeWebAssemblyAsmParser();
  LLVMInitializeX86AsmParser();
}

LLVMInitializeAllDisassemblers :: proc() {
  LLVMInitializeAArch64Disassembler();
  LLVMInitializeAMDGPUDisassembler();
  LLVMInitializeARMDisassembler();
  LLVMInitializeAVRDisassembler();
  LLVMInitializeBPFDisassembler();
  LLVMInitializeHexagonDisassembler();
  LLVMInitializeLanaiDisassembler();
  LLVMInitializeMipsDisassembler();
  LLVMInitializeMSP430Disassembler();
  LLVMInitializePowerPCDisassembler();
  LLVMInitializeRISCVDisassembler();
  LLVMInitializeSparcDisassembler();
  LLVMInitializeSystemZDisassembler();
  LLVMInitializeWebAssemblyDisassembler();
  LLVMInitializeX86Disassembler();
  LLVMInitializeXCoreDisassembler();
}
