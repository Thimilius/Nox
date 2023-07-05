package nox

import "core:runtime"
import "core:time"
import "core:strings"
import "tracy"

/**
* Represents the mode for loading a value.
*/
Loading_Mode :: enum {
  Load_Value,   // The default for loading values with an address.
  Load_Address, // For getting the address of a value without loading from it.
}

/**
* Represents a local symbol for LLVM.
*/
LLVM_Local_Symbol :: struct {
  llvm_value: LLVMValueRef, // The LLVM value of the local symbol.
  llvm_type: LLVMTypeRef,   // The LLVM type of the local symbol.

  type: ^Type,              // The type of the local symbol.
  is_parameter: bool,       // Is the local symbol a parameter?
}

/**
* Represents an LLVM routine symbol and its LLVM type.
*/
LLVM_Routine_Tuple :: struct {
  llvm_symbol: LLVMValueRef, // The LLVM symbol of the routine.
  llvm_type: LLVMTypeRef,    // The LLVM type of the routine.
}

/**
* Represents a built-in routine for LLVM.
*/
LLVM_Builtin_Routine :: struct {
  symbol: ^Symbol,                // The built-in symbol.
  llvm_tuple: LLVM_Routine_Tuple, // The LLVM symbol.
}

/**
* Represents the key for looking up a vtable. 
*/
VTable_Key :: struct {
  interface_type: ^Type,      // The type of the interface that gets implemented.
  implementation_type: ^Type, // The type implementing the interface.
}

/**
* Represents the kind for a local routine scope.
*/
Routine_Local_Scope_Kind :: enum {
  Block,         // A regular block.
  Control_Block, // A block corresponding to a control statement.
}

/**
* Represents a local routine scope.
*/
Routine_Local_Scope :: struct {
  kind: Routine_Local_Scope_Kind,           // The kind of the local scope.
  parent: ^Routine_Local_Scope,             // The parent scope.

  symbols: map[string]LLVM_Local_Symbol,    // The local symbols defined in this scope.
  deferred_statements: [dynamic]^Statement, // The statements that got deferred in this scope at need to be executed at the end.

  debug: Routine_Debug_Local_Scope,         // The debug information associated with this scope.
}

/**
* Represents the context when generating a routine.
*/
Routine_Context :: struct {
  symbol: ^Symbol,                           // The symbol of the routine.
  llvm_symbol: LLVMValueRef,                 // The LLVM symbol of the routine.
  return_type: ^Type,                        // The return type of the routine.

  local_scope: ^Routine_Local_Scope,         // Holds the deepest local scope in the stack of the routine.
  
  current_loading_mode: Loading_Mode,        // The currently requested loading mode.

  declaration_block: LLVMBasicBlockRef,      // The block where all 'alloca' instructions should be located.
  current_block: LLVMBasicBlockRef,          // The currently active block.
  needs_after_block: bool,                   // Used to check if control flow statements should append a block after themselves.
  last_next_block: LLVMBasicBlockRef,        // Used to transfer out of a scope to the next (this is the most convoluted one as it can mean different things).
  last_break_block: LLVMBasicBlockRef,       // Used to transfer out of a scope throguh 'break'.
  last_continue_block: LLVMBasicBlockRef,    // Used to transfer out of a scope through 'continue'.
  next_fallthrough_block: LLVMBasicBlockRef, // Used to tranfer into next scope through 'fallthrough'.
  
  // NOTE: All these counters are not really necessary for the generation but are quite useful when trying to read the LLVM IR names.
  block_counter: uint,                       // For blocks.
  value_counter: uint,                       // For temporary LLVM values.
  variable_counter: uint,                    // For variables allocated through 'alloca'.
  temp_counter: uint,                        // For general temporaries allocated through 'alloca'.
  byval_counter: uint,                       // For parameters that are passed as pointers.
  sret_counter: uint,                        // For return values that are passed as pointers.
  array_counter: uint,                       // For arrays that are needed (usually as the backing data of slices).
  slice_counter: uint,                       // For slices that are needed.
  compound_counter: uint,                    // For the values of compound expressions.

  // This is a little bit of a hack but works for now.
  is_global_initializer: bool,               // Are we currently generating an initializer for a global?
  is_storing_byval_or_sret: bool,            // Are we currently storing a parameter marked with 'byval' or 'sret'?
  last_resolved_symbol_name: string,         // Holds the last resolved symbol name.
  last_resolved_symbol_was_parameter: bool,  // Was the last resolved symbol a parameter?

  debug: Routine_Context_Debug,              // The debug information associated with the routine.
}

/**
* Represents data needed for generating the LLVM IR.
*/
Generator :: struct {
  storage: ^Type_Storage,                                             // Holds a reference to the type storage.
  resolver_output: Resolver_Output,                                   // Holds the output of the resolver.
  user_entry_point: ^Symbol,                                          // Holds the user defined entry point.

  module: LLVMModuleRef,                                              // Holds the LLVM module used as singular translation unit.
  builder: LLVMBuilderRef,                                            // Holds the LLVM builder used for generating.
  temporary_builder: LLVMBuilderRef,                                  // Holds a temporary LLVM builder.

  llvm_types: map[^Type]LLVMTypeRef,                                  // Holds all types for LLVM.
  llvm_symbols: map[^Symbol]LLVMValueRef,                             // Holds all symbols for LLVM.
  llvm_builtin_routines: [Builtin_Routine.Count]LLVM_Builtin_Routine, // Holds all built-in routines for LLVM.
  llvm_intrinsics: map[Intrinsic_Routine]LLVM_Routine_Tuple,          // Holds all intrinsic routines for LLVM.
  llvm_strings: map[string]LLVMValueRef,                              // Holds all strings for LLVM.
  llvm_vtables: map[VTable_Key]LLVMValueRef,                          // Holds all vtables for LLVM.
  llvm_hasher_functions: map[^Type]LLVM_Routine_Tuple,                // Holds all hasher functions for LLVM.
  llvm_comparer_functions: map[^Type]LLVM_Routine_Tuple,              // Holds all comparer functions for LLVM.

  type_info_table_indices: map[Type_Id]u64,                           // Holds all type info table indices.
  llvm_type_info_table_indices_map: LLVMValueRef,                     // Holds the LLVM type info indices table.
  llvm_type_info_table_indices_map_type: LLVMTypeRef,                 // Holds the LLVM type of the type info indices table.
  llvm_type_info_table: LLVMValueRef,                                 // Holds the LLVM type info table.
  llvm_type_info_table_type: LLVMTypeRef,                             // Holds the LLVM type of the type info table.

  initializer_routine_context: Routine_Context,                       // Holds the routine context for the global initializer.
  initializer_context: LLVMValueRef,                                  // Holds the implicit context for the global initializer.
  initializer_declaration_block: LLVMBasicBlockRef,                   // Holds the declaration block for insering 'alloca' instructions.
  initializer_block: LLVMBasicBlockRef,                               // Holds the actual block for regular global intiailizer expressions.

  llvm_target_machine: LLVMTargetMachineRef,                          // Holds the LLVM target machine.
  llvm_target_data_layout: LLVMTargetDataRef,                         // Holds the LLVM data layout of the target machine.
  llvm_module_pass_manager: LLVMPassManagerRef,                       // Holds the LLVM module pass manager used in optimization.
  llvm_function_pass_manager: LLVMPassManagerRef,                     // Holds the LLVM function pass manager used in optimization.

  debug: Generator_Debug,                                             // Holds the associated debug information.
}

/**
* Holds all arguments required for generation.
*/
Generation_Arguments :: struct {
  storage: ^Type_Storage,                       // The reference to the type storage.
  resolver_output: Resolver_Output,             // The output of the resolver.
  main_symbol: ^Symbol,                         // The symbol of the main entry point.
  compilation_arguments: Compilation_Arguments, // The compilation arguments that are being used.
  libraries_to_link: []string,                  // The external libraries to link to.
}

/**
* Makes a new generator.
*
* @return The new generator.
*/
generator_make :: proc() -> ^Generator {
  generator := new(Generator);
  return generator;
}

/**
* Destroys the resouces assocociated with a generator.
* 
* @param generator The generator to destroy.
*/
generator_destroy :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_debug_destroy(generator);

  delete(generator.llvm_types);
  delete(generator.llvm_symbols);
  delete(generator.llvm_intrinsics);
  delete(generator.llvm_strings);
  delete(generator.llvm_vtables);
  delete(generator.llvm_hasher_functions);
  delete(generator.llvm_comparer_functions);
  delete(generator.type_info_table_indices);

  // We could dispose of LLVM resources here but that is pretty much just time being wasted.

  free(generator);
}

/**
* Starts the generation phase.
*
* @param generator The reference to the generator.
* @param arguments The generation arguments.
*/
generate :: proc(generator: ^Generator, arguments: Generation_Arguments) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator.storage = arguments.storage;
  generator.resolver_output = arguments.resolver_output;
  generator.module = LLVMModuleCreateWithName(to_cstring(arguments.compilation_arguments.output_name));
  generator.builder = LLVMCreateBuilder();
  generator.temporary_builder = LLVMCreateBuilder();

  generator_debug_init(generator, arguments);

  generator_init_types(generator);
  generator_init_symbols(generator);
  generator_init_routine_intrinsics(generator);
  generator_init_initializer_function(generator);
  generator_init_target_machine(generator);
  generator_init_module_pass_manager(generator, arguments.compilation_arguments.optimization_level);
  generator_init_function_pass_manager(generator, arguments.compilation_arguments. optimization_level);

  generator.user_entry_point = arguments.main_symbol;

  generator_emit_type_info_table(generator, arguments.compilation_arguments.rttr_disabled);
  generator_emit_symbols(generator);
  
  generator_finalize_initializer_function(generator);
  generator_debug_finalize(generator);

  compiler := cast(^Compiler) context.user_ptr;

  compiler.profiler.generator_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.individual_stopwatch));
  time.stopwatch_reset(&compiler.profiler.individual_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);

  compiler.profiler.frontend_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.global_stopwatch));

  generator_optimize(generator);

  if arguments.compilation_arguments.generate_llvm_ir {
    generator_generate_intermediate(generator, arguments.compilation_arguments.output_name);
  }
  generator_generate_executable(generator, arguments.compilation_arguments.output_name);
  compiler.profiler.llvm_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.individual_stopwatch));
  time.stopwatch_reset(&compiler.profiler.individual_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);

  generator_link_executable(arguments.compilation_arguments, arguments.libraries_to_link);
  compiler.profiler.link_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.individual_stopwatch));
  time.stopwatch_reset(&compiler.profiler.individual_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);
}

/**
* Initializes all LLVM intrinsics.
*
* @param generator The reference to the generator.
*/
generator_init_routine_intrinsics :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  intrinsic_name_trap := "llvm.trap";
  llvm_intrinsic_trap_id := LLVMLookupIntrinsicID(to_cstring(intrinsic_name_trap), cast(u64) len(intrinsic_name_trap));
  llvm_intrinsic_trap_symbol := LLVMGetIntrinsicDeclaration(generator.module, llvm_intrinsic_trap_id, nil, 0);
  llvm_intrinsic_trap_type := LLVMIntrinsicGetType(LLVMGetGlobalContext(), llvm_intrinsic_trap_id, nil, 0);
  generator.llvm_intrinsics[.Trap] = {llvm_intrinsic_trap_symbol, llvm_intrinsic_trap_type};
}

/**
* Initializes the global initializer function.
*
* @param generator The reference to the generator.
*/
generator_init_initializer_function :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Inspired from: https://gist.github.com/ishikawa/765033/cebaaf73d5ea1e250be6204e5d1ccc0b61be646e

  function_type := LLVMFunctionType(LLVMVoidType(), nil, 0, false);
  pointer_type := LLVMPointerType(LLVMInt8Type(), 0);

  initializer_function := LLVMAddFunction(generator.module, "$ctor", function_type);
  generator.initializer_routine_context.is_global_initializer = true;
  generator.initializer_routine_context.llvm_symbol = initializer_function;
  generator.initializer_routine_context.debug.scope = generator_debug_get_initializer_function_scope(generator);
  generator.initializer_declaration_block = LLVMAppendBasicBlock(initializer_function, "block_0");
  generator.initializer_block = LLVMAppendBasicBlock(initializer_function, "block_1");
  generator.initializer_routine_context.current_block = generator.initializer_block;
  generator.initializer_routine_context.declaration_block = generator.initializer_declaration_block;
  LLVMPositionBuilderAtEnd(generator.builder, generator.initializer_declaration_block);
  generator.initializer_context = LLVMBuildAlloca(generator.builder, generator_type_to_llvm(generator, generator.storage.cached_runtime_types.context_struct), SPECIAL_NAME_CONTEXT);
  LLVMPositionBuilderAtEnd(generator.builder, generator.initializer_block);
  generator_emit_default_context_initialization(generator, generator.initializer_context);
  
  struct_element_types: []LLVMTypeRef = { LLVMInt32Type(), LLVMPointerType(function_type, 0), pointer_type };
  llvm_global_ctors_element_type := LLVMStructType(raw_data(struct_element_types), cast(u32) len(struct_element_types), false);

  llvm_global_ctors := LLVMAddGlobal(generator.module, LLVMArrayType(llvm_global_ctors_element_type, 1), "llvm.global_ctors");
  LLVMSetLinkage(llvm_global_ctors, .Appending);
  LLVMSetSection(llvm_global_ctors, ".ctor");

  struct_element_values: []LLVMValueRef = { LLVMConstInt(LLVMInt32Type(), 65535, false), initializer_function, LLVMConstNull(pointer_type) }
  array_element_values: []LLVMValueRef = { LLVMConstStruct(raw_data(struct_element_values), cast(u32) len(struct_element_values), false) };
  LLVMSetInitializer(llvm_global_ctors, LLVMConstArray(llvm_global_ctors_element_type, raw_data(array_element_values), cast(u32) len(array_element_values)));
}

/**
* Initializes the target machine.
*
* @param generator The reference to the generator.
*/
generator_init_target_machine :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Initialize all targets for now (may or may not be necessary).
  LLVMInitializeAllTargetInfos();
  LLVMInitializeAllTargets();
  LLVMInitializeAllTargetMCs();
  LLVMInitializeAllAsmPrinters();
  LLVMInitializeAllAsmParsers();
  LLVMInitializeAllDisassemblers();

  target_triple := LLVMGetDefaultTargetTriple();
  target: LLVMTargetRef = nil;
  target_error: cstring = nil;
  LLVMGetTargetFromTriple(target_triple, &target, &target_error);

  llvm_cpu: cstring = "generic";
  // On Linux we need a different relocation mode.
  relocation_mode := LLVMRelocMode.PIC when ODIN_OS == .Linux else  LLVMRelocMode.Default;
  llvm_features := LLVMGetHostCPUFeatures();
  generator.llvm_target_machine = LLVMCreateTargetMachine(
    target,
    target_triple,
    llvm_cpu,
    llvm_features,
    .Default,
    relocation_mode,
    .Default,
  );
  generator.llvm_target_data_layout = LLVMCreateTargetDataLayout(generator.llvm_target_machine);
  LLVMSetModuleDataLayout(generator.module, generator.llvm_target_data_layout);
  LLVMSetTarget(generator.module, target_triple);
}

/**
* Finalizes the generation of the global initializer function.
*
* @param generator The reference to the generator.
*/
generator_finalize_initializer_function :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  LLVMPositionBuilderAtEnd(generator.builder, generator.initializer_declaration_block);
  LLVMBuildBr(generator.builder, generator.initializer_block);

  LLVMPositionBuilderAtEnd(generator.builder, generator.initializer_block);
  LLVMBuildRetVoid(generator.builder);
}

/**
* Generates the intermediate LLVM IR in text form.
*
* @param generator   The reference to the generator.
* @param output_name The name of the output file.
*/
generator_generate_intermediate :: proc(generator: ^Generator, output_name: string) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  builder: strings.Builder;
  defer strings.builder_destroy(&builder);
  strings.write_string(&builder, output_name);
  strings.write_string(&builder, ".ll");
  filename := strings.clone_to_cstring(strings.to_string(builder));
  defer delete(filename);

  module_print_error: cstring;
  LLVMPrintModuleToFile(generator.module, filename, &module_print_error);
  LLVMDisposeMessage(module_print_error);
}

/**
* Generates the native machine code.
*
* @param generator   The reference to the generator.
* @param output_name The name of the output file.
*/
generator_generate_executable :: proc(generator: ^Generator, output_name: string) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // First we verify the module.
  verify_error: cstring;
  LLVMVerifyModule(generator.module, LLVMVerifierFailureAction.AbortProcess, &verify_error);
  LLVMDisposeMessage(verify_error);

  // Here we finally produce the object file.
  builder: strings.Builder;
  defer strings.builder_destroy(&builder);
  strings.write_string(&builder, output_name);
  when ODIN_OS == .Windows {
    strings.write_string(&builder, ".obj");
  } else when ODIN_OS == .Linux {
    strings.write_string(&builder, ".o");
  }
  filename := strings.clone_to_cstring(strings.to_string(builder));
  defer delete(filename);

  emit_error: cstring;
  LLVMTargetMachineEmitToFile(generator.llvm_target_machine, generator.module, filename, LLVMCodeGenFileType.ObjectFile, &emit_error);
}

/**
* Links the generated native machine code with the os.
*
* @param arguments         The compilation arguments that are being used.
* @param libraries_to_link The external libraries to link to.
*/
generator_link_executable :: proc(compilation_arguments: Compilation_Arguments, libraries_to_link: []string) {
  // Link the object file to produce a final executable.
  os_invoke_linker(compilation_arguments, libraries_to_link);
}
