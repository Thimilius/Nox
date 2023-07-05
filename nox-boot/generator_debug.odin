package nox

import "core:strings"
import "tracy"

/**
* Holds the debug data for the generator.
*/
Generator_Debug :: struct {
  build_debug: bool,                                          // Should debug information be generated?

  builder: LLVMDIBuilderRef,                                  // The LLVM debug builder.
  
  metadata_files: map[rawptr]LLVMMetadataRef,                 // The mapping of files to their metadata.
  compile_unit: LLVMMetadataRef,                              // The compile unit the debug info lives in.

  debug_types: map[^Type]LLVMMetadataRef,                     // The mapping of types to their debug representation.
  incomplete_types: [dynamic]Generator_Debug_Incomplete_Type, // The incomplete types that need to be completed in a later step.
}

/**
* Represents an incomplete debug type.
*/
Generator_Debug_Incomplete_Type :: struct {
  incomplete_type: ^Type,                // The incomplete type.
  llvm_incomplete_type: LLVMMetadataRef, // The LLVM incomplete debug type.
}

/**
* Holds the debug info for a local scope.
*/
Routine_Debug_Local_Scope :: struct {
  scope: LLVMMetadataRef, // The debug scope of the local scope.
}

/**
* Holds the debug info for a routine.
*/
Routine_Context_Debug :: struct {
  scope: LLVMMetadataRef, // The debug scope of the routine.
}

/**
* Initializes the debug information.
*
* @param generator The reference to the generator.
* @param arguments The used generation arguments.
*/
generator_debug_init :: proc(generator: ^Generator, arguments: Generation_Arguments) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  generator.debug.build_debug = arguments.compilation_arguments.build_debug;
  if !generator.debug.build_debug {
    return;
  }

  generator.debug.builder = LLVMCreateDIBuilder(generator.module);

  DEBUG_METADATA_VERSION :: 3;
  debug_metadata := LLVMValueAsMetadata(LLVMConstInt(LLVMInt32Type(), DEBUG_METADATA_VERSION, true));
  LLVMAddModuleFlag(generator.module, .Warning, "Debug Info Version", 18, debug_metadata);

  when ODIN_OS == .Windows {
    code_view_metadata := LLVMValueAsMetadata(LLVMConstInt(LLVMInt32Type(), 1, true));
    LLVMAddModuleFlag(generator.module, .Warning, "CodeView", 8, code_view_metadata);
  }

  compiler := cast(^Compiler) context.user_ptr;
  for _, pack in compiler.packages {
    for _, package_file in pack.files {
      filename := generator_debug_get_filename_from_path(package_file.path);
      directory := generator_debug_get_directory_from_path(package_file.path);

      debug_file := LLVMDIBuilderCreateFile(
        generator.debug.builder,
        cast(cstring) raw_data(filename),
        cast(u64) len(filename),
        cast(cstring) raw_data(directory),
        cast(u64) len(directory),
      );

      generator.debug.metadata_files[package_file] = debug_file;
    }
  }

  main_file := generator.debug.metadata_files[arguments.main_symbol.package_file];
  producer := "nox";
  is_optimized: LLVMBool = arguments.compilation_arguments.optimization_level > 0;
  
  generator.debug.compile_unit = LLVMDIBuilderCreateCompileUnit(
    generator.debug.builder,
    .C99,
    main_file,
    cast(cstring) raw_data(producer),
    cast(u64) len(producer),
    is_optimized,
    "",
    0,
    1,
    "",
    0,
    .Full,
    0,
    false,
    false,
    "",
    0,
    "",
    0,
  );
}

/**
* Initializes the debug information for a routine.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the rouinte.
*/
generator_debug_init_routine :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  name := routine_context.symbol.name;
  line := routine_context.symbol.declaration.position.line;
  file := generator.debug.metadata_files[routine_context.symbol.package_file];
  type := generator_debug_get_type_routine(generator, routine_context.symbol);
  flags := LLVMDIFlags.StaticMember;
  scope := file;

  debug_info := LLVMDIBuilderCreateFunction(
    generator.debug.builder,
    scope,
    cast(cstring) raw_data(name),
    cast(u64) len(name),
    cast(cstring) raw_data(name),
    cast(u64) len(name),
    file,
    cast(u32) line,
    type,
    false,
    true,
    cast(u32) line,
    flags,
    false,
  );
  routine_context.debug.scope = debug_info;
  LLVMSetSubprogram(routine_context.llvm_symbol, debug_info);

  generator.debug.metadata_files[debug_info] = file;
}

/**
* Enters a local debug scope.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the rouinte.
* @param parent          The parent of the local scope to enter.
* @param position        The position of the local scope to enter.
*/
generator_debug_enter_local_scope :: proc(generator: ^Generator, routine_context: ^Routine_Context, parent: ^Routine_Local_Scope, position: Source_Position) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  parent_scope: LLVMMetadataRef;
  if parent == nil {
    parent_scope = routine_context.debug.scope;
  } else {
    parent_scope = parent.debug.scope;
  }
  assert(parent_scope != nil);

  file := LLVMDIScopeGetFile(parent_scope);
  routine_context.local_scope.debug.scope = LLVMDIBuilderCreateLexicalBlock(
    generator.debug.builder,
    parent_scope,
    file,
    cast(u32) position.line,
    cast(u32) position.column,
  );
}

/**
* Emit debug info for a parameter.
*
* @param generator        The reference to the generator.
* @param routine_context  The context of the rouinte.
* @param position         The position of the parameter.
* @param type             The type of the parameter.
* @param name             The name of the parameter.
* @param storage          The LLVM pointer where the parameter is stored.
* @param parameter_number The number of the parameter.
*/
generator_debug_emit_parameter :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  position: Source_Position,
  type: ^Type,
  name: string,
  storage: LLVMValueRef,
  parameter_number: u32,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }
  if name == SPECIAL_NAME_DISCARD {
    return;
  }

  scope := routine_context.debug.scope;
  file := generator.debug.metadata_files[scope];
  assert(file != nil);
  always_preserve: LLVMBool = (cast(^Compiler) context.user_ptr).arguments.optimization_level == 0;

  llvm_parameter := LLVMDIBuilderCreateParameterVariable(
    generator.debug.builder,
    scope,
    to_cstring(name),
    cast(u64) len(name),
    parameter_number,
    file,
    cast(u32) position.line,
    generator_debug_get_type(generator, type),
    always_preserve,
    .Zero,
  );

  location := generator_debug_get_location(generator, routine_context, position);
  llvm_expression := LLVMDIBuilderCreateExpression(generator.debug.builder, nil, 0);
  block := routine_context.declaration_block;

  LLVMDIBuilderInsertDbgValueAtEnd(generator.debug.builder, storage, llvm_parameter, llvm_expression, location, block);
}

/**
* Emit debug info for a local variable.
*
* @param generator        The reference to the generator.
* @param routine_context  The context of the rouinte.
* @param position         The position of the local variable.
* @param type             The type of the local variable.
* @param name             The name of the local variable.
* @param storage          The LLVM pointer where the local variable is stored.
*/
generator_debug_emit_local_variable :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  position: Source_Position,
  type: ^Type,
  name: string,
  storage: LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }
  if name == SPECIAL_NAME_DISCARD {
    return;
  }

  assert(routine_context.current_block != routine_context.declaration_block);

  scope := generator_debug_get_current_scope(routine_context);
  file := generator.debug.metadata_files[routine_context.debug.scope];
  assert(file != nil);

  flags := LLVMDIFlags.Zero;
  always_preserve: LLVMBool = (cast(^Compiler) context.user_ptr).arguments.optimization_level == 0;
  debug_type := generator_debug_get_type(generator, type);
  alignment_in_bits: u32 = 8 * cast(u32) type_alignment_of(type);

  variable_info := LLVMDIBuilderCreateAutoVariable(
    generator.debug.builder,
    scope,
    to_cstring(name),
    cast(u64) len(name),
    file,
    cast(u32) position.line,
    debug_type,
    always_preserve,
    flags,
    alignment_in_bits,
  );
  
  llvm_expression := LLVMDIBuilderCreateExpression(generator.debug.builder, nil, 0);
  location := generator_debug_get_location(generator, routine_context, position);
  block := routine_context.current_block;
  LLVMDIBuilderInsertDeclareAtEnd(generator.debug.builder, storage, variable_info, llvm_expression, location, block);
}

/**
* Emit debug location info for the current position.
*
* @param generator        The reference to the generator.
* @param routine_context  The context of the rouinte.
* @param position         The current position.
*/
generator_debug_emit_location :: proc(generator: ^Generator, routine_context: ^Routine_Context, position: Source_Position) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  location := generator_debug_get_location(generator, routine_context, position); 
  LLVMSetCurrentDebugLocation2(generator.builder, location);
}

/**
* Emit no debug location.
*
* @param generator        The reference to the generator.
*/
generator_debug_emit_no_location :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  LLVMSetCurrentDebugLocation2(generator.builder, nil);
}

/**
* Get the debug representation for a given type.
*
* @param generator The reference to the generator.
* @param type      The type to get the debug representation of.
* @return The debug representation corresponding to the type.
*/
generator_debug_get_type :: proc(generator: ^Generator, type: ^Type) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  debug_type, found := generator.debug.debug_types[type];
  if found {
    return debug_type;
  }
  
  size_in_bits := 8 * cast(u64) type_size_of(type);
  align_in_bits := 8 * cast(u32) type_alignment_of(type);
  word_size_in_bits: u64 = 64; // NOTE: This is hardcoded for 64-bit architectures which is the only one we support right now.

  switch type.kind {
    case .B8: debug_type = generator_debug_create_type_basic(generator, "b8", size_in_bits, .Boolean);
    case .B16: debug_type = generator_debug_create_type_basic(generator, "b16", size_in_bits, .Boolean);
    case .B32: debug_type = generator_debug_create_type_basic(generator, "b32", size_in_bits, .Boolean);
    case .B64: debug_type = generator_debug_create_type_basic(generator, "b64", size_in_bits, .Boolean);
    case .Bool: debug_type = generator_debug_create_type_basic(generator, "bool", size_in_bits, .Boolean);

    case .I8: debug_type = generator_debug_create_type_basic(generator, "i8", size_in_bits, .Signed);
    case .I16: debug_type = generator_debug_create_type_basic(generator, "i16", size_in_bits, .Signed);
    case .I32: debug_type = generator_debug_create_type_basic(generator, "i32", size_in_bits, .Signed);
    case .I64: debug_type = generator_debug_create_type_basic(generator, "i64", size_in_bits, .Signed);
    case .Int: debug_type = generator_debug_create_type_basic(generator, "int", size_in_bits, .Signed);
    
    case .U8: debug_type = generator_debug_create_type_basic(generator, "u8", size_in_bits, .Unsigned);
    case .U16: debug_type = generator_debug_create_type_basic(generator, "u16", size_in_bits, .Unsigned);
    case .U32: debug_type = generator_debug_create_type_basic(generator, "u32", size_in_bits, .Unsigned);
    case .U64: debug_type = generator_debug_create_type_basic(generator, "u64", size_in_bits, .Unsigned);
    case .UInt: debug_type = generator_debug_create_type_basic(generator, "uint", size_in_bits, .Unsigned);
    
    case .F32: debug_type =  generator_debug_create_type_basic(generator, "f32", size_in_bits, .Float);
    case .F64: debug_type =  generator_debug_create_type_basic(generator, "f64", size_in_bits, .Float);
    
    case .Char: debug_type =  generator_debug_create_type_basic(generator, "char", size_in_bits, .Utf);
    
    case .Enumeration: {
      debug_type = generator_debug_create_type_enumeration(generator, type, size_in_bits, align_in_bits);
    }
    
    case .String: {
      scope := generator_debug_get_runtime_package_scope(generator);

      elements: []LLVMMetadataRef = {
        generator_debug_create_member(generator, scope, "data", type_storage_get_or_make_type_pointer(generator.storage, generator.storage.type_u8), 0),
        generator_debug_create_member(generator, scope, "length", generator.storage.type_int, word_size_in_bits),
      };
      debug_type = generator_debug_create_type_struct(generator, scope, nil, "string", size_in_bits, cast(u32) size_in_bits, elements);
    }
    case .CString: {
      char_type := generator_debug_create_type_basic(generator, "char", 8, .UnsignedChar);
      debug_type = LLVMDIBuilderCreatePointerType(generator.debug.builder, char_type, size_in_bits, cast(u32) size_in_bits, 0, "cstring", 7);
    }
    
    case .Function, .Procedure, .Rawptr: {
      debug_type = LLVMDIBuilderCreatePointerType(
        generator.debug.builder,
        nil,
        size_in_bits,
        cast(u32) size_in_bits,
        cast(u32) LLVMDWARFTypeEncoding.Address,
        to_cstring(type.name),
        cast(u64) len(type.name),
      );
    }
    case .Pointer: {
      debug_type = LLVMDIBuilderCreatePointerType(
        generator.debug.builder,
        generator_debug_get_type(generator, type.base),
        size_in_bits,
        cast(u32) size_in_bits,
        0,
        nil,
        0,
      );
    }
    case .Self_Relative_Pointer, .Offset_Relative_Pointer: {
      debug_type = generator_debug_get_type(generator, (cast(^Type_Relative_Pointer) type).relative_base);
    }
    case .SoA_Layout_Pointer, .AoSoA_Layout_Pointer: {
      scope := generator_debug_get_runtime_package_scope(generator);

      elements: []LLVMMetadataRef = {
        generator_debug_create_member(generator, scope, "pointer", generator.storage.type_rawptr, 0),
        generator_debug_create_member(generator, scope, "index", generator.storage.type_int, word_size_in_bits),
      };
      debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements);
    }
    case .Dynamic_Pointer: {
      scope := generator_debug_get_runtime_package_scope(generator);

      elements: []LLVMMetadataRef = {
        generator_debug_create_member(generator, scope, "pointer", generator.storage.type_rawptr, 0),
        generator_debug_create_member(generator, scope, "vtable", generator.storage.type_rawptr, word_size_in_bits),
      };
      debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements);
    }
    case .Constant: debug_type = generator_debug_get_type(generator, type.base);
    case .Array: {
      type_array := cast(^Type_Array) type;

      scope := generator_debug_get_runtime_package_scope(generator);

      if type_is_array_soa_or_aosoa(type) {
        debug_type = generator_debug_create_type_layout_struct(generator, scope, type, size_in_bits);
      } else {
        subrange := LLVMDIBuilderGetOrCreateSubrange(generator.debug.builder, 0, cast(i64) type_array.number_of_elements);
        subranges: []LLVMMetadataRef = {subrange};

        base_type := generator_debug_get_type(generator, type.base);
        alignment_in_bits := 8 * cast(u32) type_alignment_of(type);
        debug_type = LLVMDIBuilderCreateArrayType(generator.debug.builder, size_in_bits, alignment_in_bits, base_type, raw_data(subranges), cast(u32) len(subranges));
      }
    }
    case .Dynamic_Array: {
      scope := generator_debug_get_runtime_package_scope(generator);

      if (type_is_dynamic_array_soa_or_aosoa(type)) {
        debug_type = generator_debug_create_type_layout_struct(generator, scope, type, size_in_bits);
      } else {
        elements: []LLVMMetadataRef = {
          generator_debug_create_member(generator, scope, "data", type_storage_get_or_make_type_pointer(generator.storage, type.base), 0),
          generator_debug_create_member(generator, scope, "length", generator.storage.type_int, word_size_in_bits),
          generator_debug_create_member(generator, scope, "capacity", generator.storage.type_int, word_size_in_bits * 2),
          generator_debug_create_member(generator, scope, "allocator", generator.storage.cached_runtime_types.allocator, word_size_in_bits * 3),
        };
        debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements);
      }
    }
    case .Slice: {
      scope := generator_debug_get_runtime_package_scope(generator);

      if (type_is_slice_soa_or_aosoa(type)) {
        debug_type = generator_debug_create_type_layout_struct(generator, scope, type, size_in_bits);
      } else {
        elements: []LLVMMetadataRef = {
          generator_debug_create_member(generator, scope, "data", type_storage_get_or_make_type_pointer(generator.storage, type.base), 0),
          generator_debug_create_member(generator, scope, "length", generator.storage.type_int, word_size_in_bits),
        };
        debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements);
      }
    }
    case .Map: {
      scope := generator_debug_get_runtime_package_scope(generator);

      elements: []LLVMMetadataRef = {
        generator_debug_create_member(generator, scope, "data", generator.storage.type_rawptr, 0),
        generator_debug_create_member(generator, scope, "length", generator.storage.type_int, word_size_in_bits),
        generator_debug_create_member(generator, scope, "capacity", generator.storage.type_int, word_size_in_bits * 2),
        generator_debug_create_member(generator, scope, "allocator", generator.storage.cached_runtime_types.allocator, word_size_in_bits * 3),
        generator_debug_create_member(generator, scope, "hash_procedure", generator.storage.type_rawptr, word_size_in_bits * 5), // '5' because Allocator is 16 bytes.
        generator_debug_create_member(generator, scope, "compare_procedure", generator.storage.type_rawptr, word_size_in_bits * 6),
      };
      debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements);
    }
    case .Tuple: {
      type_tuple := cast(^Type_Tuple) type;

      scope := generator_debug_get_runtime_package_scope(generator);

      elements := make_dynamic_array_len_cap([dynamic]LLVMMetadataRef, 0, len(type_tuple.elements));
      defer delete(elements);
      for element, i in type_tuple.elements {
        offset_in_bits := 8 * cast(u64) element.offset;
        name := to_cstring_f("element_%v", i);
        append(&elements, generator_debug_create_member(generator, scope, name, element.type, offset_in_bits));
      }

      debug_type = generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, align_in_bits, elements[:]);
    }

    case .Struct: fallthrough;
    case .Union: {
      tag: LLVMDWARFTag = type.kind == .Union ? .Union : .Structure;
      temp_forward_declaration := LLVMDIBuilderCreateReplaceableCompositeType(
        generator.debug.builder,
        tag,
        "",
        0,
        nil,
        nil,
        0,
        0,
        size_in_bits,
        align_in_bits,
        .Zero,
        "",
        0,
			);

      debug_type = temp_forward_declaration;
      append(&generator.debug.incomplete_types, Generator_Debug_Incomplete_Type{type, debug_type});
    }

    case .Any: {
      scope := generator_debug_get_runtime_package_scope(generator);

      elements: [2]LLVMMetadataRef;
      elements[0] = generator_debug_create_member(generator, scope, "data", generator.storage.type_rawptr, 0);
      elements[1] = generator_debug_create_member(generator, scope, "typeid", generator.storage.type_int, word_size_in_bits);
      debug_type = generator_debug_create_type_struct(generator, scope, nil, "any", size_in_bits, cast(u32) size_in_bits, elements[:]);
    }
    case .Typeid: debug_type = generator_debug_create_type_basic(generator, "typeid", size_in_bits, .Unsigned);

    case .None, .Void, .Interface, .Generic: fallthrough;
    case .Untyped_Boolean, .Untyped_Integer, .Untyped_Float, .Untyped_Char, .Untyped_String, .Untyped_Null: fallthrough;
    case: assert(false);
  }

  generator.debug.debug_types[type] = debug_type;
  return debug_type;
}

/**
* Create debug info for a basic type.
*
* @param generator    The reference to the generator.
* @param name         The name of the type.
* @param size_in_bits The size of the type in bits.
* @param enconding    The encoding of the type.
* @return The debug info for the basic type.
*/
generator_debug_create_type_basic :: proc(generator: ^Generator, name: cstring, size_in_bits: u64, encoding: LLVMDWARFTypeEncoding) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  return LLVMDIBuilderCreateBasicType(generator.debug.builder, name, cast(u64) len(name), size_in_bits, encoding, .Zero);
}

/**
* Create debug info for an enumeration type.
*
* @param generator     The reference to the generator.
* @param name          The name of the type.
* @param size_in_bits  The size of the type in bits.
* @param align_in_bits The alignment of the type in bits.
* @return The debug info for the enumeration type.
*/
generator_debug_create_type_enumeration :: proc(generator: ^Generator, type: ^Type, size_in_bits: u64, align_in_bits: u32) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  symbol := type.symbol;
  file := generator.debug.metadata_files[symbol.package_file];
  scope := file;

  type_enumeration := cast(^Type_Enumeration) type;

  enumerators: [dynamic]LLVMMetadataRef;
  defer delete(enumerators);
  for item in type_enumeration.items {
    value: i64;
    #partial switch type.base.kind {
      case .I8:   value = cast(i64) item.value.(i8);
      case .I16:  value = cast(i64) item.value.(i16);
      case .I32:  value = cast(i64) item.value.(i32);
      case .I64:  value = cast(i64) item.value.(i64);
      case .Int:  value = cast(i64) item.value.(int);
      case .U8:   value = cast(i64) item.value.(u8);
      case .U16:  value = cast(i64) item.value.(u16);
      case .U32:  value = cast(i64) item.value.(u32);
      case .U64:  value = cast(i64) item.value.(u64);
      case .UInt: value = cast(i64) item.value.(uint);
    }
    
    enumerator := LLVMDIBuilderCreateEnumerator(
      generator.debug.builder,
      to_cstring(item.name),
      cast(u64) len(item.name),
      value,
      cast(LLVMBool) type_is_unsigned(type.base),
    );
    append(&enumerators, enumerator);
  }

  return LLVMDIBuilderCreateEnumerationType(
    generator.debug.builder,
    scope,
    to_cstring(symbol.name),
    cast(u64) len(symbol.name),
    file,
    generator_debug_get_line_from_symbol(symbol),
    size_in_bits,
    align_in_bits,
    raw_data(enumerators),
    cast(u32) len(enumerators),
    generator_debug_get_type(generator, type.base),
  );
}

/**
* Create debug info for a struct type.
*
* @param generator     The reference to the generator.
* @param scope         The scope the type lives in.
* @param symbol        The symbol of the type.
* @param name          The name of the type.
* @param size_in_bits  The size of the type in bits.
* @param align_in_bits The alignment of the type in bits.
* @param elements      The elements of the struct type.
* @return The debug info for the struct type.
*/
generator_debug_create_type_struct :: proc(
  generator: ^Generator,
  scope: LLVMMetadataRef,
  symbol: ^Symbol,
  name: cstring,
  size_in_bits: u64,
  align_in_bits: u32,
  elements: []LLVMMetadataRef,
) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  file := scope;
  return LLVMDIBuilderCreateStructType(
    generator.debug.builder,
    scope,
    name,
    cast(u64) len(name),
    file,
    generator_debug_get_line_from_symbol(symbol),
    size_in_bits,
    align_in_bits,
    .Zero,
    nil,
    raw_data(elements),
    cast(u32) len(elements),
    0,
    nil,
    "",
    0,
  );
}

/**
* Create debug info for a member of a struct type.
*
* @param generator      The reference to the generator.
* @param scope          The scope the member lives in.
* @param name           The name of the member.
* @param type           The type of the member.
* @param offset_in_bits The offset of the member in bits.
* @return The debug info for the struct member.
*/
generator_debug_create_member :: proc(generator: ^Generator, scope: LLVMMetadataRef, name: cstring, type: ^Type, offset_in_bits: u64) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  file := scope;

  size_in_bits := 8 * type_size_of(type);
  align_in_bits := 8 * type_alignment_of(type);

  return LLVMDIBuilderCreateMemberType(
    generator.debug.builder,
    scope,
    name,
    cast(u64) len(name),
    file,
    1,
    cast(u64) size_in_bits,
    cast(u32) align_in_bits,
    offset_in_bits,
    .Zero,
    generator_debug_get_type(generator, type),
  );
}

/**
* Create debug info for a layout struct type.
*
* @param generator     The reference to the generator.
* @param scope         The scope the type lives in.
* @param type          The type of the layout struct.
* @param size_in_bits  The size of the type in bits.
* @return The debug info for the layout struct type.
*/
generator_debug_create_type_layout_struct :: proc(generator: ^Generator, scope: LLVMMetadataRef, type: ^Type, size_in_bits: u64) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  assert(type_is_soa_or_aosoa(type));
  assert(type_is_struct(type.base));

  type_struct := cast(^Type_Struct) type.base;
  elements := make_dynamic_array_len_cap([dynamic]LLVMMetadataRef, 0, len(type_struct.fields), context.temp_allocator);
  for field in type_struct.fields {
    field_offset_in_bits := 8 * cast(u64) field.offset; 
    append(&elements, generator_debug_create_member(generator, scope, to_cstring(field.name), field.type, field_offset_in_bits));
  }
  return generator_debug_create_type_struct(generator, scope, nil, to_cstring(type.name), size_in_bits, cast(u32) size_in_bits, elements[:]);
}

/**
* Gets the scope of the runtime package.
* 
* @param generator The reference to the generator.
* @return The scope of the runtime package.
*/
generator_debug_get_runtime_package_scope :: proc(generator: ^Generator) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  package_file := package_get_first_package_file((cast(^Compiler) context.user_ptr).runtime_package);
  return generator.debug.metadata_files[package_file];
}

/**
* Gets the scope for the initializer function.
* 
* @param generator The reference to the generator.
* @return The scope for the initializer function.
*/
generator_debug_get_initializer_function_scope :: proc(generator: ^Generator) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return nil;
  }

  name := "initializer";
  line := 0;
  file := generator_debug_get_runtime_package_scope(generator);
  scope := file;
  type := LLVMDIBuilderCreateSubroutineType(generator.debug.builder, file, nil, 0, .Zero);
  flags := LLVMDIFlags.StaticMember;

  debug_info := LLVMDIBuilderCreateFunction(
    generator.debug.builder,
    scope,
    cast(cstring) raw_data(name),
    cast(u64) len(name),
    cast(cstring) raw_data(name),
    cast(u64) len(name),
    file,
    cast(u32) line,
    type,
    false,
    true,
    cast(u32) line,
    flags,
    false,
  );

  generator.debug.metadata_files[debug_info] = file;
  return debug_info;
}

/**
* Get the debug location for a position.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param position        The position to get the location of.
* @return The debug location.
*/
generator_debug_get_location :: proc(generator: ^Generator, routine_context: ^Routine_Context, position: Source_Position) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  scope := generator_debug_get_current_scope(routine_context);
  return LLVMDIBuilderCreateDebugLocation(LLVMGetGlobalContext(), cast(u32) position.line, cast(u32) position.column, scope, nil);
}

/**
* Gets the line from a symbol.
* 
* @param symbol The symbol to get the line of.
* @return The line of the symbol.
*/
generator_debug_get_line_from_symbol :: proc(symbol: ^Symbol) -> u32 {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if symbol != nil && symbol.declaration != nil {
    return cast(u32) symbol.declaration.position.line;
  } else {
    return 1;
  }
}

/**
* Get the current debug scope
* 
* @param routine_context The context of the routine.
* @return The debug location.
*/
generator_debug_get_current_scope :: proc(routine_context: ^Routine_Context) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if routine_context.local_scope == nil {
    return routine_context.debug.scope;
  } else {
    return routine_context.local_scope.debug.scope;
  }
}

/**
* Get the debug type for a routine symbol.
* 
* @param generator The reference to the generator.
* @param symbol    The routine symbol to get the debug type of
* @return The debug type of the routine symbol.
*/
generator_debug_get_type_routine :: proc(generator: ^Generator, symbol: ^Symbol) -> LLVMMetadataRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  file := generator.debug.metadata_files[symbol.package_file];
  assert(file != nil);

  flags := LLVMDIFlags.Zero;

  type_routine := cast(^Type_Routine) symbol.type;
  
  parameter_types: [dynamic]LLVMMetadataRef;
  defer delete(parameter_types);
  append(&parameter_types, type_is_void(type_routine.return_type) ? nil : generator_debug_get_type(generator, type_routine.return_type));
  for parameter in type_routine.parameters {
    append(&parameter_types, generator_debug_get_type(generator, parameter.type));
  }

  return LLVMDIBuilderCreateSubroutineType(generator.debug.builder, file, raw_data(parameter_types), cast(u32) len(parameter_types), flags);
}

/**
* Finalize the creation of the debug info.
* 
* @param generator The reference to the generator.
*/
generator_debug_finalize :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  free_all(context.temp_allocator);
  generator_debug_complete_types(generator);

  LLVMDIBuilderFinalize(generator.debug.builder);
}

/**
* Complete the incomplete debug types.
* 
* @param generator The reference to the generator.
*/
generator_debug_complete_types :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  // We iterate manually here as 'incomplete_types' gets populated inside the loop.
  for i := 0; i < len(generator.debug.incomplete_types); i += 1 {
    incomplete := generator.debug.incomplete_types[i];
    incomplete_type := incomplete.incomplete_type;

    size_in_bits := 8 * cast(u64) type_size_of(incomplete_type);
    align_in_bits := 8 * cast(u32) type_alignment_of(incomplete_type);

    llvm_complete_type: LLVMMetadataRef;
    #partial switch incomplete_type.kind {
      case .Struct: {
        type_struct := cast(^Type_Struct) incomplete_type;

        symbol := incomplete_type.symbol;
        file := generator.debug.metadata_files[symbol.package_file];
        scope := file;

        elements := make_dynamic_array_len_cap([dynamic]LLVMMetadataRef, 0, len(type_struct.fields), context.temp_allocator);
        for field in type_struct.fields {
          field_offset_in_bits := 8 * cast(u64) field.offset;
          append(&elements, generator_debug_create_member(generator, scope, to_cstring(field.name), field.type, field_offset_in_bits));
        }

        llvm_complete_type = generator_debug_create_type_struct(generator, scope, symbol, to_cstring(incomplete_type.name), size_in_bits, align_in_bits, elements[:]);
      }
      case .Union: {
        type_union := cast(^Type_Union) incomplete_type;

        symbol := incomplete_type.symbol;
        file := generator.debug.metadata_files[symbol.package_file];
        scope := file;

        elements := make_dynamic_array_len_cap([dynamic]LLVMMetadataRef, 0, len(type_union.variants), context.temp_allocator);
        append(&elements, generator_debug_create_member(generator, scope, "tag", generator.storage.type_uint, 0));
        variant_offset := 8 * cast(u64) type_size_of(generator.storage.type_uint);
        for variant, i in type_union.variants {
          element := generator_debug_create_member(generator, scope, to_cstring_f("v%v", i + 1), variant, variant_offset);
          append(&elements, element);
        }

        llvm_complete_type = generator_debug_create_type_struct(generator, scope, symbol, to_cstring(incomplete_type.name), size_in_bits, align_in_bits, elements[:]);
      }
      case: assert(false);
    }

    generator.debug.debug_types[incomplete_type] = llvm_complete_type;
    LLVMMetadataReplaceAllUsesWith(incomplete.llvm_incomplete_type, llvm_complete_type);
  }
}

/**
* Destroy the debug data.
* 
* @param generator The reference to the generator.
*/
generator_debug_destroy :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  if !generator.debug.build_debug {
    return;
  }

  delete(generator.debug.metadata_files);
  delete(generator.debug.debug_types);
  delete(generator.debug.incomplete_types);
}

/**
* Get the filename from a path.
* 
* @param path The path to get the filename of.
* @return The filename from the path.
*/
generator_debug_get_filename_from_path :: proc(path: string) -> string {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  index := strings.last_index(path, OS_PATH_SEPERATOR);
  if index < 0 {
    return path;
  } else {
    return path[index + 1:];
  }
}

/**
* Get the directory from a path.
* 
* @param path The path to get the directory of.
* @return The directory from the path.
*/
generator_debug_get_directory_from_path :: proc(path: string) -> string {
  tracy.ZoneC(ZONE_COLOR_GENERATOR_DEBUG);

  index := strings.last_index(path, OS_PATH_SEPERATOR);
  if index < 0 {
    return path;
  } else {
    return path[:index + 1];
  }
}
