package nox

import "core:fmt"
import "core:strings"
import "tracy"

/**
* Enters a LLVM block.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param block           The LLVM block to enter.
* @return The previous LLVM block.
*/
generator_enter_llvm_block :: proc(generator: ^Generator, routine_context: ^Routine_Context, block: LLVMBasicBlockRef) -> LLVMBasicBlockRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  previous_block := routine_context.current_block;
  LLVMPositionBuilderAtEnd(generator.builder, block);
  routine_context.current_block = block;
  return previous_block;
}

/**
* Leaves the current LLVM block.
* 
* @param routine_context The context of the routine.
* @param previous_block  The previous block that gets entered when leaving.
*/
generator_leave_llvm_block :: proc(routine_context: ^Routine_Context, previous_block: LLVMBasicBlockRef) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  routine_context.current_block = previous_block;
}

/**
* Appends a block to a routine.
* 
* @param routine_context The context of the routine.
* @return The appended LLVM block.
*/
generator_append_block :: proc(routine_context: ^Routine_Context) -> LLVMBasicBlockRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  block_name := to_cstring_f("block_%v", routine_context.block_counter);
  routine_context.block_counter += 1;
  assert(routine_context.llvm_symbol != nil);
  return LLVMAppendBasicBlock(routine_context.llvm_symbol, block_name);
}

/**
* Enters a local scope.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param kind            The kind of the local scope.
* @param position        The position of the local scope.
*/
generator_enter_local_scope :: proc(generator: ^Generator, routine_context: ^Routine_Context, kind: Routine_Local_Scope_Kind, position: Source_Position) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  scope := routine_context.local_scope;
  
  local_scope := new(Routine_Local_Scope);
  local_scope.kind = kind;
  local_scope.parent = scope;
  routine_context.local_scope = local_scope;

  generator_debug_enter_local_scope(generator, routine_context, scope, position);
}

/**
* Leaves a local scope.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param returns         Does the scope return?
* @param leaves          Does the scope leave?
*/
generator_leave_local_scope :: proc(generator: ^Generator, routine_context: ^Routine_Context, returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  if !returns && !leaves {
    generator_emit_deferred_statements_local(generator, routine_context);
  }

  scope := routine_context.local_scope;
  if scope != nil {
    routine_context.local_scope = scope.parent;
    delete(scope.symbols);
    delete(scope.deferred_statements);
    free(scope);
  } else {
    routine_context.local_scope = nil;
  }
}

/**
* Enters a specified locaing mode.
* 
* @param routine_context The context of the routine.
* @param loading_mode    The loading mode to enter.
* @return The previous loading mode.
*/
generator_enter_loading_mode :: proc(routine_context: ^Routine_Context, loading_mode: Loading_Mode) -> Loading_Mode {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  previous_loading_mode := routine_context.current_loading_mode;
  routine_context.current_loading_mode = loading_mode;
  return previous_loading_mode;
}

/**
* Enters a specified locaing mode.
* 
* @param routine_context       The context of the routine.
* @param previous_loading_mode The previous loading mode that gets entered when leaving.
*/
generator_leave_loading_mode :: proc(routine_context: ^Routine_Context, previous_loading_mode: Loading_Mode) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  routine_context.current_loading_mode = previous_loading_mode;
}

/**
* Adds a local symbol to the given scope.
* 
* @param name         The name of the symbol.
* @param llvm_symbol  The symbol to add.
* @param llvm_type    The LLVM type of the symbol.
* @param type         The type of the symbol
* @param is_parameter Is the symbol a parameter?
* @param scope        The scope to add the symbol to.
*/
generator_add_local_symbol :: proc(name: string, llvm_symbol: LLVMValueRef, llvm_type: LLVMTypeRef, type: ^Type, is_parameter: bool, scope: ^Routine_Local_Scope) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  scope.symbols[name] = {llvm_symbol, llvm_type, type, is_parameter};
}

/**
* Gets the local symbol for a given name.
* 
* @param routine_context The context of the routine.
* @param name            The name of the local symbol to get.
* @return 1. The local symbol; 2. True if the symbol was found otherwise false.
*/
generator_get_local_symbol :: proc(routine_context: ^Routine_Context, name: string) -> (LLVM_Local_Symbol, bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  return generator_get_local_symbol_in_scope_recursive(routine_context.local_scope, name);
}

/**
* Gets the local symbol for a given name and scope recursively.
* 
* @param scope The scope to recursively look in.
* @param name  The name of the local symbol to get.
* @return 1. The local symbol; 2. True if the symbol was found otherwise false.
*/
generator_get_local_symbol_in_scope_recursive :: proc(scope: ^Routine_Local_Scope, name: string) -> (LLVM_Local_Symbol, bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  if scope == nil {
    return {}, false;
  }

  symbol, found := scope.symbols[name];
  if found {
    return symbol, true;
  }

  return generator_get_local_symbol_in_scope_recursive(scope.parent, name);
}

/**
* Gets the resolved symbol for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved symbol.
*/
generator_get_resolved_symbol :: proc(generator: ^Generator, key: ^Expression) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We don't assert here as we are relying on the symbol not always being found.
  return generator.resolver_output.resolved_symbols[key];
}

/**
* Gets the resolved type for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved type.
*/
generator_get_resolved_type :: proc(generator: ^Generator, key: rawptr) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  resolved_type, found := generator.resolver_output.resolved_types[key];
  assert(found);

  return resolved_type;
}

/**
* Gets the resolved overwrite type for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved overwrite type.
*/
generator_get_resolved_overwrite_type :: proc(generator: ^Generator, key: ^Expression) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  return generator.resolver_output.resolved_overwrite_types[key];
}

/**
* Gets the resolved constant for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved constant.
*/
generator_get_resolved_constant :: proc(generator: ^Generator, key: ^Expression) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We don't assert here as we are relying on the constant not always being found.
  return generator.resolver_output.resolved_constants[key];
}

/**
* Gets the resolved member for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved member.
*/
generator_get_resolved_member :: proc(generator: ^Generator, key: ^Expression) -> Resolved_Member {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  resolved_index, found := generator.resolver_output.resolved_members[key];
  assert(found);

  return resolved_index;
}

/**
* Gets the resolved compound field for a key.
* 
* @param generator The reference to the generator.
* @parma key       The key to look up.
* @return The resolved compound field.
*/
generator_get_resolved_compound_field :: proc(generator: ^Generator, key: ^Expression) -> Resolved_Compound_Field {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  resolved_index, found := generator.resolver_output.resolved_compound_fields[key];
  assert(found);

  return resolved_index;
}

/**
* Gets the LLVM representation for a symbol.
* 
* @param generator The reference to the generator.
* @parma symbol    The symbol to get the LLVM representation of.
* @return The LLVM representation of the symbol.
*/
generator_symbol_to_llvm :: proc(generator: ^Generator, symbol: ^Symbol) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_symbol, found := generator.llvm_symbols[symbol];
  assert(found);
  return llvm_symbol;
}

/**
* Gets the LLVM representation for a constant.
* 
* @param generator The reference to the generator.
* @parma value     The value of the constant.
* @parma type      The type of the constant.
* @return The LLVM representation of the constant.
*/
generator_constant_to_llvm :: proc(generator: ^Generator, value: Value, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type := type_unqualify(type);
  if type_is_enumeration(type) {
    type = type.base;
  }
  llvm_type := generator_type_to_llvm(generator, type);

  #partial switch type.kind {
    case .B8:   return LLVMConstInt(llvm_type, cast(u64) value.(b8), false);
    case .B16:  return LLVMConstInt(llvm_type, cast(u64) value.(b16), false);
    case .B32:  return LLVMConstInt(llvm_type, cast(u64) value.(b32), false);
    case .B64:  return LLVMConstInt(llvm_type, cast(u64) value.(b64), false);
    case .Bool: return LLVMConstInt(llvm_type, cast(u64) value.(bool), false);
    case .I8:   return LLVMConstInt(llvm_type, cast(u64) value.(i8), false);
    case .I16:  return LLVMConstInt(llvm_type, cast(u64) value.(i16), false);
    case .I32:  return LLVMConstInt(llvm_type, cast(u64) value.(i32), false);
    case .I64, .Untyped_Integer: return LLVMConstInt(llvm_type, cast(u64) value.(i64), false);
    case .Int: return LLVMConstInt(llvm_type, cast(u64) value.(int), false);
    case .U8:  return LLVMConstInt(llvm_type, cast(u64) value.(u8), false);
    case .U16: return LLVMConstInt(llvm_type, cast(u64) value.(u16), false);
    case .U32: return LLVMConstInt(llvm_type, cast(u64) value.(u32), false);
    case .U64: return LLVMConstInt(llvm_type, cast(u64) value.(u64), false);
    case .UInt: return LLVMConstInt(llvm_type, cast(u64) value.(uint), false);

    case .F32: return LLVMConstReal(llvm_type, cast(f64) value.(f32));
    case .F64, .Untyped_Float: return LLVMConstReal(llvm_type, value.(f64));

    case .Char, .Untyped_Char: return LLVMConstInt(llvm_type, cast(u64) value.(rune), false);

    case .Untyped_String, .CString, .String: {
      return value == nil ? LLVMConstNull(llvm_type) : generator_string_to_llvm(generator, value.(string), type.kind == .CString);
    } 
    
    case .Rawptr, .Pointer, .Offset_Relative_Pointer, .Dynamic_Pointer: fallthrough;
    case .Array, .Slice, .Struct, .Union, .Procedure, .Function, .Untyped_Null: { 
      return LLVMConstNull(llvm_type);
    }

    // Constants for self-relative pointers are special as we actually want the pointer 'null' and not '0' for the relative base.
    case .Self_Relative_Pointer: return LLVMConstNull(LLVMPointerType(LLVMVoidType(), 0))
  }

  assert(false);
  return nil;
}

/**
* Gets the LLVM representation for a string.
* 
* @param generator    The reference to the generator.
* @parma string_value The string value.
* @parma is_cstring   Is the string a cstring?
* @return The LLVM representation of the string.
*/
generator_string_to_llvm :: proc(generator: ^Generator, string_value: string, is_cstring: bool) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We only ever want to emit a string a single time and reuse it.
  llvm_string_pointer, found_string := generator.llvm_strings[string_value];
  if !found_string {
    // We guarantee that all constant strings (or rather their data buffers) are going to be null-terminated.
    cstring_value := strings.clone_to_cstring(string_value, context.temp_allocator);
    llvm_string_pointer = LLVMBuildGlobalStringPtr(generator.builder, cstring_value, make_string_name(generator));
    generator.llvm_strings[string_value] = llvm_string_pointer;
  }

  if is_cstring {
    return llvm_string_pointer;
  } else {
    llvm_string_element_values: []LLVMValueRef = {llvm_string_pointer, LLVMConstInt(LLVMInt64Type(), cast(u64) len(string_value), false)};
    llvm_string_type := generator_type_to_llvm(generator, generator.storage.type_string);
    return LLVMConstNamedStruct(llvm_string_type, raw_data(llvm_string_element_values), cast(u32) len(llvm_string_element_values));
  }
}

/**
* Resolves the LLVM representation for a name.
* 
* @param generator       The reference to the generator.
* @parma routine_context The context of the routine.
* @parma expression      The expression of the name.
* @parma name            The name to resolved.
* @return 1. The LLVM value of the resolved name; 2. The LLVM type of the resolved name.
*/
generator_name_to_llvm :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, name: string) -> (LLVMValueRef, LLVMTypeRef) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // First we look up the name as a local symbol.
  llvm_local, found_local := generator_get_local_symbol(routine_context, name);
  if found_local {
    return llvm_local.llvm_value, llvm_local.llvm_type;
  }

  // Look for a globally declared symbol.
  symbol := generator_get_resolved_symbol(generator, expression);

  return generator_symbol_to_llvm(generator, symbol), generator_type_to_llvm(generator, symbol.type);
}

/**
* Gets the default initializer value for a type.
* 
* @param generator The reference to the generator.
* @param type      The type to get the default initializer for.
* @return The default initializer value for the type.
*/
generator_default_initializer_value :: proc(generator: ^Generator, type: ^Type) -> LLVMValueRef {
  llvm_type := generator_type_to_llvm(generator, type);
  return LLVMConstNull(llvm_type);
}

/**
* Creates a LLVM enum attribute.
* 
* @param name  The name of the LLVM enum attribute.
* @param value The value of the LLVM enum attribute.
* @return The LLVM enum attribute.
*/
generator_create_llvm_enum_attribute :: proc(name: cstring, value: u64 = 0) -> LLVMAttributeRef {
  kind := LLVMGetEnumAttributeKindForName(name, cast(u64) len(name));
  return LLVMCreateEnumAttribute(LLVMGetGlobalContext(), kind, value);
}

/**
* Creates a LLVM enum attribute type.
* 
* @param name      The name of the LLVM enum attribute type.
* @param llvm_type The LLVM type of the LLVM enum attribute type.
* @return The LLVM enum attribute type.
*/
generator_create_llvm_enum_attribute_type :: proc(name: cstring, llvm_type: LLVMTypeRef) -> LLVMAttributeRef {
  kind := LLVMGetEnumAttributeKindForName(name, cast(u64) len(name));
  return LLVMCreateTypeAttribute(LLVMGetGlobalContext(), kind, llvm_type);
}

/**
* Checks whether or not an expression accesses an SoA or AoSoA collection.
* 
* @param generator         The reference to the generator.
* @param member_expression The member expression to check.
* @return True if the expression is accesses an SoA or AoSoA collection.
*/
generator_member_is_soa_or_aosoa_access :: proc(generator: ^Generator, member_expression: ^Expression) -> bool {
  if member_expression.kind == .Index {
    index_type := generator_get_resolved_type(generator, (cast(^Expression_Index) member_expression).expression);
    if type_is_absolute_pointer(index_type) || type_is_self_relative_pointer(index_type) {
      index_type = index_type.base;
    }
    return type_is_soa_or_aosoa(index_type);
  } else {
    return false;
  }
}

/**
* Get the hash function for a type.
* 
* @param generator The reference to the generator.
* @param type      The type to get the hash function for.
* @return The LLVM hash routine. 
*/
generator_get_hash_function_for_type :: proc(generator: ^Generator, type: ^Type) -> LLVM_Routine_Tuple {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // First we try to get a hasher that might already be present.
  // Pointer types are all treated as a 'rawptr'.
  type := type;
  is_pointer_like := type_is_absolute_pointer_like(type) || type_is_cstring(type);
  if (is_pointer_like) {
    type = generator.storage.type_rawptr;
  }

  hasher, found := generator.llvm_hasher_functions[type];
  if found {
    return hasher;
  }

  // We have to remember to take into account that some types are actually passed as pointers.
  llvm_type := generator_type_to_llvm(generator, type);
  llvm_hasher_parameter_types: []LLVMTypeRef = {generator_type_is_passed_as_pointer(type) ? LLVMPointerType(LLVMVoidType(), 0) : llvm_type};
  llvm_hasher, llvm_hasher_type := generator_emit_temporary_routine(
    generator,
    to_cstring_f("runtime.hash_%v", type.name),
    LLVMInt64Type(),
    llvm_hasher_parameter_types,
  );

  builder := generator.temporary_builder;
  routine_context: Routine_Context;
  routine_context.llvm_symbol = llvm_hasher;
  routine_context.declaration_block = generator_append_block(&routine_context);
  LLVMPositionBuilderAtEnd(builder, routine_context.declaration_block);

  builtin_routine_hash_data := generator.llvm_builtin_routines[Builtin_Routine.Hash_Data];

  is_scalar := type_is_scalar(type);
  is_string := type_is_string(type);
  is_any := type_is_any(type);
  is_offset_relative_pointer := type_is_offset_relative_pointer(type);
  is_dynamic_pointer := type_is_dynamic_pointer(type);

  llvm_return_hash: LLVMValueRef;
  if is_pointer_like || is_scalar || is_string || is_any || is_offset_relative_pointer || is_dynamic_pointer {
    llvm_args: []LLVMValueRef;
    if (is_pointer_like || is_scalar || is_offset_relative_pointer) {
      llvm_temp := LLVMBuildAlloca(builder, llvm_type, make_temp_name(&routine_context));
      LLVMBuildStore(builder, LLVMGetParam(llvm_hasher, 0), llvm_temp);
      llvm_args = {llvm_temp, LLVMConstInt(LLVMInt64Type(), cast(u64) type_size_of(type), false)};
    } else if is_string {
      llvm_data_pointer := generator_emit_gep_value_data(builder, &routine_context, llvm_type, LLVMGetParam(llvm_hasher, 0));
      llvm_data_pointer = LLVMBuildLoad2(
        builder,
        generator_type_to_llvm(generator, generator.storage.type_rawptr),
        llvm_data_pointer,
        make_value_name(&routine_context),
      );
      llvm_length_pointer := generator_emit_gep_value_length(builder, &routine_context, llvm_type, LLVMGetParam(llvm_hasher, 0));
      llvm_length_pointer = LLVMBuildLoad2(
        builder,
        generator_type_to_llvm(generator, generator.storage.type_int),
        llvm_length_pointer,
        make_value_name(&routine_context),
      );
      llvm_args = {llvm_data_pointer, llvm_length_pointer};
    } else {
      assert(is_any || is_dynamic_pointer)
      llvm_args = {LLVMGetParam(llvm_hasher, 0), LLVMConstInt(LLVMInt64Type(), cast(u64) type_size_of(type), false)};
    }
    
    llvm_return_hash = LLVMBuildCall2(
      builder,
      builtin_routine_hash_data.llvm_tuple.llvm_type,
      builtin_routine_hash_data.llvm_tuple.llvm_symbol,
      raw_data(llvm_args),
      cast(u32) len(llvm_args),
      make_value_name(&routine_context),
    );
  } else {
    assert(type_is_struct(type));

    type_struct := cast(^Type_Struct) type;
    field_count := len(type_struct.fields);
    
    FNV_OFFSET_BASIS: u64 = 0xcbf29ce484222325;
    llvm_return_hash = LLVMConstInt(LLVMInt64Type(), FNV_OFFSET_BASIS, false);
    FNV_PRIME: u64 = 0x100000001b3;
    llvm_fnv_prime := LLVMConstInt(LLVMInt64Type(), FNV_PRIME, false);

    llvm_struct := LLVMGetParam(llvm_hasher, 0);
    for field_index := 0; field_index < field_count; field_index += 1 {
      field_type := type_struct.fields[field_index].type;

      llvm_field_pointer := generator_emit_gep_field(builder, &routine_context, llvm_type, llvm_struct, field_index);
      llvm_field_hasher_argument := llvm_field_pointer;
      if !generator_type_is_passed_as_pointer(field_type) {
        llvm_field_type := generator_type_to_llvm(generator, field_type);
        llvm_field_hasher_argument = LLVMBuildLoad2(builder, llvm_field_type, llvm_field_pointer, make_value_name(&routine_context));
      }

      llvm_field_hasher := generator_get_hash_function_for_type(generator, field_type);
      // We have to reset the builder here because it might have been used when getting the hasher for the field.
      LLVMPositionBuilderAtEnd(builder, routine_context.declaration_block);

      llvm_field_hasher_arguments: []LLVMValueRef = {llvm_field_hasher_argument};
      llvm_field_hash := LLVMBuildCall2(
        builder,
        llvm_field_hasher.llvm_type,
        llvm_field_hasher.llvm_symbol,
        raw_data(llvm_field_hasher_arguments),
        cast(u32) len(llvm_field_hasher_arguments),
        make_value_name(&routine_context),
      );
      llvm_return_hash = LLVMBuildXor(builder, llvm_return_hash, llvm_field_hash, make_value_name(&routine_context));
      llvm_return_hash = LLVMBuildMul(builder, llvm_return_hash, llvm_fnv_prime, make_value_name(&routine_context));
    }
  }
  LLVMBuildRet(builder, llvm_return_hash);

  hasher_routine_tuple := LLVM_Routine_Tuple{llvm_hasher, llvm_hasher_type};
  generator.llvm_hasher_functions[type] = hasher_routine_tuple;
  return hasher_routine_tuple;
}

/**
* Get the comparer function for a type.
* 
* @param generator The reference to the generator.
* @param type      The type to get the comparer function for.
* @return The LLVM comparer routine. 
*/
generator_get_compare_function_for_type :: proc(generator: ^Generator, type: ^Type) -> LLVM_Routine_Tuple {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type := type;
  type_is_pointer_like := type_is_absolute_pointer_like(type) || type_is_cstring(type);
  if (type_is_pointer_like) {
    type = generator.storage.type_rawptr;
  }

  comparer, found := generator.llvm_comparer_functions[type];
  if found {
    return comparer;
  }

  llvm_comparer: LLVMValueRef;
  llvm_comparer_type: LLVMTypeRef;

  if type_is_string(type) || type_is_cstring(type) {
    llvm_builtin_routine := generator.llvm_builtin_routines[type_is_string(type) ? Builtin_Routine.Compare_String : Builtin_Routine.Compare_CString];
    comparer_routine_tuple := LLVM_Routine_Tuple{llvm_builtin_routine.llvm_tuple.llvm_symbol, llvm_builtin_routine.llvm_tuple.llvm_type};
    generator.llvm_comparer_functions[type] = comparer_routine_tuple;
    return comparer_routine_tuple;
  }

  llvm_type := generator_type_to_llvm(generator, type);
  llvm_parameter_type := generator_type_is_passed_as_pointer(type) ? LLVMPointerType(LLVMVoidType(), 0) : llvm_type;
  llvm_comparer_parameter_types: []LLVMTypeRef = {llvm_parameter_type, llvm_parameter_type};
  llvm_comparer, llvm_comparer_type = generator_emit_temporary_routine(
    generator,
    to_cstring_f("runtime.compare_%v", type.name),
    LLVMInt1Type(), 
    llvm_comparer_parameter_types,
  );

  builder := generator.temporary_builder;
  routine_context: Routine_Context;
  routine_context.llvm_symbol = llvm_comparer;

  if type_is_pointer_like || type_is_scalar(type) || type_is_any(type) || type_is_offset_relative_pointer(type) || type_is_dynamic_pointer(type) {
    llvm_block := generator_append_block(&routine_context);
    LLVMPositionBuilderAtEnd(builder, llvm_block);
    llvm_return_comparison: LLVMValueRef;
    if (type_is_float(type)) {
      llvm_return_comparison = LLVMBuildFCmp(builder, .UEQ, LLVMGetParam(llvm_comparer, 0), LLVMGetParam(llvm_comparer, 1), make_value_name(&routine_context));
    } else if type_is_any(type) || type_is_dynamic_pointer(type) {
      llvm_data_a := generator_emit_gep_value_data(builder, &routine_context, llvm_type, LLVMGetParam(llvm_comparer, 0));
      llvm_data_a = LLVMBuildLoad2(builder, LLVMPointerType(LLVMVoidType(), 0), llvm_data_a, make_value_name(&routine_context));
      llvm_data_b := generator_emit_gep_value_data(builder, &routine_context, llvm_type, LLVMGetParam(llvm_comparer, 1));
      llvm_data_b = LLVMBuildLoad2(builder, LLVMPointerType(LLVMVoidType(), 0), llvm_data_b, make_value_name(&routine_context));
      llvm_return_comparison = LLVMBuildICmp(builder, .EQ, llvm_data_a, llvm_data_b, make_value_name(&routine_context));
    } else {
      llvm_return_comparison = LLVMBuildICmp(builder, .EQ, LLVMGetParam(llvm_comparer, 0), LLVMGetParam(llvm_comparer, 1), make_value_name(&routine_context));
    }
    LLVMBuildRet(builder, llvm_return_comparison);
  } else {
    assert(type_is_struct(type));

    type_struct := cast(^Type_Struct) type;
    field_count := len(type_struct.fields);
    llvm_field_blocks := make_dynamic_array_len_cap([dynamic]LLVMBasicBlockRef, 0, field_count, context.temp_allocator);
    for i := 0; i < field_count; i += 1 {
      append(&llvm_field_blocks, generator_append_block(&routine_context));
    }
    llvm_return_true_block := generator_append_block(&routine_context);
    LLVMPositionBuilderAtEnd(builder, llvm_return_true_block);
    LLVMBuildRet(builder, LLVMConstInt(LLVMInt1Type(), 1, false));
    llvm_return_false_block: LLVMBasicBlockRef;
    if field_count > 0 {
      llvm_return_false_block = generator_append_block(&routine_context);
      LLVMPositionBuilderAtEnd(builder, llvm_return_false_block);
      LLVMBuildRet(builder, LLVMConstInt(LLVMInt1Type(), 0, false));
    }

    llvm_struct_a := LLVMGetParam(llvm_comparer, 0);
    llvm_struct_b := LLVMGetParam(llvm_comparer, 1);
    for field_index := 0; field_index < field_count; field_index += 1 {
      field_type := type_struct.fields[field_index].type;

      llvm_field_block := llvm_field_blocks[field_index];
      LLVMPositionBuilderAtEnd(builder, llvm_field_block);
      
      llvm_field_pointer_a := generator_emit_gep_field(builder, &routine_context, llvm_type, llvm_struct_a, field_index);
      llvm_field_pointer_b := generator_emit_gep_field(builder, &routine_context, llvm_type, llvm_struct_b, field_index);
      llvm_field_comparer_argument_a := llvm_field_pointer_a;
      llvm_field_comparer_argument_b := llvm_field_pointer_b;
      if !generator_type_is_passed_as_pointer(field_type) {
        llvm_field_type := generator_type_to_llvm(generator, field_type);
        llvm_field_comparer_argument_a = LLVMBuildLoad2(builder, llvm_field_type, llvm_field_pointer_a, make_value_name(&routine_context));
        llvm_field_comparer_argument_b = LLVMBuildLoad2(builder, llvm_field_type, llvm_field_pointer_b, make_value_name(&routine_context));
      }

      llvm_field_comparer := generator_get_compare_function_for_type(generator, field_type);
      // We have to reset the builder here because it might have been used when getting the comparer for the field.
      LLVMPositionBuilderAtEnd(builder, llvm_field_block);

      llvm_field_comparer_arguments: []LLVMValueRef = {llvm_field_comparer_argument_a, llvm_field_comparer_argument_b};
      llvm_field_comparison_value := LLVMBuildCall2(
        builder,
        llvm_field_comparer.llvm_type,
        llvm_field_comparer.llvm_symbol,
        raw_data(llvm_field_comparer_arguments),
        cast(u32) len(llvm_field_comparer_arguments),
        make_value_name(&routine_context),
      );
      llvm_next_block := field_index + 1 >= field_count ? llvm_return_true_block : llvm_field_blocks[field_index + 1];
      LLVMBuildCondBr(builder, llvm_field_comparison_value, llvm_next_block, llvm_return_false_block);
    }
  }

  comparer_routine_tuple := LLVM_Routine_Tuple{llvm_comparer, llvm_comparer_type};
  generator.llvm_comparer_functions[type] = comparer_routine_tuple;
  return comparer_routine_tuple;
}

/**
* Checks whether or not a relative base of a relative pointer needs conversion.
*
* @param relative_base The relative base of the relative pointer to check.
* @return True if the relative pointer base needs conversion otherwise false.
*/
generator_relative_pointer_base_needs_conversion :: proc(relative_base: ^Type) -> bool {
  #partial switch relative_base.kind {
    case .I8, .I16, .I32, .U8, .U16, .U32: return true;
  }
  return false;
}

/**
* Gets the LLVM representation for a calling convention.
*
* @param calling_convention The calling convention to get the LLVM representation of.
* @return The corresponding LLVM calling convention.
*/
generator_calling_convention_to_llvm :: proc(calling_convention: Calling_Convention) -> LLVMCallConv {
  switch calling_convention {
    case .Nox: return .C;
    case .No_Context: return .C;
    case .C: return .C;
    case .Std_Call: return .X86Stdcall;
    case .Fast_Call: return .X86Fastcall;
    case .Win64: return .Win64;
    case: assert(false); return .C;
  }
}

/**
* Makes a mangled name from a symbol.
*
* @param symbol The symbol to get the mangled name of.
* @return The mangled name.  
*/
make_mangled_name :: proc(symbol: ^Symbol) -> cstring {
  result := to_cstring_f("%v.%v", symbol.package_file.pack.name, symbol.name);
  return result;
}

/**
* Makes a mangled name from a type.
*
* @param type   The type to get the mangled name of.
* @param symbol The symbol of the type.
* @return The mangled name.  
*/
make_mangled_name_from_type :: proc(type: ^Type, symbol: ^Symbol) -> cstring {
  result := to_cstring_f("%v.%v", symbol == nil ? "" : symbol.package_file.pack.name, type.name);
  return result;
}

/**
* Makes a name for a parameter.
*
* @param parameter_index The index of the parameter.
* @return The parameter name.  
*/
make_parameter_name :: proc(parameter_index: int) -> cstring {
  result := to_cstring_f("parameter_%v", parameter_index);
  return result;
}

/**
* Makes a name for a value.
*
* @param routine_contex The context of the routine.
* @return The value name.
*/
make_value_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("value_%v", routine_context.value_counter);
  routine_context.value_counter += 1;
  return result;
}

/**
* Makes a name for a variable.
*
* @param routine_contex The context of the routine.
* @parma name           The name of the variable.
* @return The variable name.
*/
make_variable_name :: proc(routine_context: ^Routine_Context, name: string) -> cstring {
  result := to_cstring_f("variable_%v_%v", routine_context.variable_counter, name);
  routine_context.variable_counter += 1;
  return result;
}

/**
* Makes a name for a temporary.
*
* @param routine_contex The context of the routine.
* @return The temporary name.
*/
make_temp_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("temp_%v", routine_context.temp_counter);
  routine_context.temp_counter += 1;
  return result;
}

/**
* Makes a name for a byval.
*
* @param routine_contex The context of the routine.
* @return The byval name.
*/
make_byval_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("byval_%v", routine_context.byval_counter);
  routine_context.byval_counter += 1;
  return result;
}

/**
* Makes a name for a sret.
*
* @param routine_contex The context of the routine.
* @return The sret name.
*/
make_sret_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("sret_%v", routine_context.sret_counter);
  routine_context.sret_counter += 1;
  return result;
}

/**
* Makes a name for an array.
*
* @param routine_contex The context of the routine.
* @return The array name.
*/
make_array_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("array_%v", routine_context.array_counter);
  routine_context.array_counter += 1;
  return result;
}

/**
* Makes a name for a slice.
*
* @param routine_contex The context of the routine.
* @return The slice name.
*/
make_slice_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("slice_%v", routine_context.slice_counter);
  routine_context.slice_counter += 1;
  return result;
}

/**
* Makes a name for a compound.
*
* @param routine_contex The context of the routine.
* @return The compound name.
*/
make_compound_name :: proc(routine_context: ^Routine_Context) -> cstring {
  result := to_cstring_f("compound_%v", routine_context.compound_counter);
  routine_context.compound_counter += 1;
  return result;
}

/**
* Makes a name for a string.
*
* @param generator The reference to the generator.
* @return The string name.
*/
make_string_name :: proc(generator: ^Generator) -> cstring {
  result := to_cstring_f("$string_%v", len(generator.llvm_strings));
  return result;
}

/**
* Converts a formatted string to a cstring.
*
* @param format The format of the string.
* @param args   The arguments of the format.
* @return The corresponding cstring.
*/
to_cstring_f :: proc(format: string, args: ..any) -> cstring {
  return to_cstring(fmt.tprintf(format, ..args));
}

/**
* Converts a string to a cstring.
*
* @param str The string to convert.
* @return The corresponding cstring.
*/
to_cstring :: proc(str: string) -> cstring {
  return strings.clone_to_cstring(str, context.temp_allocator);
}
