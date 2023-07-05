package nox

import "tracy"

/**
* Initialize all types.
*
* @param generator The reference to the generator.
*/
generator_init_types :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We convert every type we have to the corresponding LLVM type.
  for type in generator.resolver_output.reachable_types {
    if generator_type_should_skip_initialization(type) {
      continue;
    }
    generator_init_type(generator, type);
  }

  free_all(context.temp_allocator);
}

/**
* Initialize a type.
*
* @param generator The reference to the generator.
* @param type      The type to initialize.
*/
generator_init_type :: proc(generator: ^Generator, type: ^Type) -> LLVMTypeRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Check if we might have already initialized the type.
  llvm_type, found := generator.llvm_types[type];
  if found {
    return llvm_type;
  }

  // We use the fact that LLVM-15 works with opaque pointers.
  // That allows us the represent pretty much any pointer as: LLVMPointerType(LLVMVoidType(), 0).
  // This is handy, as that bypasses any potential problems in the cyclic dependency between types.
  // Right now we try to use that only where actully necessary.

  switch type.kind {
    case .Void: llvm_type = LLVMVoidType();

    case .B8:  llvm_type = LLVMInt8Type();
    case .B16: llvm_type = LLVMInt16Type();
    case .B32: llvm_type = LLVMInt32Type();
    case .B64: llvm_type = LLVMInt64Type();
    case .Bool, .Untyped_Boolean: llvm_type = LLVMInt1Type();

    case .I8:  llvm_type = LLVMInt8Type();
    case .I16: llvm_type = LLVMInt16Type();
    case .I32: llvm_type = LLVMInt32Type();
    case .I64: llvm_type = LLVMInt64Type();
    case .Int: llvm_type = LLVMInt64Type();
    case .U8:  llvm_type = LLVMInt8Type();
    case .U16: llvm_type = LLVMInt16Type();
    case .U32: llvm_type = LLVMInt32Type();
    case .U64: llvm_type = LLVMInt64Type();
    case .UInt, .Untyped_Integer: llvm_type = LLVMInt64Type();

    case .F32: llvm_type = LLVMFloatType();
    case .F64, .Untyped_Float: llvm_type = LLVMDoubleType();

    case .Untyped_Char, .Char: llvm_type = LLVMInt32Type();

    case .Enumeration, .Constant: llvm_type = generator_type_to_llvm(generator, type.base);

    case .Untyped_String, .String: {
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), "string");
      llvm_string_struct_element_types: []LLVMTypeRef = { LLVMPointerType(LLVMInt8Type(), 0), LLVMInt64Type() };
      LLVMStructSetBody(llvm_type, raw_data(llvm_string_struct_element_types), cast(u32) len(llvm_string_struct_element_types), false);
    } 
    case .CString: llvm_type = LLVMPointerType(LLVMInt8Type(), 0);

    case .Rawptr, .Pointer: llvm_type = LLVMPointerType(LLVMVoidType(), 0);
    case .Self_Relative_Pointer, .Offset_Relative_Pointer: llvm_type = generator_type_to_llvm(generator, (cast(^Type_Relative_Pointer) type).relative_base);
    case .SoA_Layout_Pointer, .AoSoA_Layout_Pointer, .Dynamic_Pointer: {
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
      llvm_dynamic_pointer_struct_element_types: []LLVMTypeRef = { LLVMPointerType(LLVMVoidType(), 0), LLVMPointerType(LLVMVoidType(), 0) };
      LLVMStructSetBody(llvm_type, raw_data(llvm_dynamic_pointer_struct_element_types), cast(u32) len(llvm_dynamic_pointer_struct_element_types), false);
    }
    case .Array: {
      type_array := cast(^Type_Array) type;
      
      if type_array.has_incomplete_elements {
        return nil;
      }

      llvm_base_type := generator_type_to_llvm(generator, type.base);
      if type_is_array_soa_or_aosoa(type) {
        llvm_type = llvm_base_type;
      } else {
        llvm_type = LLVMArrayType(llvm_base_type, cast(u32) type_array.number_of_elements);
      }
    }
    case .Dynamic_Array: {
      if type_is_dynamic_array_soa_or_aosoa(type) {
        llvm_type = generator_type_to_llvm(generator, type.base);
      } else {
        llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
        llvm_dynamic_array_struct_element_types: []LLVMTypeRef = {
          LLVMPointerType(LLVMVoidType(), 0),
          LLVMInt64Type(),
          LLVMInt64Type(),
          generator_type_to_llvm(generator, generator.storage.cached_runtime_types.allocator),
        };
        LLVMStructSetBody(llvm_type, raw_data(llvm_dynamic_array_struct_element_types), cast(u32) len(llvm_dynamic_array_struct_element_types), false);
      }
    }
    case .Slice: {
      if type_is_slice_soa_or_aosoa(type) {
        llvm_type = generator_type_to_llvm(generator, type.base);
      } else {
        llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
        llvm_slice_struct_element_types: []LLVMTypeRef = { LLVMPointerType(LLVMVoidType(), 0), LLVMInt64Type() };
        LLVMStructSetBody(llvm_type, raw_data(llvm_slice_struct_element_types), cast(u32) len(llvm_slice_struct_element_types), false);
      }
    }
    case .Map: {
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
      llvm_map_struct_element_types: []LLVMTypeRef = {
        LLVMPointerType(LLVMVoidType(), 0),
        LLVMInt64Type(),
        LLVMInt64Type(),
        generator_type_to_llvm(generator, generator.storage.cached_runtime_types.allocator),
        LLVMPointerType(LLVMVoidType(), 0),
        LLVMPointerType(LLVMVoidType(), 0),
      };
      LLVMStructSetBody(llvm_type, raw_data(llvm_map_struct_element_types), cast(u32) len(llvm_map_struct_element_types), false);
    }
    case .Tuple: {
      type_tuple := cast(^Type_Tuple) type;
      
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
      llvm_tuple_element_types := make_dynamic_array_len_cap([dynamic]LLVMTypeRef, 0, len(type_tuple.elements), context.temp_allocator);
      for element in type_tuple.elements {
        append(&llvm_tuple_element_types, generator_type_to_llvm(generator, element.type));
      }
      LLVMStructSetBody(llvm_type, raw_data(llvm_tuple_element_types), cast(u32) len(llvm_tuple_element_types), false);
    }

    case .Struct: {
      type_struct := cast(^Type_Struct) type;

      struct_name := make_mangled_name_from_type(type, type.symbol);
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), struct_name);

      llvm_struct_element_types := make_dynamic_array_len_cap([dynamic]LLVMTypeRef, 0, len(type_struct.fields), context.temp_allocator);
      for field in type_struct.fields {
        append(&llvm_struct_element_types, generator_type_to_llvm(generator, field.type));
      }

      LLVMStructSetBody(llvm_type, raw_data(llvm_struct_element_types), cast(u32) len(llvm_struct_element_types), false);
    }
    case .Union: {
      type_union := cast(^Type_Union) type;

      struct_name := make_mangled_name_from_type(type, type.symbol);
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), struct_name);

      llvm_union_element_types: []LLVMTypeRef;
      if type_union.biggest_type == nil {
        llvm_union_element_types = {LLVMInt64Type()};
      } else {
        llvm_union_element_types = {LLVMInt64Type(), generator_type_to_llvm(generator, type_union.biggest_type)};
      }

      LLVMStructSetBody(llvm_type, raw_data(llvm_union_element_types), cast(u32) len(llvm_union_element_types), false);
    }

    case .Procedure, .Function: {
      type_routine := cast(^Type_Routine) type;

      llvm_parameter_types := make_dynamic_array_len_cap([dynamic]LLVMTypeRef, 0, len(type_routine.parameters), context.temp_allocator);
      
      // Returning a struct by value is a little more involved.
      // Instead of actually returning something we transform the to be returned struct to be part of the parameter list.
      // We also always insert it as the very first parameter.
      return_type := type_routine.return_type;
      llvm_return_type := generator_type_to_llvm(generator, return_type);
      if generator_type_is_passed_as_pointer(return_type) {
        append(&llvm_parameter_types, LLVMPointerType(llvm_return_type, 0));
        llvm_return_type = LLVMVoidType();
      }

      // For the potential 'context' and 'self' pointers we simply use 'void' as their base type.
      // This is kind of a hack but at the same time its not important as LLVM does not care about the base types of pointers...
      if type_routine_has_context(type_routine) {
        append(&llvm_parameter_types, LLVMPointerType(LLVMVoidType(), 0));
      }
      if .Is_Method in type_routine.routine_flags {
        append(&llvm_parameter_types, LLVMPointerType(LLVMVoidType(), 0));
      }
      
      for parameter in type_routine.parameters {
        parameter_type := parameter.type;
        llvm_parameter_type := generator_type_to_llvm(generator, parameter_type);
        
        // Parameters that are passed by value have to be converted to pointers.
        if generator_type_is_passed_as_pointer(parameter_type) {
          llvm_parameter_type = LLVMPointerType(llvm_parameter_type, 0);
        }

        append(&llvm_parameter_types, llvm_parameter_type);
      }

      has_c_varargs := .Has_C_Varargs in type_routine.routine_flags;
      llvm_type = LLVMFunctionType(llvm_return_type, raw_data(llvm_parameter_types), cast(u32) len(llvm_parameter_types), cast(LLVMBool) has_c_varargs);
    }

    case .Interface: {
      // Interfaces create their corresponding vtable type.
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), make_mangled_name_from_type(type, type.symbol));
      llvm_vtable_struct_element_types := make_dynamic_array_len_cap([dynamic]LLVMTypeRef, 0, len(type.methods), context.temp_allocator);
      for method in type.methods {
        append(&llvm_vtable_struct_element_types, generator_type_to_llvm(generator, method.type));
      }
      LLVMStructSetBody(llvm_type, raw_data(llvm_vtable_struct_element_types), cast(u32) len(llvm_vtable_struct_element_types), false);
    }

    case .Any: {
      llvm_type = LLVMStructCreateNamed(LLVMGetGlobalContext(), to_cstring(type.name));
      llvm_any_struct_element_types: []LLVMTypeRef = { LLVMPointerType(LLVMVoidType(), 0), LLVMInt64Type() };
      LLVMStructSetBody(llvm_type, raw_data(llvm_any_struct_element_types), cast(u32) len(llvm_any_struct_element_types), false);
    }
    case .Typeid: llvm_type = LLVMInt64Type();

    case .Untyped_Null: {
      llvm_type = LLVMPointerType(LLVMVoidType(), 0);
    }

    case .None, .Generic: fallthrough;
    case: assert(false);
  }

  assert(llvm_type != nil);

  generator.llvm_types[type] = llvm_type;
  return llvm_type;
}

/**
* Gets the LLVM representation for a type.
*
* @param generator The reference to the generator.
* @param type      The type to get the LLVM representation of.
* @return The LLVM representation of the type.
*/
generator_type_to_llvm :: proc(generator: ^Generator, type: ^Type) -> LLVMTypeRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_to_resolve := type_unqualify(type);
  llvm_type, found := generator.llvm_types[type_to_resolve];
  if !found {
    llvm_type = generator_init_type(generator, type_to_resolve);
  }

  // Represent routines as pointers.
  if type_is_routine(type) {
    llvm_type = LLVMPointerType(llvm_type, 0);
  }

  assert(llvm_type != nil);
  return llvm_type;
}

/**
* Gets the LLVM representation for a type without promoting routine types.
*
* @param generator The reference to the generator.
* @param type      The type to get the LLVM representation of.
* @return The LLVM representation of the type.
*/
generator_type_to_llvm_without_routine_promotion :: proc(generator: ^Generator, type: ^Type) -> LLVMTypeRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_to_resolve := type_unqualify(type);
  llvm_type, found := generator.llvm_types[type_to_resolve];
  if !found {
    llvm_type = generator_init_type(generator, type_to_resolve);
  }

  assert(llvm_type != nil);
  return llvm_type;
}

/**
* Emit the type info table.
*
* @param generator     The reference to the generator.
* @param rttr_disabled Is RTTR disabled?
*/
generator_emit_type_info_table :: proc(generator: ^Generator, rttr_disabled: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  
  type_info_table_length := generator_emit_type_info_table_indices(generator, rttr_disabled);
  generator_emit_type_info_table_array(generator, rttr_disabled, type_info_table_length);
  generator_emit_type_info_table_data(generator, rttr_disabled);

  free_all(context.temp_allocator);
}

/**
* Emit the type info indices table.
*
* @param generator     The reference to the generator.
* @param rttr_disabled Is RTTR disabled?
* @return The length of the type info indices table.
*/
generator_emit_type_info_table_indices :: proc(generator: ^Generator, rttr_disabled: bool) -> int {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_info_indices_length := cast(u32) len(generator.resolver_output.reachable_types) + 1;
  llvm_indices_element_type := LLVMInt64Type();

  generator.llvm_type_info_table_indices_map_type = LLVMArrayType(llvm_indices_element_type, type_info_indices_length);
  generator.llvm_type_info_table_indices_map = LLVMAddGlobal(generator.module, generator.llvm_type_info_table_indices_map_type, "$type_info_indices");
  LLVMSetGlobalConstant(generator.llvm_type_info_table_indices_map, true);
  llvm_type_info_table_indices_map_elements := make_dynamic_array([dynamic]LLVMValueRef, context.temp_allocator);

  append(&llvm_type_info_table_indices_map_elements, LLVMConstInt(llvm_indices_element_type, 0, false));

  // The table will always have at least one element.
  // This first element at index 0 allows looking up types which do not have any actual data in the table.
  type_info_table_length := 1;
  for type in generator.resolver_output.reachable_types {
    should_skip_type_in_table := generator_type_should_skip_in_type_table(type, rttr_disabled);
    
    type_info_table_index: u64;
    if should_skip_type_in_table {
      type_info_table_index = 0;
    } else {
      type_info_table_index = cast(u64) type_info_table_length;
    }
    append(&llvm_type_info_table_indices_map_elements, LLVMConstInt(llvm_indices_element_type, type_info_table_index, false));
    _, found := generator.type_info_table_indices[type.id];
    assert(!found);
    generator.type_info_table_indices[type.id] = type_info_table_index;

    if !should_skip_type_in_table {
      type_info_table_length += 1;
    }
  }
  llvm_type_info_table_indices_map_data := LLVMConstArray(llvm_indices_element_type, raw_data(llvm_type_info_table_indices_map_elements), type_info_indices_length);
  LLVMSetInitializer(generator.llvm_type_info_table_indices_map, llvm_type_info_table_indices_map_data);

  return type_info_table_length;
}

/**
* Emit the array of the type info data table.
*
* @param generator              The reference to the generator.
* @param rttr_disabled          Is RTTR disabled?
* @param type_info_table_length The length of the type info indices table.
*/
generator_emit_type_info_table_array :: proc(generator: ^Generator, rttr_disabled: bool, type_info_table_length: int) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type_info_type := generator_type_to_llvm(generator, generator.storage.cached_runtime_types.type_info);
  type_info_variant_type := generator.storage.cached_runtime_types.type_info_variant;
  llvm_type_info_variant_type := generator_type_to_llvm(generator, type_info_variant_type);
  generator.llvm_type_info_table_type = LLVMArrayType(llvm_type_info_type, cast(u32) type_info_table_length);
  generator.llvm_type_info_table = LLVMAddGlobal(generator.module, generator.llvm_type_info_table_type, "$type_info_data");

  llvm_type_info_table_elements := make_dynamic_array([dynamic]LLVMValueRef, context.temp_allocator);

  // The first element is always an invalid type data.
  append(&llvm_type_info_table_elements, LLVMConstNull(llvm_type_info_type));
  
  // Here we create the whole type info array with all constant data.
  for type in generator.resolver_output.reachable_types {
    if generator_type_should_skip_in_type_table(type, rttr_disabled) {
      continue;
    }

    // The element values here have to match the order in which they are defined in the 'Type_Info' struct in the 'runtime' package.
    type_info_kind := type_info_kind(type);
    llvm_type_info_kind := LLVMConstInt(LLVMInt64Type(), cast(u64) type_info_kind, false);
    llvm_typeid := LLVMConstInt(LLVMInt64Type(), cast(u64) type.id, false);
    llvm_size := LLVMConstInt(LLVMInt64Type(), cast(u64) type_size_of(type), false);
    llvm_alignment := LLVMConstInt(LLVMInt64Type(), cast(u64) type_alignment_of(type), false);
    llvm_name := generator_string_to_llvm(generator, type.name, false);
    llvm_type_info_variant := LLVMConstNull(llvm_type_info_variant_type);
    llvm_type_info_element_values: []LLVMValueRef = {llvm_type_info_kind, llvm_typeid, llvm_size, llvm_alignment, llvm_name, llvm_type_info_variant};
    llvm_type_info_table_element := LLVMConstStruct(raw_data(llvm_type_info_element_values), cast(u32) len(llvm_type_info_element_values), false);
    append(&llvm_type_info_table_elements, llvm_type_info_table_element);
  }
  
  llvm_type_info_table_data := LLVMConstArray(llvm_type_info_type, raw_data(llvm_type_info_table_elements), cast(u32) len(llvm_type_info_table_elements));
  LLVMSetInitializer(generator.llvm_type_info_table, llvm_type_info_table_data);
}

/**
* Emit the data of the type info data table.
*
* @param generator     The reference to the generator.
* @param rttr_disabled Is RTTR disabled?
*/
generator_emit_type_info_table_data :: proc(generator: ^Generator, rttr_disabled: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_info_variant_type := generator.storage.cached_runtime_types.type_info_variant;
  routine_context := &generator.initializer_routine_context;

  llvm_elements := make_dynamic_array([dynamic]LLVMValueRef, context.temp_allocator);

  // The actual individual Type_Info_Data has to be created in the static initializer constructor as it contains non-constant data like pointers.
  LLVMPositionBuilderAtEnd(generator.builder, generator.initializer_block);
  for storage_type in generator.resolver_output.reachable_types {
    type := storage_type;

    if generator_type_should_skip_in_type_table(type, rttr_disabled) {
      continue;
    }

    type_info_kind := type_info_kind(type);
    type_info_type := generator_type_get_type_info_type_for_kind(generator.storage, type_info_kind);
    if type_info_type == nil {
      continue;
    }
    llvm_variant := LLVMConstNull(generator_type_to_llvm(generator, type_info_type));

    switch type_info_kind {
      case .Integer: {
        llvm_is_signed := LLVMConstInt(LLVMInt1Type(), cast(u64) (!TYPE_METRICS[type.kind].unsigned), false);
        llvm_variant_elements: []LLVMValueRef = {llvm_is_signed};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Enumeration: {
        type_enumeration := cast(^Type_Enumeration) type;

        llvm_base := generator_type_get_type_info_pointer(generator, type.base);

        llvm_is_flags := LLVMConstInt(LLVMInt1Type(), cast(u64) type_enumeration.is_flags, false);

        item_count := len(type_enumeration.items);

        clear(&llvm_elements);
        for item in type_enumeration.items {
          append(&llvm_elements, generator_string_to_llvm(generator, item.name, false));
        }
        llvm_names_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_string, raw_data(llvm_elements), item_count);
        
        clear(&llvm_elements);
        for item in type_enumeration.items {
          item_value: u64;
          #partial switch type.base.kind {
            case .I8:   item_value = cast(u64) item.value.(i8);
            case .I16:  item_value = cast(u64) item.value.(i16);
            case .I32:  item_value = cast(u64) item.value.(i32);
            case .I64:  item_value = cast(u64) item.value.(i64);
            case .Int:  item_value = cast(u64) item.value.(int);
            case .U8:   item_value = cast(u64) item.value.(u8);
            case .U16:  item_value = cast(u64) item.value.(u16);
            case .U32:  item_value = cast(u64) item.value.(u32);
            case .U64:  item_value = cast(u64) item.value.(u64);
            case .UInt: item_value = cast(u64) item.value.(uint);
          }
          append(&llvm_elements, LLVMConstInt(LLVMInt64Type(), item_value, false));
        }
        llvm_values_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_uint, raw_data(llvm_elements), item_count);

        llvm_variant_elements: []LLVMValueRef = {llvm_base, llvm_is_flags, llvm_names_slice, llvm_values_slice};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .String: {
        llvm_is_cstring := LLVMConstInt(LLVMInt1Type(), cast(u64) (type.kind == .CString), false);
        llvm_variant_elements: []LLVMValueRef = {llvm_is_cstring};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Pointer, .Dynamic_Pointer: {
        llvm_base := generator_type_get_type_info_pointer(generator, type.base);
        llvm_variant_elements: []LLVMValueRef = {llvm_base};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Layout_Pointer: {
        llvm_base := generator_type_get_type_info_pointer(generator, type.base);
        layout_modifier := (cast(^Type_Struct) type.base.base).layout_info.modifier;
        llvm_layout_modifier := LLVMConstInt(LLVMInt64Type(), cast(u64) layout_modifier, false);
        llvm_variant_elements: []LLVMValueRef = {llvm_base, llvm_layout_modifier};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Relative_Pointer: {
        llvm_relative_pointer_kind := LLVMConstInt(LLVMInt64Type(), type.kind == .Self_Relative_Pointer ? 0 : 1, false);
        llvm_base := generator_type_get_type_info_pointer(generator, type.base);
        llvm_relative_base := generator_type_get_type_info_pointer(generator, (cast(^Type_Relative_Pointer) type).relative_base);
        llvm_variant_elements: []LLVMValueRef = {llvm_relative_pointer_kind, llvm_base, llvm_relative_base};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Array: {
        type_array := cast(^Type_Array) type;
        llvm_base := generator_type_get_type_info_pointer(generator, type.base);
        llvm_number_of_elements := LLVMConstInt(LLVMInt64Type(), cast(u64) type_array.number_of_elements, false);
        llvm_layout_modifier := LLVMConstInt(LLVMInt64Type(), cast(u64) type_array.layout_info.modifier, false);
        llvm_variant_elements: []LLVMValueRef = {llvm_base, llvm_layout_modifier, llvm_number_of_elements};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Dynamic_Array, .Slice: {
        layout_modifier := type_is_dynamic_array(type) ? (cast(^Type_Dynamic_Array) type).layout_info.modifier : (cast(^Type_Slice) type).layout_info.modifier;
        llvm_base := generator_type_get_type_info_pointer(generator, type.base);
        llvm_layout_modifier := LLVMConstInt(LLVMInt64Type(), cast(u64) layout_modifier, false);
        llvm_variant_elements: []LLVMValueRef = {llvm_base, llvm_layout_modifier};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Map: {
        type_map := cast(^Type_Map) type;
        llvm_key := generator_type_get_type_info_pointer(generator, type_map.key);
        llvm_value := generator_type_get_type_info_pointer(generator, type_map.value);
        llvm_variant_elements: []LLVMValueRef = {llvm_key, llvm_value};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Tuple: {
        type_tuple := cast(^Type_Tuple) type;

        item_count := len(type_tuple.elements);

        clear(&llvm_elements);
        for element_type in type_tuple.elements {
          append(&llvm_elements, generator_type_get_type_info_pointer(generator, element_type.type));
        }
        llvm_types_slice := generator_type_make_type_info_slice_type_info_pointer(generator, routine_context, raw_data(llvm_elements), item_count);

        clear(&llvm_elements);
        for element in type_tuple.elements {
          append(&llvm_elements, LLVMConstInt(LLVMInt64Type(), cast(u64) element.offset, false));
        }
        llvm_offsets_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_uint, raw_data(llvm_elements), item_count);

        llvm_variant_elements: []LLVMValueRef = {llvm_types_slice, llvm_offsets_slice};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Struct: {
        type_struct := cast(^Type_Struct) type;

        item_count := len(type_struct.fields);

        clear(&llvm_elements);
        for field in type_struct.fields {
          append(&llvm_elements, generator_type_get_type_info_pointer(generator, field.type));
        }
        llvm_types_slice := generator_type_make_type_info_slice_type_info_pointer(generator, routine_context, raw_data(llvm_elements), item_count);
        
        clear(&llvm_elements);
        for field in type_struct.fields {
          append(&llvm_elements, generator_string_to_llvm(generator, field.name, false));
        }
        llvm_names_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_string, raw_data(llvm_elements), item_count);
        
        clear(&llvm_elements);
        for field in type_struct.fields {
          append(&llvm_elements, LLVMConstInt(LLVMInt64Type(), cast(u64) field.offset, false));
        }
        llvm_offsets_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_uint, raw_data(llvm_elements), item_count);

        llvm_variant_elements: []LLVMValueRef = {llvm_types_slice, llvm_names_slice, llvm_offsets_slice};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Union: {
        type_union := cast(^Type_Union) type;

        item_count := len(type_union.variants);

        clear(&llvm_elements);
        for variant_type in type_union.variants {
          append(&llvm_elements, generator_type_get_type_info_pointer(generator, variant_type));
        }
        llvm_types_slice := generator_type_make_type_info_slice_type_info_pointer(generator, routine_context, raw_data(llvm_elements), item_count);
        
        llvm_variant_elements: []LLVMValueRef = {llvm_types_slice};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Routine: {
        type_routine := cast(^Type_Routine) type;

        llvm_routine_kind := LLVMConstInt(LLVMInt64Type(), type.kind == .Procedure ? 0 : 1, false);
        llvm_calling_convention := LLVMConstInt(LLVMInt64Type(), cast(u64) type_routine.calling_convention, false);

        item_count := len(type_routine.parameters);
        clear(&llvm_elements);
        for parameter in type_routine.parameters {
          append(&llvm_elements, generator_type_get_type_info_pointer(generator, parameter.type));
        }
        llvm_parameters_slice := generator_type_make_type_info_slice_type_info_pointer(generator, routine_context, raw_data(llvm_elements), item_count);

        llvm_return_type := generator_type_get_type_info_pointer(generator, type_routine.return_type);

        llvm_variant_elements: []LLVMValueRef = {llvm_routine_kind, llvm_calling_convention, llvm_parameters_slice, llvm_return_type};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }
      case .Interface: {
        item_count := len(type.methods);

        clear(&llvm_elements);
        for method in type.methods {
          append(&llvm_elements, generator_type_get_type_info_pointer(generator, method.type));
        }
        llvm_types_slice := generator_type_make_type_info_slice_type_info_pointer(generator, routine_context, raw_data(llvm_elements), item_count);

        clear(&llvm_elements);
        for method in type.methods {
          append(&llvm_elements, generator_string_to_llvm(generator, method.name, false));
        }
        llvm_names_slice := generator_type_make_type_info_slice(generator, routine_context, generator.storage.type_string, raw_data(llvm_elements), item_count);

        llvm_variant_elements: []LLVMValueRef = {llvm_types_slice, llvm_names_slice};
        llvm_variant = LLVMConstStruct(raw_data(llvm_variant_elements), cast(u32) len(llvm_variant_elements), false);
      }

      // Those do not have any additional data.
      case .Invalid, .Void, .Boolean, .Float, .Char, .Any, .Typeid: continue;

      case: assert(false);
    }

    // This gets us the appropriate pointer of the 'data' member inside the array.
    llvm_type_info_table_index := generator_type_get_llvm_type_info_table_index(generator, type);
    llvm_type_info_variant_pointer_indices: []LLVMValueRef = {
      LLVMConstInt(LLVMInt32Type(), 0, false),
      llvm_type_info_table_index,
      LLVMConstInt(LLVMInt32Type(), 5, false),
    };
    llvm_type_info_variant_pointer := LLVMBuildGEP2(
      generator.builder,
      generator.llvm_type_info_table_type,
      generator.llvm_type_info_table,
      raw_data(llvm_type_info_variant_pointer_indices),
      cast(u32) len(llvm_type_info_variant_pointer_indices),
      make_value_name(routine_context),
    );
    generator_emit_store_union_value(generator, routine_context, type_info_variant_type, type_info_type, llvm_type_info_variant_pointer, llvm_variant, nil);
  }
}

/**
* Get the 'Type_Info' type for a type kind.
*
* @param generator      The reference to the generator.
* @param type_info_kind The type info kind.
* @return The corresponding 'Type_Info' type.
*/
generator_type_get_type_info_type_for_kind :: proc(storage: ^Type_Storage, type_info_kind: Type_Info_Kind) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_info_type: ^Type;
  switch type_info_kind {
    case .Integer: return storage.cached_runtime_types.type_info_integer;
    case .Enumeration: return storage.cached_runtime_types.type_info_enumeration;
    case .String: return storage.cached_runtime_types.type_info_string;
    case .Pointer: return storage.cached_runtime_types.type_info_pointer;
    case .Relative_Pointer: return storage.cached_runtime_types.type_info_relative_pointer;
    case .Layout_Pointer: return storage.cached_runtime_types.type_info_layout_pointer;
    case .Dynamic_Pointer: return storage.cached_runtime_types.type_info_dynamic_pointer;
    case .Array: return storage.cached_runtime_types.type_info_array;
    case .Dynamic_Array: return storage.cached_runtime_types.type_info_dynamic_array;
    case .Slice: return storage.cached_runtime_types.type_info_slice;
    case .Map: return storage.cached_runtime_types.type_info_map;
    case .Tuple: return storage.cached_runtime_types.type_info_tuple;
    case .Struct: return storage.cached_runtime_types.type_info_struct;
    case .Union: return storage.cached_runtime_types.type_info_union;
    case .Routine: return storage.cached_runtime_types.type_info_routine;
    case .Interface: return storage.cached_runtime_types.type_info_interface;

    case .Invalid, .Void, .Boolean, .Float, .Char, .Typeid, .Any: fallthrough;
    case: return nil;
  }

  return nil;
}

/**
* Makes a type info slice with type info pointers as its elements.
*
* @param generator           The reference to the generator.
* @param routine_context     The context of the initialization routine.
* @param llvm_array_elements The array elements of the slice.
* @param item_count          The item count of the slice.
* @return The LLVM slice.
*/
generator_type_make_type_info_slice_type_info_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  llvm_array_elements: [^]LLVMValueRef,
  item_count: int,
) -> LLVMValueRef {
  type_info_pointer_type := type_storage_get_or_make_type_pointer(generator.storage, generator.storage.cached_runtime_types.type_info);
  return generator_type_make_type_info_slice(generator, routine_context, type_info_pointer_type, llvm_array_elements, item_count);
}

/**
* Makes a type info slice with a specified element type.
*
* @param generator           The reference to the generator.
* @param routine_context     The context of the initialization routine.
* @param element_type        The element type of the slice.
* @param llvm_array_elements The array elements of the slice.
* @param item_count          The item count of the slice.
* @return The LLVM slice.
*/
generator_type_make_type_info_slice :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  element_type: ^Type,
  llvm_array_elements: [^]LLVMValueRef,
  item_count: int,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_array_element_type := generator_type_to_llvm(generator, element_type);
  llvm_array_data := LLVMConstArray(llvm_array_element_type, llvm_array_elements, cast(u32) item_count);
  llvm_array_type := LLVMArrayType(llvm_array_element_type, cast(u32) item_count);
  llvm_array_global := LLVMAddGlobal(generator.module, llvm_array_type, "$type_info_array");
  LLVMSetGlobalConstant(llvm_array_global, true);
  LLVMSetInitializer(llvm_array_global, llvm_array_data);

  llvm_slice_type := generator_type_to_llvm(generator, type_storage_get_or_make_type_slice(generator.storage, LAYOUT_INFO_NONE, element_type));
  llvm_slice := LLVMConstNull(llvm_slice_type);
  llvm_slice = LLVMBuildInsertValue(generator.builder, llvm_slice, llvm_array_global, 0, make_value_name(routine_context));
  llvm_slice = LLVMBuildInsertValue(generator.builder, llvm_slice, LLVMConstInt(LLVMInt64Type(), cast(u64) item_count, false), 1, make_value_name(routine_context));

  return llvm_slice;
}

/**
* Gets the type info pointer for a given type.
*
* @param generator The reference to the generator.
* @param type      The type to get the type info pointer of.
* @return The LLVM type info pointer.
*/
generator_type_get_type_info_pointer :: proc(generator: ^Generator, type: ^Type) -> LLVMValueRef {
  llvm_type_info_table_index := generator_type_get_llvm_type_info_table_index(generator, type);
  return generator_emit_gep_field_dynamic(
    generator.builder,
    &generator.initializer_routine_context,
    generator.llvm_type_info_table_type,
    generator.llvm_type_info_table,
    llvm_type_info_table_index,
  );
}

/**
* Gets the type info pointer for a given type.
*
* @param generator The reference to the generator.
* @param type      The type to get the type info pointer of.
* @return The LLVM type info table index
*/
generator_type_get_llvm_type_info_table_index :: proc(generator: ^Generator, type: ^Type) -> LLVMValueRef {
  index, found := generator.type_info_table_indices[type.id];
  assert(found);
  return LLVMConstInt(LLVMInt32Type(), index, false);
}

/**
* Check whether or not a type should be skipped in initialization.
*
* @param type The type to check.
* @return True if the type should be skipped otherwise false.
*/
generator_type_should_skip_initialization :: proc(type: ^Type) -> bool {
  return .Uninstantiated_Generic in type.flags || type_is_generic(type);
}

/**
* Check whether or not a type should be skipped in the type table.
*
* @param type          The type to check.
* @param rttr_disabled Is RTTR disabled?
* @return True if the type should be skipped otherwise false.
*/
generator_type_should_skip_in_type_table :: proc(type: ^Type, rttr_disabled: bool) -> bool {
  return rttr_disabled || .Generate_RTTR not_in type.flags;
}

/**
* Check whether or not a type is passed as a pointer parameter.
*
* @param type The type to check.
* @return True if the type is passed as a pointer otherwise false.
*/
generator_type_is_passed_as_pointer :: proc(type: ^Type) -> bool {
  return .Passed_As_Pointer in type.flags;
}

/**
* Check whether or not a type requires loading its address instead of its value.
*
* @param resolved_type  The resolved type to check.
* @param overwrite_type The overwrite type to check.
* @return True if the type requires loading its address otherwise false.
*/
generator_type_requires_loading_address :: proc(resolved_type: ^Type, overwrite_type: ^Type) -> bool {
  return type_is_any(resolved_type) || .Requires_Address_Load in overwrite_type.flags;
}
