package nox

import "tracy"

/**
* Emit a load instruction.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param type            The type to load.
* @param llvm_pointer    The LLVM pointer to load from.
* @return The loaded LLVM value.
*/
generator_emit_load :: proc(generator: ^Generator, routine_context: ^Routine_Context, type: ^Type, llvm_pointer: LLVMValueRef) -> LLVMValueRef {
  llvm_type := generator_type_to_llvm(generator, type);
  return LLVMBuildLoad2(generator.builder, llvm_type, llvm_pointer, make_value_name(routine_context));
}

/**
* Emit a raw store instruction.
*
* @param generator    The reference to the generator.
* @param llvm_pointer The LLVM pointer to store into.
* @param llvm_value   The LLVM value to store.
*/
generator_emit_store_raw :: proc(generator: ^Generator, llvm_pointer: LLVMValueRef, llvm_value: LLVMValueRef) {
  LLVMBuildStore(generator.builder, llvm_value, llvm_pointer);
}

/**
* Emit a store instruction.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param type            The type to store.
* @param llvm_pointer    The LLVM pointer to store into.
* @param llvm_value      The LLVM value to store.
* @param expression      The expression of the value to store.
*/
generator_emit_store :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  needs_special_handling := type_is_union(type) || type_is_any(type) || type_is_dynamic_pointer(type) || type_contains_self_relative_pointer_in_value(type);
  if needs_special_handling {
    if type_is_union(type) {
      generator_emit_store_union(generator, routine_context, type, llvm_pointer, llvm_value, expression);
    } else if type_is_any(type) {
      generator_emit_store_any(generator, routine_context, llvm_pointer, llvm_value, expression);
    } else if type_is_dynamic_pointer(type) {
      generator_emit_store_dynamic_pointer(generator, routine_context, type, llvm_pointer, llvm_value, expression);
    } else {
      assert(type_contains_self_relative_pointer_in_value(type))

      if LLVMIsNull(llvm_value) {
        generator_emit_store_raw(generator, llvm_pointer, llvm_value);
      } else if type_is_self_relative_pointer(type) {
        generator_emit_store_self_relative_pointer(generator, routine_context, type, llvm_pointer, llvm_value, expression);
      } else {
        generator_emit_store_self_relative_pointers(generator, routine_context, type, llvm_pointer, llvm_value, expression);
      }
    }
  } else {
    generator_emit_store_raw(generator, llvm_pointer, llvm_value);
  }
}

/**
* Emit a store instruction for a union.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param union_type      The type of the union to store.
* @param llvm_pointer    The LLVM pointer to store into.
* @param llvm_value      The LLVM value to store.
* @param expression      The expression of the value to store.
*/
generator_emit_store_union :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  union_type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  variant_type: ^Type;
  if (expression != nil) {
    variant_type = generator_get_resolved_overwrite_type(generator, expression);
    if variant_type == nil {
      variant_type = generator_get_resolved_type(generator, expression);
    }
  }

  if expression == nil || union_type == variant_type {
    // Here we are simply coping the union but because 'llvm_value' is a pointer we have to load it first.
    llvm_value_union := llvm_value
    if (!LLVMIsNull(llvm_value_union)) {
      llvm_value_union = generator_emit_load(generator, routine_context, union_type, llvm_value);
    }
    generator_emit_store_raw(generator, llvm_pointer, llvm_value_union);
  } else {
    #partial switch variant_type.kind {
      case .Untyped_Boolean: variant_type = generator.storage.type_bool;
      case .Untyped_Integer: variant_type = generator.storage.type_int;
      case .Untyped_Float:   variant_type = generator.storage.type_f64;
      case .Untyped_Char:    variant_type = generator.storage.type_char;
      case .Untyped_String:  variant_type = generator.storage.type_string;
    }
    generator_emit_store_union_value(generator, routine_context, union_type, variant_type, llvm_pointer, llvm_value, expression);
  }
}

/**
* Emit a store instruction for a union value.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param union_type      The type of the union to store.
* @param variant_type    The type of the union variant that gets stored.
* @param llvm_pointer    The LLVM pointer to store into.
* @param llvm_value      The LLVM value to store.
* @param expression      The expression of the value to store.
*/
generator_emit_store_union_value :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  union_type: ^Type,
  variant_type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  assert(variant_type != nil);

  type_union := cast(^Type_Union) union_type;
  
  llvm_union_type := generator_type_to_llvm(generator, union_type);
  
  llvm_union_tag_pointer := generator_emit_gep_union_tag(generator.builder, routine_context, llvm_union_type, llvm_pointer);
  tag := 0;
  for variant, i in type_union.variants {
    if variant_type == variant {
      tag = i + 1;
    }
  }
  assert(tag != 0);
  generator_emit_store_raw(generator, llvm_union_tag_pointer, LLVMConstInt(LLVMInt64Type(), cast(u64) tag, false));

  llvm_union_value_pointer := generator_emit_gep_union_value(generator.builder, routine_context, llvm_union_type, llvm_pointer);
  generator_emit_store(generator, routine_context, variant_type, llvm_union_value_pointer, llvm_value, expression);
}

/**
* Emit a store instruction for an any.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param llvm_pointer    The LLVM pointer to store into.
* @param llvm_value      The LLVM value to store.
* @param expression      The expression of the value to store.
*/
generator_emit_store_any :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_value := llvm_value;

  // We have to explicitly handle the case where we are simply storing an any inside an any.
  // For example in the case of an empty initializer or a simply copy.
  expression_type := expression == nil ? nil : generator_get_resolved_type(generator, expression);
  if expression_type == nil || type_is_any(expression_type) {
    llvm_value_any := generator_emit_load(generator, routine_context, generator.storage.type_any, llvm_value);
    generator_emit_store_raw(generator, llvm_pointer, llvm_value_any);
    return;
  }

  // There are still some scenarios in which 'llvm_value' is still not a pointer like we need it to be,
  // so that we can properly store it as the data pointer of the any. These cases are:
  //   - Constants like: 1, 123.323, "Foo" and so on.
  //   - A parameter that is passed by value (pretty much all basic types).
  //   - Unary '&' expression are also an rvalue and therefore produce no proper pointer.
  //   - Query expressions (size_of, typeid_of, ...) which produce direct values.
  is_pointer := LLVMGetTypeKind(LLVMTypeOf(llvm_value)) == .Pointer;
  is_constant := LLVMIsConstant(llvm_value);
  is_value_parameter := !generator_type_is_passed_as_pointer(expression_type) && routine_context.last_resolved_symbol_was_parameter;
  is_unary := expression.kind == .Unary;
  is_unary_and := is_unary && (cast(^Expression_Unary) expression).operator == .And;
  is_query := expression.kind == .Query;
  if (!is_pointer || (is_value_parameter && !is_unary)) && (is_constant || is_value_parameter || is_unary_and || is_query) {
    llvm_temporary := generator_emit_temporary(generator, routine_context, expression_type, make_temp_name(routine_context));
    generator_emit_store(generator, routine_context, expression_type, llvm_temporary, llvm_value, expression);
    llvm_value = llvm_temporary;
  } 

  id := expression_type.id;
  // We remove untyped types as those do not have a proper entry in the type table.
  #partial switch expression_type.kind {
    case .Untyped_Boolean: id = generator.storage.type_bool.id;
    case .Untyped_Integer: id = generator.storage.type_int.id;
    case .Untyped_Float:   id = generator.storage.type_f64.id;
    case .Untyped_Char:    id = generator.storage.type_char.id;
    case .Untyped_String:  id = generator.storage.type_string.id;
    case .Untyped_Null:    id = generator.storage.type_rawptr.id;
  }

  llvm_any_data_pointer := generator_emit_gep_value_data(
    generator.builder,
    routine_context,
    generator_type_to_llvm(generator, generator.storage.type_any),
    llvm_pointer,
  );
  generator_emit_store_raw(generator, llvm_any_data_pointer, llvm_value);
  llvm_any_typeid_pointer := generator_emit_gep_value_length(
    generator.builder,
    routine_context,
    generator_type_to_llvm(generator, generator.storage.type_any),
    llvm_pointer,
  );
  generator_emit_store_raw(generator, llvm_any_typeid_pointer, LLVMConstInt(LLVMInt64Type(), cast(u64) id, false));
}

/**
* Emit a store instruction for a dynamic pointer.
*
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param dynamic_pointer_type The type of the dynamic pointer.
* @param llvm_pointer         The LLVM pointer to store into.
* @param llvm_value           The LLVM value to store.
* @param expression           The expression of the value to store.
*/
generator_emit_store_dynamic_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  dynamic_pointer_type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_type := expression == nil ? nil : generator_get_resolved_type(generator, expression);
  if expression_type == nil || type_is_dynamic_pointer(expression_type) || LLVMIsNull(llvm_value) {
    generator_emit_store_raw(generator, llvm_pointer, llvm_value);
  } else {
    assert(type_is_absolute_pointer(expression_type));

    llvm_dynamic_pointer_type := generator_type_to_llvm(generator, dynamic_pointer_type);
    llvm_dynamic_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_dynamic_pointer_type, llvm_pointer);
    generator_emit_store_raw(generator, llvm_dynamic_pointer_data_pointer, llvm_value);

    llvm_dynamic_pointer_vtable_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_dynamic_pointer_type, llvm_pointer);
    vtable_key := VTable_Key{dynamic_pointer_type.base, expression_type.base};
    llvm_vtable, found := generator.llvm_vtables[vtable_key];
    assert(found);
    generator_emit_store_raw(generator, llvm_dynamic_pointer_vtable_pointer, llvm_vtable);
  }
}

/**
* Emit a store instruction for a self-relative pointer.
*
* @param generator                  The reference to the generator.
* @param routine_context            The context of the routine.
* @param self_relative_pointer_type The type of the dynamic pointer.
* @param llvm_pointer               The LLVM pointer to store into.
* @param llvm_value                 The LLVM value to store.
* @param expression                 The expression of the value to store.
*/
generator_emit_store_self_relative_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  self_relative_pointer_type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_self_relative_offset: LLVMValueRef;
  if expression == nil {
    llvm_value_to_assign := llvm_value;
    llvm_value_to_assign = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, self_relative_pointer_type, llvm_value_to_assign);
    llvm_self_relative_offset = generator_emit_self_relative_pointer_offset(
      generator,
      routine_context,
      self_relative_pointer_type,
      llvm_pointer,
      llvm_value_to_assign,
    );
  } else {
    llvm_value_to_assign := llvm_value;
    right_type := generator_get_resolved_type(generator, expression);
    if type_is_self_relative_pointer(right_type) && expression.kind != .Unary {
      llvm_value_to_assign = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, self_relative_pointer_type, llvm_value_to_assign);
    } else if type_is_absolute_pointer(right_type) && !routine_context.last_resolved_symbol_was_parameter {
      llvm_value_to_assign = generator_emit_load(
        generator,
        routine_context,
        type_storage_get_or_make_type_pointer(generator.storage, right_type.base),
        llvm_value_to_assign,
      );
    }
    llvm_self_relative_offset = generator_emit_self_relative_pointer_offset(
      generator,
      routine_context,
      self_relative_pointer_type,
      llvm_pointer,
      llvm_value_to_assign,
    );  
  }

  generator_emit_store_raw(generator, llvm_pointer, llvm_self_relative_offset);
}

/**
* Emit a store instruction for types containing self-relative pointers.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param type               The type that contains the self-relative pointers.
* @param llvm_pointer       The LLVM pointer to store into.
* @param llvm_value_pointer The LLVM pointer to the value to store.
* @param expression         The expression of the value to store.
*/
generator_emit_store_self_relative_pointers :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value_pointer: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // First we do a general trivial copy for the type as we will only have special logic for the self-relative pointers.
  llvm_value_to_store := generator_emit_load(generator, routine_context, type, llvm_value_pointer);
  generator_emit_store_raw(generator, llvm_pointer, llvm_value_to_store);

  #partial switch type.kind {
    case .Array: generator_emit_store_self_relative_pointers_array(generator, routine_context, type, llvm_pointer, llvm_value_pointer, expression);
    case .Tuple: generator_emit_store_self_relative_pointers_tuple(generator, routine_context, type, llvm_pointer, llvm_value_pointer, expression);
    case .Struct: generator_emit_store_self_relative_pointers_struct(generator, routine_context, type, llvm_pointer, llvm_value_pointer, expression);
    case: assert(false); // Every other type can't contain a self-relative pointer as a value.
  }
}

/**
* Emit a store instruction for an array containing self-relative pointers.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param type               The array type that contains the self-relative pointers.
* @param llvm_pointer       The LLVM pointer to store into.
* @param llvm_value_pointer The LLVM pointer to the value to store.
* @param expression         The expression of the value to store.
*/
generator_emit_store_self_relative_pointers_array :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value_pointer: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_array := cast(^Type_Array) type;
  assert(!type_array.has_incomplete_elements);

  llvm_type := generator_type_to_llvm(generator, type);
  base_type := type.base;

  if type_is_self_relative_pointer(base_type) {
    for i := 0; i < type_array.number_of_elements; i += 1 {
      llvm_element_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_type, llvm_pointer, i);
      llvm_right_pointer_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_type, llvm_value_pointer, i);

      llvm_right_pointer := generator_emit_self_relative_pointer_to_absolute(generator, routine_context, base_type, llvm_right_pointer_pointer);
      llvm_self_relative_offset := generator_emit_self_relative_pointer_offset(generator, routine_context, base_type, llvm_element_pointer, llvm_right_pointer);
      generator_emit_store_raw(generator, llvm_element_pointer, llvm_self_relative_offset);
    }
  } else {
    generator_emit_store_self_relative_pointers(generator, routine_context, base_type, llvm_pointer, llvm_value_pointer, expression);
  }
}

/**
* Emit a store instruction for a tuple containing self-relative pointers.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param type               The tuple type that contains the self-relative pointers.
* @param llvm_pointer       The LLVM pointer to store into.
* @param llvm_value_pointer The LLVM pointer to the value to store.
* @param expression         The expression of the value to store.
*/
generator_emit_store_self_relative_pointers_tuple :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value_pointer: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_tuple := cast(^Type_Tuple) type;
  llvm_type := generator_type_to_llvm(generator, type);

  for element, element_index in type_tuple.elements {
    generator_emit_store_self_relative_pointers_struct_or_tuple_element(
      generator,
      routine_context,
      llvm_pointer,
      llvm_value_pointer,
      expression,
      llvm_type,
      element.type,
      element_index,
    );
  }
}

/**
* Emit a store instruction for a struct containing self-relative pointers.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param type               The struct type that contains the self-relative pointers.
* @param llvm_pointer       The LLVM pointer to store into.
* @param llvm_value_pointer The LLVM pointer to the value to store.
* @param expression         The expression of the value to store.
*/
generator_emit_store_self_relative_pointers_struct :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_pointer: LLVMValueRef,
  llvm_value_pointer: LLVMValueRef,
  expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_struct := cast(^Type_Struct) type;
  llvm_type := generator_type_to_llvm(generator, type);

  for field, field_index in type_struct.fields {
    generator_emit_store_self_relative_pointers_struct_or_tuple_element(
      generator,
      routine_context,
      llvm_pointer,
      llvm_value_pointer,
      expression,
      llvm_type,
      field.type,
      field_index,
    );
  }
}

/**
* Emit a store instruction for a struct or tuple field/element containing self-relative pointers.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param llvm_pointer       The LLVM pointer to store into.
* @param llvm_value_pointer The LLVM pointer to the value to store.
* @param expression         The expression of the value to store.
* @param llvm_type          The LLVM type of the field/element.
* @param field_type         The type of the field/element.
* @param field_index        The index of the field/element.
*/
generator_emit_store_self_relative_pointers_struct_or_tuple_element :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  llvm_pointer: LLVMValueRef,
  llvm_value_pointer: LLVMValueRef,
  expression: ^Expression,
  llvm_type: LLVMTypeRef,
  field_type: ^Type,
  field_index: int,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  if !type_contains_self_relative_pointer_in_value(field_type) {
    return;
  }

  llvm_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_type, llvm_pointer, field_index);
  llvm_right_pointer_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_type, llvm_value_pointer, field_index);

  if type_is_self_relative_pointer(field_type) {
    llvm_right_pointer := generator_emit_self_relative_pointer_to_absolute(generator, routine_context, field_type, llvm_right_pointer_pointer);
    llvm_self_relative_offset := generator_emit_self_relative_pointer_offset(generator, routine_context, field_type, llvm_field_pointer, llvm_right_pointer);
    LLVMBuildStore(generator.builder, llvm_self_relative_offset, llvm_field_pointer);
  } else {
    generator_emit_store_self_relative_pointers(generator, routine_context, field_type, llvm_field_pointer, llvm_value_pointer, expression);
  }
}

/**
* Emit a possible dereference for a pointer.
*
* @param generator             The reference to the generator.
* @param routine_context       The context of the routine.
* @param expression            The expression of the pointer that possibly needs to be dereferenced.
* @param llvm_possible_pointer The LLVM pointer that possibly needs to be dereferenced.
* @return The dereferenced pointer.
*/
generator_emit_possible_pointer_dereference :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression: ^Expression,
  llvm_possible_pointer: LLVMValueRef,
) -> LLVMValueRef {
  type := generator_get_resolved_type(generator, expression);
  needs_to_dereference_pointer := type_is_absolute_pointer(type) || type_is_self_relative_pointer(type);
  if needs_to_dereference_pointer && !routine_context.last_resolved_symbol_was_parameter && expression.kind != .Unary {
    if type_is_self_relative_pointer(type) {
      return generator_emit_self_relative_pointer_to_absolute(generator, routine_context, type, llvm_possible_pointer);
    } else {
      return generator_emit_load(generator, routine_context, type, llvm_possible_pointer);
    }
  }
  return llvm_possible_pointer;
}

/**
* Emit a local on the stack.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param position        The position of the local.
* @param type            The type of the local.
* @param name            The name of the local.
* @return The local that got emitted.
*/
generator_emit_local :: proc(generator: ^Generator, routine_context: ^Routine_Context, position: Source_Position, type: ^Type, name: string) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);

  previous_block := generator_enter_llvm_block(generator, routine_context, routine_context.declaration_block);
  llvm_local := LLVMBuildAlloca(generator.builder, llvm_type, make_variable_name(routine_context, name));
  generator_enter_llvm_block(generator, routine_context, previous_block);

  generator_emit_store_raw(generator, llvm_local, generator_default_initializer_value(generator, type));

  // We purposefully overwrite potential previous entries.
  generator_add_local_symbol(name, llvm_local, llvm_type, type, false, routine_context.local_scope);
  generator_debug_emit_local_variable(generator, routine_context, position, type, name, llvm_local);

  return llvm_local;
}

/**
* Emit a temporaray on the stack.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param type            The type of the temporary.
* @param name            The name of the temporary.
* @return The temporary that got emitted.
*/
generator_emit_temporary :: proc(generator: ^Generator, routine_context: ^Routine_Context, type: ^Type, name: cstring) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);

  previous_block := generator_enter_llvm_block(generator, routine_context, routine_context.declaration_block);
  llvm_temorary := LLVMBuildAlloca(generator.builder, llvm_type, name);
  generator_enter_llvm_block(generator, routine_context, previous_block);

  generator_emit_store_raw(generator, llvm_temorary, generator_default_initializer_value(generator, type));

  return llvm_temorary;
}

/**
* Emit a temporaray on the stack and store a raw value in it.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param type            The type of the temporary.
* @param llvm_value      The LLVM value to store in the temporary.
* @param name            The name of the temporary.
* @return The temporary that got emitted.
*/
generator_emit_temporary_and_store_raw :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  type: ^Type,
  llvm_value: LLVMValueRef,
  name: cstring,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);

  previous_block := generator_enter_llvm_block(generator, routine_context, routine_context.declaration_block);
  llvm_temorary := LLVMBuildAlloca(generator.builder, llvm_type, name);
  generator_enter_llvm_block(generator, routine_context, previous_block);

  generator_emit_store_raw(generator, llvm_temorary, llvm_value);

  return llvm_temorary;
}

/**
* Emit a temporaray global.
*
* @param generator The reference to the generator.
* @param type      The type of the temporary.
* @return The temporary global.
*/
generator_emit_temporary_global :: proc(generator: ^Generator, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);
  llvm_temporary_global := LLVMAddGlobal(generator.module, llvm_type, "global");
  LLVMSetInitializer(llvm_temporary_global, generator_default_initializer_value(generator, type));

  return llvm_temporary_global;
}

/**
* Emit a temporaray routine.
*
* @param generator        The reference to the generator.
* @param name             The name of the routine.
* @param llvm_return_type The LLVM return type of the routine.
* @param llvm_parameters  The LLVM parameters of the routine.
* @return The temporary routine.
*/
generator_emit_temporary_routine :: proc(
  generator: ^Generator,
  name: cstring,
  llvm_return_type: LLVMTypeRef,
  llvm_parameters: []LLVMTypeRef,
) -> (LLVMValueRef, LLVMTypeRef) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_routine_type := LLVMFunctionType(llvm_return_type, raw_data(llvm_parameters), cast(u32) len(llvm_parameters), false);
  llvm_routine := LLVMAddFunction(generator.module, name, llvm_routine_type);
  return llvm_routine, llvm_routine_type;
}

/**
* Emit the deferred statements in the current local scope.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_deferred_statements_local :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_emit_deferred_statements_in_scope(generator, routine_context, routine_context.local_scope);
}

/**
* Emit the deferred statements until a control scope is reached.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_deferred_statements_until_control :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We emit all deferred statements until we reach the first control block.
  // This ensures that 'continue' and 'break' don't emit deferred statements past their corresponding control statement.

  scope := routine_context.local_scope;
  for scope != nil {
    generator_emit_deferred_statements_in_scope(generator, routine_context, scope);
    if scope.kind == .Control_Block {
      return;
    }
    scope = scope.parent;
  }
}

/**
* Emit all deferred statements starting from the current local scope.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_deferred_statements_all :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  scope := routine_context.local_scope;
  for scope != nil {
    generator_emit_deferred_statements_in_scope(generator, routine_context, scope);
    scope = scope.parent;
  }
}

/**
* Emit the deferred statements of a given scope.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param scope           The scope to emit the deferred statements of.
*/
generator_emit_deferred_statements_in_scope :: proc(generator: ^Generator, routine_context: ^Routine_Context, scope: ^Routine_Local_Scope) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  for i := len(scope.deferred_statements) - 1; i >= 0; i -= 1 {
    deferred_statement := scope.deferred_statements[i];
    generator_emit_statement(generator, routine_context, deferred_statement, false);
  }
}

/**
* Emit a GEP instruction for a field.
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @param field_index     The index of the field to use in the GEP instruction.
* @return The result of the GEP.
*/
generator_emit_gep_field :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
  field_index: int,
) -> LLVMValueRef {
  return generator_emit_gep_field_dynamic(builder, routine_context, llvm_type, llvm_pointer, LLVMConstInt(LLVMInt32Type(), cast(u64) field_index, false));
}

/**
* Emit a GEP instruction for a dynamic field.
*
* @param builder          The LLVM builder to use.
* @param routine_context  The context of the routine.
* @param llvm_type        The LLVM type the GEP instruction is based on.
* @param llvm_pointer     The LLVM pointer the GEP instruction is based on.
* @param llvm_field_index The LLVM index of the field to use in the GEP instruction.
* @return The result of the GEP.
*/
generator_emit_gep_field_dynamic :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
  llvm_field_index: LLVMValueRef,
) -> LLVMValueRef {
  llvm_field_indices: []LLVMValueRef = {LLVMConstInt(LLVMInt32Type(), 0, false), llvm_field_index};
  return LLVMBuildGEP2(
    builder,
    llvm_type,
    llvm_pointer,
    raw_data(llvm_field_indices),
    cast(u32) len(llvm_field_indices),
    make_value_name(routine_context),
  );
}

/**
* Emit a GEP instruction for a member.
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @param member          The member to use in the GEP instruction.
* @return The result of the GEP.
*/
generator_emit_gep_member :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
  member: Resolved_Member,
) -> LLVMValueRef {
  if len(member.indices) == 0 {
    return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, member.index);
  } else {
    llvm_field_indices := make_dynamic_array_len_cap([dynamic]LLVMValueRef, 0, len(member.indices) + 1, context.temp_allocator);
    append(&llvm_field_indices, LLVMConstInt(LLVMInt32Type(), 0, false));
    for index in member.indices {
      append(&llvm_field_indices, LLVMConstInt(LLVMInt32Type(), cast(u64) index, false));
    }
    return LLVMBuildGEP2(
      builder,
      llvm_type,
      llvm_pointer,
      raw_data(llvm_field_indices),
      cast(u32) len(llvm_field_indices),
      make_value_name(routine_context),
    );
  }
}

/**
* Emit a GEP instruction for a pointer.
*
* @param builder            The LLVM builder to use.
* @param routine_context    The context of the routine.
* @param llvm_type          The LLVM type the GEP instruction is based on.
* @param llvm_pointer       The LLVM pointer the GEP instruction is based on.
* @param llvm_element_index The LLVM element index to use in the GEP instruction.
* @return The result of the GEP.
*/
generator_emit_gep_pointer  :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
  llvm_element_index: LLVMValueRef,
) -> LLVMValueRef {
  llvm_elemenet_indices: []LLVMValueRef = {llvm_element_index};
  return LLVMBuildGEP2(
    builder,
    llvm_type,
    llvm_pointer,
    raw_data(llvm_elemenet_indices),
    cast(u32) len(llvm_elemenet_indices),
    make_value_name(routine_context),
  );
}

/**
* Emit a GEP instruction for a union tag.
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @return The result of the GEP.
*/
generator_emit_gep_union_tag :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
) -> LLVMValueRef {
  return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, 0);
}

/**
* Emit a GEP instruction for a union value.
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @return The result of the GEP.
*/
generator_emit_gep_union_value :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
) -> LLVMValueRef {
  return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, 1);
}

/**
* Emit a GEP instruction for the data of a value (first element at index 0).
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @return The result of the GEP.
*/
generator_emit_gep_value_data :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
) -> LLVMValueRef {
  return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, 0);
}

/**
* Emit a GEP instruction for the length of a value (second element at index 1).
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @return The result of the GEP.
*/
generator_emit_gep_value_length :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
) -> LLVMValueRef {
  return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, 1);
}

/**
* Emit a GEP instruction for the capacity of a value (second element at index 2).
*
* @param builder         The LLVM builder to use.
* @param routine_context The context of the routine.
* @param llvm_type       The LLVM type the GEP instruction is based on.
* @param llvm_pointer    The LLVM pointer the GEP instruction is based on.
* @return The result of the GEP.
*/
generator_emit_gep_value_capacity :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  llvm_type: LLVMTypeRef,
  llvm_pointer: LLVMValueRef,
) -> LLVMValueRef {
  return generator_emit_gep_field(builder, routine_context, llvm_type, llvm_pointer, 2);
}

/**
* Emit a GEP instruction for the length of a layout collection.
*
* @param builder                     The LLVM builder to use.
* @param routine_context             The context of the routine.
* @param layout_collection_type      The type of the layout collection.
* @param llvm_layout_collection_type The LLVM type of the layout collection.
* @param llvm_layout_collection      The LLVM layout collection.
* @return The result of the GEP.
*/
generator_emit_gep_layout_collection_length :: proc(
  builder: LLVMBuilderRef,
  routine_context: ^Routine_Context,
  layout_collection_type: ^Type,
  llvm_layout_collection_type: LLVMTypeRef,
  llvm_layout_collection: LLVMValueRef,
) -> LLVMValueRef {
  layout_struct := cast(^Type_Struct) layout_collection_type.base;
  assert(type_is_struct(layout_struct));

  // Depending on whether or not we have a slice or dynamic array, the 'length' field is at a different place inside the layout struct.
  field_offset_from_end := type_is_slice(layout_collection_type) ? 1 : 3;
  length_index := len(layout_struct.fields) - field_offset_from_end;
  return generator_emit_gep_field(builder, routine_context, llvm_layout_collection_type, llvm_layout_collection, length_index);
}

/**
* Emit the field pointer for an AoSoA element.
*
* @param generator             The reference to the generator.
* @param routine_context       The context of the routine.
* @param aosoa_collection_type The type of the AoSOA collection.
* @param llvm_data_pointer     The LLVM data pointer of the AoSOA collection.
* @param llvm_field_type       The LLVM type of the field.
* @param llvm_index            The LLVM index into the AoSoA collection.
* @param member                The member corresponding to the field.
* @return The field pointer for the AoSoA element.
*/
generator_emit_aosoa_element_field_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  aosoa_collection_type: ^Type,
  llvm_data_pointer: LLVMValueRef,
  llvm_field_type: LLVMTypeRef,
  llvm_index: LLVMValueRef,
  member: Resolved_Member,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_struct := cast(^Type_Struct) aosoa_collection_type.base;
  aosoa_collection_array_base := type_struct.fields[0].type.base;
  items_in_aosoa_chunk := type_struct.layout_info.items_in_aosoa_chunk
  llvm_aosoa_collection_array_base := generator_type_to_llvm(generator, aosoa_collection_array_base);

  // We always do the calculation with 64-bit integers.
  llvm_index := LLVMBuildSExt(generator.builder, llvm_index, LLVMInt64Type(), make_value_name(routine_context));
  llvm_items_in_aosoa_chunk := LLVMConstInt(LLVMInt64Type(), cast(u64) items_in_aosoa_chunk, false);

  llvm_outer_index := LLVMBuildUDiv(generator.builder, llvm_index, llvm_items_in_aosoa_chunk, make_value_name(routine_context));
  llvm_outer_pointer := generator_emit_gep_pointer(generator.builder, routine_context, llvm_aosoa_collection_array_base, llvm_data_pointer, llvm_outer_index);
  llvm_outer_field_pointer := generator_emit_gep_member(generator.builder, routine_context, llvm_aosoa_collection_array_base, llvm_outer_pointer, member);
  llvm_element_index := LLVMBuildURem(generator.builder, llvm_index, llvm_items_in_aosoa_chunk, make_value_name(routine_context));
  return generator_emit_gep_pointer(generator.builder, routine_context, llvm_field_type, llvm_outer_field_pointer, llvm_element_index);
}

/**
* Emit the offset for a self-relative pointer.
*
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param pointer_type       The type of the self-relative pointer.
* @param llvm_self_pointer  The LLVM pointer to the self-relative pointer.
* @param llvm_right_pointer The LLVM pointer to the right to base the offset of.
* @return The offset for the self-relative pointer.
*/
generator_emit_self_relative_pointer_offset :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  pointer_type: ^Type,
  llvm_self_pointer: LLVMValueRef,
  llvm_right_pointer: LLVMValueRef,
) -> LLVMValueRef {
  if LLVMIsNull(llvm_right_pointer) {
    return llvm_right_pointer;
  }
  
  relative_base := (cast(^Type_Relative_Pointer) pointer_type).relative_base;

  llvm_self_int_pointer := LLVMBuildPtrToInt(generator.builder, llvm_self_pointer, LLVMInt64Type(), make_value_name(routine_context));
  llvm_right_int_pointer := LLVMBuildPtrToInt(generator.builder, llvm_right_pointer, LLVMInt64Type(), make_value_name(routine_context));
  llvm_self_relative_offset := LLVMBuildSub(generator.builder, llvm_right_int_pointer, llvm_self_int_pointer, make_value_name(routine_context));
  llvm_self_relative_offset = generator_emit_self_relative_pointer_offset_truncation(generator, routine_context, relative_base, llvm_self_relative_offset);
  llvm_pointer_is_zero := LLVMBuildICmp(generator.builder, .EQ, llvm_right_int_pointer, LLVMConstInt(LLVMInt64Type(), 0, false), make_value_name(routine_context));
  llvm_zero_value := generator_default_initializer_value(generator, relative_base);
  return LLVMBuildSelect(generator.builder, llvm_pointer_is_zero, llvm_zero_value, llvm_self_relative_offset, make_value_name(routine_context));
}

/**
* Emit the offset truncation for a self-relative pointer.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param relative_base   The relative base of the self-relative pointer.
* @param llvm_offset     The LLVM offset to truncate.
* @return The truncated offset.
*/
generator_emit_self_relative_pointer_offset_truncation :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  relative_base: ^Type,
  llvm_offset: LLVMValueRef,
) -> LLVMValueRef {
  needs_trunc := generator_relative_pointer_base_needs_conversion(relative_base);
  if needs_trunc {
    return LLVMBuildTrunc(generator.builder, llvm_offset, generator_type_to_llvm(generator, relative_base), make_value_name(routine_context));
  } else {
    return llvm_offset;
  }
}

/**
* Emit the conversion from a self-relative pointer to an absolute pointer.
*
* @param generator             The reference to the generator.
* @param routine_context       The context of the routine.
* @param pointer_type          The type of the self-relative pointer.
* @param llvm_relative_pointer The LLVM pointer to the self-relative pointer.
* @return The truncated offset.
*/
generator_emit_self_relative_pointer_to_absolute :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  pointer_type: ^Type,
  llvm_relative_pointer: LLVMValueRef,
) -> LLVMValueRef {
  if LLVMIsNull(llvm_relative_pointer) {
    return llvm_relative_pointer;
  }
  
  relative_base := (cast(^Type_Relative_Pointer) pointer_type).relative_base;

  llvm_self_relative_offset := generator_emit_load(generator, routine_context, relative_base, llvm_relative_pointer);
  llvm_self_relative_offset = generator_emit_relative_pointer_offset_extension(generator, routine_context, relative_base, llvm_self_relative_offset);
  llvm_self_relative_int_pointer := LLVMBuildPtrToInt(generator.builder, llvm_relative_pointer, LLVMInt64Type(), make_value_name(routine_context));
  llvm_computed_int_pointer := LLVMBuildAdd(generator.builder, llvm_self_relative_int_pointer, llvm_self_relative_offset, make_value_name(routine_context));
  llvm_pointer_type := generator_type_to_llvm(generator, type_storage_get_or_make_type_pointer(generator.storage, pointer_type.base)); 
  llvm_computed_pointer := LLVMBuildIntToPtr(generator.builder, llvm_computed_int_pointer, llvm_pointer_type, make_value_name(routine_context));
  llvm_zero_value := generator_default_initializer_value(generator, generator.storage.type_i64);
  llvm_offset_is_zero := LLVMBuildICmp(generator.builder, .EQ, llvm_self_relative_offset, llvm_zero_value, make_value_name(routine_context));
  return LLVMBuildSelect(generator.builder, llvm_offset_is_zero, LLVMConstNull(llvm_pointer_type), llvm_computed_pointer, make_value_name(routine_context));
}

/**
* Emit the offset extension for a self-relative pointer.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param relative_base   The relative base of the self-relative pointer.
* @param llvm_offset     The LLVM offset to extend.
* @return The extended offset.
*/
generator_emit_relative_pointer_offset_extension :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  relative_base: ^Type,
  llvm_offset: LLVMValueRef,
) -> LLVMValueRef {
  needs_extend := generator_relative_pointer_base_needs_conversion(relative_base);
  if needs_extend {
    return LLVMBuildSExt(generator.builder, llvm_offset, LLVMInt64Type(), make_value_name(routine_context));
  } else {
    return llvm_offset;
  }
}

/**
* Emit the access for a member of an element in a layout collection.
*
* @param generator              The reference to the generator.
* @param routine_context        The context of the routine.
* @param layout_collection_type The type of the layout collection.
* @param field_type             The type of the field to access.
* @param llvm_layout_collection The LLVM layout collection.
* @param llvm_index             The LLVM index of the element in the layout collection.
* @return The accessed member.
*/
generator_emit_layout_member_access :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  layout_collection_type: ^Type,
  resolved_member: Resolved_Member,
  field_type: ^Type,
  llvm_layout_collection: LLVMValueRef,
  llvm_index: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_data_pointer: LLVMValueRef;
  if type_is_aosoa(layout_collection_type) {
    llvm_data_pointer = llvm_layout_collection;
  } else {
    // This gets us the pointer to the correct field array inside the layout struct.
    llvm_layout_collection_type := generator_type_to_llvm(generator, layout_collection_type);
    llvm_data_pointer = generator_emit_gep_member(generator.builder, routine_context, llvm_layout_collection_type, llvm_layout_collection, resolved_member);
  }
  // If we are a dynamic array or a slice we have to load the field data pointer first.
  if !type_is_array(layout_collection_type) {
    llvm_data_pointer = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_data_pointer);
  }
  
  // Now we get the pointer to the actual element inside the array (based on the provided index).
  llvm_field_type := generator_type_to_llvm(generator, field_type);
  llvm_layout_field_pointer: LLVMValueRef;
  if type_is_aosoa(layout_collection_type) {
    llvm_layout_field_pointer = generator_emit_aosoa_element_field_pointer(
      generator,
      routine_context,
      layout_collection_type,
      llvm_data_pointer,
      llvm_field_type,
      llvm_index,
      resolved_member,
    );
  } else {
    llvm_layout_field_pointer = generator_emit_gep_pointer(generator.builder, routine_context, llvm_field_type, llvm_data_pointer, llvm_index);
  }

  if routine_context.current_loading_mode == .Load_Address {
    return llvm_layout_field_pointer;
  } else {
    return generator_emit_load(generator, routine_context, field_type, llvm_layout_field_pointer);
  }
}

/**
* Emit the access for a complete element in a layout collection.
*
* @param generator              The reference to the generator.
* @param routine_context        The context of the routine.
* @param layout_collection_type The type of the layout collection.
* @param llvm_layout_collection The LLVM layout collection.
* @param llvm_index             The LLVM index of the element in the layout collection.
* @return The accessed element.
*/
generator_emit_layout_index_access :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  layout_collection_type: ^Type,
  llvm_layout_collection: LLVMValueRef,
  llvm_index: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Here we handle the SoA/AoSoA scenerio where we are trying to read a single complete element at a specific index.
  // We of course don't have a complete single element in an SoA/AoSoA collection which we can return.
  // Instead we create a temporary and fill it with the corresponding data by copying over the fields one by one.
  // When the index expression is being used as an lvalue in an assignment that is handled explicitly when generating an assignment statement.
  // This means that the 'Load_Adress' loading mode returns here the address of the temporary that is being created.

  element_type := layout_collection_type.base.base;
  assert(type_is_struct(element_type));
  type_struct := cast(^Type_Struct) element_type;

  llvm_element_type := generator_type_to_llvm(generator, element_type);
  llvm_collection_type := generator_type_to_llvm(generator, layout_collection_type);

  // This is the temporary that we fill with the single field elements and return.
  llvm_element := generator_emit_temporary(generator, routine_context, element_type, make_temp_name(routine_context));

  is_aosoa := type_is_aosoa(layout_collection_type);

  for field, i in type_struct.fields {
    llvm_data_pointer: LLVMValueRef;
    if is_aosoa {
      llvm_data_pointer = llvm_layout_collection;
    } else {
      llvm_data_pointer = generator_emit_gep_field(generator.builder, routine_context, llvm_collection_type, llvm_layout_collection, i);
    }
    
    // If we are a dynamic array or a slice we have to load the field data pointer first.
    if !type_is_array(layout_collection_type) {
      llvm_data_pointer = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_data_pointer);
    }
    llvm_field_type := generator_type_to_llvm(generator, field.type);

    llvm_layout_element_pointer: LLVMValueRef;
    if is_aosoa {
      llvm_layout_element_pointer = generator_emit_aosoa_element_field_pointer(
        generator,
        routine_context,
        layout_collection_type,
        llvm_data_pointer,
        llvm_field_type,
        llvm_index,
        {i, {}, field.type},
      );
    } else {
      llvm_layout_element_pointer = generator_emit_gep_pointer(generator.builder, routine_context, llvm_field_type, llvm_data_pointer, llvm_index);
    }

    llvm_field_value: LLVMValueRef;
    if generator_type_requires_loading_address(field.type, field.type) {
      llvm_field_value = llvm_layout_element_pointer;
    } else {
      llvm_field_value = generator_emit_load(generator, routine_context, field.type, llvm_layout_element_pointer);
    }
    
    llvm_element_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_element_type, llvm_element, i);
    generator_emit_store(generator, routine_context, field.type, llvm_element_field_pointer, llvm_field_value, nil);
  }

  if routine_context.current_loading_mode == .Load_Address {
    return llvm_element;
  } else {
    return generator_emit_load(generator, routine_context, element_type, llvm_element);
  }
}

/**
* Emit the default context initialization.
*
* @param generator    The reference to the generator.
* @param llvm_context The LLVM context to initialize.
*/
generator_emit_default_context_initialization :: proc(generator: ^Generator, llvm_context: LLVMValueRef) {
  llvm_arguments: []LLVMValueRef = {llvm_context, llvm_context};
  generator_emit_call_to_builtin_routine(generator, .Runtime_Init_Default_Context, llvm_arguments, "");
}

/**
* Emit a call to a built-in routine.
*
* @param generator       The reference to the generator.
* @param builtin_routine The built-in routine to call.
* @param llvm_arguments  The LLVM arguments to pass along.
* @param name            The name of the LLVM return value.
* @return The return value.
*/
generator_emit_call_to_builtin_routine :: proc(
  generator: ^Generator,
  builtin_routine: Builtin_Routine,
  llvm_arguments: []LLVMValueRef,
  name: cstring,
) -> LLVMValueRef {
  llvm_builtin_routine := generator.llvm_builtin_routines[builtin_routine];
  return LLVMBuildCall2(
    generator.builder,
    generator_type_to_llvm_without_routine_promotion(generator, llvm_builtin_routine.symbol.type),
    llvm_builtin_routine.llvm_tuple.llvm_symbol,
    raw_data(llvm_arguments),
    cast(u32) len(llvm_arguments),
    name,
  );
}

/**
* Emit a boolean expression.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The boolean expression.
* @param type            The type of the boolean expression.
* @return The result of the boolean expression.
*/
generator_emit_boolean_truncation :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  llvm_boolean := generator_emit_expression(generator, routine_context, expression, type);
  return LLVMBuildTrunc(generator.builder, llvm_boolean, LLVMInt1Type(), make_value_name(routine_context));
}

/**
* Emit a typeid expression for an any.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression of the typeid expression.
* @return The typeid of the any.
*/
generator_emit_typeid_expression_any :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_any_pointer := generator_emit_expression(generator, routine_context, expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  llvm_any_type := generator_type_to_llvm(generator, generator.storage.type_any);
  llvm_any_typeid_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_any_type, llvm_any_pointer);
  return generator_emit_load(generator, routine_context, generator.storage.type_typeid, llvm_any_typeid_pointer);
}

/**
* Emit a typeid expression for a union.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression of the typeid expression.
* @param union_type      The type of the union.
* @return The typeid of the union.
*/
generator_emit_typeid_expression_union :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, union_type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_union_pointer := generator_emit_expression(generator, routine_context, expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  llvm_union_tag_pointer := generator_emit_gep_union_tag(generator.builder, routine_context, generator_type_to_llvm(generator, union_type), llvm_union_pointer);
  llvm_union_tag := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_union_tag_pointer);

  type_union := cast(^Type_Union) union_type;
  union_variant_count := len(type_union.variants);

  llvm_switch_default := generator_append_block(routine_context);
  llvm_switch_blocks := make_dynamic_array_len_cap([dynamic]LLVMBasicBlockRef, 0, union_variant_count, context.temp_allocator);
  for variant in type_union.variants {
    append(&llvm_switch_blocks, generator_append_block(routine_context));
  }

  llvm_after_block := generator_append_block(routine_context);
  llvm_switch := LLVMBuildSwitch(generator.builder, llvm_union_tag, llvm_switch_default, cast(u32) union_variant_count);
  for variant, i in type_union.variants {
    tag := i + 1;
    LLVMAddCase(llvm_switch, LLVMConstInt(LLVMInt64Type(), cast(u64) tag, false), llvm_switch_blocks[i]);
  }

  previous_block := generator_enter_llvm_block(generator, routine_context, llvm_switch_default);
  LLVMBuildBr(generator.builder, llvm_after_block);
  generator_leave_llvm_block(routine_context, previous_block);
  for variant, i in type_union.variants {
    previous_block := generator_enter_llvm_block(generator, routine_context, llvm_switch_blocks[i]);
    LLVMBuildBr(generator.builder, llvm_after_block);
    generator_leave_llvm_block(routine_context, previous_block);
  } 

  // We use a PHI node to determine where in the switch we came from and what value we need.
  generator_enter_llvm_block(generator, routine_context, llvm_after_block);
  llvm_phi_value := LLVMBuildPhi(generator.builder, generator_type_to_llvm(generator, generator.storage.type_typeid), make_value_name(routine_context));
  llvm_phi_blocks := make_dynamic_array_len_cap([dynamic]LLVMBasicBlockRef, 0, union_variant_count + 1, context.temp_allocator);
  append(&llvm_phi_blocks, llvm_switch_default);
  for variant, i in type_union.variants {
    append(&llvm_phi_blocks, llvm_switch_blocks[i]);
  }
  llvm_phi_values := make_dynamic_array_len_cap([dynamic]LLVMValueRef, 0, union_variant_count + 1, context.temp_allocator);
  append(&llvm_phi_values, LLVMConstInt(LLVMInt64Type(), 0, false));
  for variant, i in type_union.variants {
    append(&llvm_phi_values, LLVMConstInt(LLVMInt64Type(), cast(u64) variant.id, false));
  }
  LLVMAddIncoming(llvm_phi_value, raw_data(llvm_phi_values), raw_data(llvm_phi_blocks), cast(u32) union_variant_count + 1);
  return llvm_phi_value;
}

/**
* Emit the conversion for the operands of a shift operation.
*
* @param generator        The reference to the generator.
* @param routine_context  The context of the routine.
* @param left_type        The type of the left shift operand.
* @param right_type       The type of the right shift operand.
* @param llvm_right_value The value of the right shift operand.
* @return The converted right value.
*/
generator_emit_expression_binary_shit_type_conversion :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  left_type: ^Type,
  right_type: ^Type,
  llvm_right_value: LLVMValueRef,
) -> LLVMValueRef {
  llvm_left_type := generator_type_to_llvm(generator, left_type);
  name := make_value_name(routine_context);

  #partial switch (left_type.kind) {
    case .I8, .U8: {
      #partial switch (right_type.kind) {
        case .U8, .U16, .U32, .U64, .UInt, .Untyped_Integer: {
          return LLVMBuildTrunc(generator.builder, llvm_right_value, llvm_left_type, name);
        }
      }
    }
    case .I16, .U16: {
      #partial switch (right_type.kind) {
        case .U8: return LLVMBuildZExt(generator.builder, llvm_right_value, llvm_left_type, name);
        case .U16, .U32, .U64, .UInt, .Untyped_Integer: {
          return LLVMBuildTrunc(generator.builder, llvm_right_value, llvm_left_type, name);
        }
      }
    }
    case .I32, .U32: {
      #partial switch (right_type.kind) {
        case .U8, .U16: return LLVMBuildZExt(generator.builder, llvm_right_value, llvm_left_type, name);
        case .U32, .U64, .UInt, .Untyped_Integer: {
          return LLVMBuildTrunc(generator.builder, llvm_right_value, llvm_left_type, name);
        }
      }
    }
    case: {
      #partial switch (right_type.kind) {
        case .U8, .U16, .U32: return LLVMBuildZExt(generator.builder, llvm_right_value, llvm_left_type, name);
        case .U64, .UInt, .Untyped_Integer: {
          return LLVMBuildTrunc(generator.builder, llvm_right_value, llvm_left_type, name);
        }
      }
    }
  }
  
  assert(false);
  return nil;
}

/**
* Emit a cast expression.
*
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The cast expression to emit.
* @return The result of the expression.
*/
generator_emit_cast_expression :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  assert(expression.kind == .Cast);

  expression_cast := cast(^Expression_Cast) expression;

  destination_type := generator_get_resolved_type(generator, expression_cast.type);
  llvm_destination_type := generator_type_to_llvm(generator, destination_type);
  source_type := generator_get_resolved_type(generator, expression_cast.expression);
  llvm_source_type := generator_type_to_llvm(generator, source_type);

  needs_to_load_address := generator_type_requires_loading_address(source_type, source_type);
  previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  llvm_source_value := generator_emit_expression(generator, routine_context, expression_cast.expression, source_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  destination_is_offset_relative_pointer := type_is_offset_relative_pointer(destination_type);
  relative_base: ^Type;
  if destination_is_offset_relative_pointer {
    relative_base = (cast(^Type_Relative_Pointer) destination_type).relative_base;
    destination_type = relative_base;
  }

  // If the types are already the same, we don't need to do anything.
  if (source_type == destination_type) || (llvm_source_type == llvm_destination_type) {
    return llvm_source_value;
  }

  name := make_value_name(routine_context);

  if type_is_scalar(source_type) && type_is_scalar(destination_type) {
    // Convert enums to their base type.
    if type_is_enumeration(source_type) {
      source_type = source_type.base;
    }
    if type_is_enumeration(destination_type) {
      destination_type = destination_type.base;
    }

    // For convenience we treat booleans as their unsigned integer counterpart.

    #partial switch source_type.kind {
      case .Bool, .Untyped_Boolean: {
        return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
      }
      case .I8: {
        #partial switch destination_type.kind {
          case .Bool:                                              return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B8, .B16, .B32, .B64, .I8, .I16, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U8, .U16, .U32, .U64, .UInt:                return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                                         return LLVMBuildSIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .I16: {
        #partial switch destination_type.kind {
          case .B8, .Bool, .I8:                          return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B16, .B32, .B64, .I16, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8:                                      return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U16, .U32, .U64, .UInt:           return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                               return LLVMBuildSIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .I32: {
        #partial switch destination_type.kind {
          case .B8, .B16, .Bool, .I8, .I16:  return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B32, .B64, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16:                    return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U32, .U64, .UInt:     return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                   return LLVMBuildSIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .I64: {
        #partial switch destination_type.kind {
          case .B8, .B16, .B32, .Bool, .I8, .I16, .I32: return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B64, .I64, .Int:                        return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U8, .U16, .U32:                  return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U64, .UInt:                             return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                              return LLVMBuildSIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .Int, .Untyped_Integer: {
        #partial switch destination_type.kind {
          case .B8, .B16, .B32, .B64, .Bool, .I8, .I16, .I32, .I64, .Int, .U8, .U16, .U32, .U64, .UInt, .Char: return LLVMBuildTrunc(
            generator.builder,
            llvm_source_value,
            llvm_destination_type,
            name,
          );
          case .F32, .F64: return LLVMBuildSIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .B8, .U8: {
        #partial switch destination_type.kind {
          case .Bool:                                         return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B8, .B16, .B32, .B64, .I16, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U8, .U16, .U32, .U64, .UInt:           return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                                    return LLVMBuildUIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .B16, .U16: {
        #partial switch destination_type.kind {
          case .B8, .Bool, .I8:                          return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B16, .B32, .B64, .I16, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8:                                      return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U16, .U32, .U64, .UInt:           return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                               return LLVMBuildUIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .B32, .U32: {
        #partial switch destination_type.kind {
          case .B8, .B16, .Bool, .I8, .I16:  return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B32, .B64, .I32, .I64, .Int: return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16:                    return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U32, .U64, .UInt:     return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                   return LLVMBuildUIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .B64, .U64: {
        #partial switch destination_type.kind {
          case .B8, .B16, .B32, .Bool, .I8, .I16, .I32: return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .B64, .I64, .Int:                        return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16, .U32, .Char:                  return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U64, .UInt:                             return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                              return LLVMBuildUIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .UInt: {
        #partial switch destination_type.kind {
          case .B8, .B16, .B32, .B64, .Bool,.I8, .I16, .I32, .I64, .U8, .U16, .U32, .U64, .UInt, .Char: return LLVMBuildTrunc(
            generator.builder,
            llvm_source_value,
            llvm_destination_type,
            name,
          );
          case .F32, .F64:                               return LLVMBuildUIToFP(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Procedure, .Function, .Pointer, .Rawptr: return LLVMBuildIntToPtr(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .F32: {
        #partial switch destination_type.kind {
          case .I8, .I16, .I32, .I64, .Int:  return LLVMBuildFPToSI(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildFPToUI(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                   return LLVMBuildFPExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .F64, .Untyped_Float: {
        #partial switch destination_type.kind {
          case .I8, .I16, .I32, .I64, .Int:  return LLVMBuildFPToSI(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildFPToUI(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .F32, .F64:                   return LLVMBuildFPTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case .Char, .Untyped_Char: {
        #partial switch destination_type.kind {
          case .I8, .I16:                return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .I32, .I64, .Int:         return LLVMBuildSExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .U8, .U16:                return LLVMBuildTrunc(generator.builder, llvm_source_value, llvm_destination_type, name);
          case .Char, .U32, .U64, .UInt: return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
          case: assert(false);
        }
      }
      case: assert(false);
    }
  } else if type_is_string(source_type) || type_is_string(destination_type) {
    // Here we know we are casting a string literal (or null) to a cstring which means we can just return the source value.
    if (LLVMIsNull(llvm_source_value)) {
      return generator_default_initializer_value(generator, generator.storage.type_string);
    } else {
      return llvm_source_value;
    }
  } else if type_is_absolute_pointer_like(source_type) && type_is_integer(destination_type) {
    return LLVMBuildPtrToInt(generator.builder, llvm_source_value, llvm_destination_type, name);
  } else if type_is_integer(source_type) && type_is_absolute_pointer_like(destination_type) {
    return LLVMBuildIntToPtr(generator.builder, llvm_source_value, llvm_destination_type, name);
  } else if type_is_self_relative_pointer(source_type) {
    assert(type_is_absolute_pointer(destination_type));
    return generator_emit_self_relative_pointer_to_absolute(generator, routine_context, source_type, llvm_source_value);
  } else if destination_is_offset_relative_pointer {
    // We know our source type is an untyped integer.
    return LLVMBuildZExt(generator.builder, llvm_source_value, llvm_destination_type, name);
  }

  assert(false);
  return nil;
}
