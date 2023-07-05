package nox

import "core:strings"
import "tracy"

/**
* The kind of 'params' parameter of the routine.
*/
Params_Kind :: enum {
  None,  // The routine has no 'params' parameter.
  Empty, // No arguments for the 'params' parameter are passed in.
  Pass,  // The single argument for 'params' parameter can just be passed as is (meaning it already is a slice).
  Fill,  // The arguments for the 'params' parameter have to be put into a slice which is then passed.
}


/**
* Emit an expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_debug_emit_location(generator, routine_context, expression.position);

  type := type;
  if type == nil || type_is_any(type) {
    type = generator_get_resolved_type(generator, expression);
  }

  switch expression.kind {
    case .Parenthesized: return generator_emit_expression(generator, routine_context, (cast(^Expression_Parenthesized) expression).expression, type);

    case .Boolean: return generator_emit_expression_boolean(generator, expression, type);
    case .Integer: return generator_emit_expression_integer(generator, expression, type);
    case .Float: return generator_emit_expression_float(generator, expression, type);
    case .Character: return generator_emit_expression_character(generator, expression, type);
    case .String: return generator_emit_expression_string(generator, routine_context, expression, type);
    case .Name: return generator_emit_expression_name(generator, routine_context, expression, type);

    case .Cast: return generator_emit_expression_cast(generator, routine_context, expression);
    case .Call: return generator_emit_expression_call(generator, routine_context, expression);
    case .Index: return generator_emit_expression_index(generator, routine_context, expression);
    case .Slice: return generator_emit_expression_slice(generator, routine_context, expression);
    case .Member: return generator_emit_expression_member(generator, routine_context, expression);
    case .Compound: return generator_emit_expression_compound(generator, routine_context, expression);
    case .Selector: return generator_emit_expression_selector(generator, routine_context, expression);
    case .Implicit_Selector: return generator_emit_expression_implicit_selector(generator, routine_context, expression, type);

    case .Unary: return generator_emit_expression_unary(generator, routine_context, expression, type);
    case .Binary: return generator_emit_expression_binary(generator, routine_context, expression, type);
    case .Ternary: return generator_emit_expression_ternary(generator, routine_context, expression, type);
    case .Modify: return generator_emit_expression_modify(generator, routine_context, expression, type);

    case .Query: return generator_emit_expression_query(generator, routine_context, expression);
    case .Directive: return generator_emit_expression_directive(generator, routine_context, expression);

    case .None: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Emit a boolean expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_boolean :: proc(generator: ^Generator, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);
  return LLVMConstInt(llvm_type, cast(u64) (cast(^Expression_Literal) expression).value.(bool), false);
}

/**
* Emit an integer expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_integer :: proc(generator: ^Generator, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);
  if type_is_float(type) {
    return LLVMConstReal(llvm_type, cast(f64) (cast(^Expression_Literal) expression).value.(u64));  
  } else {
    return LLVMConstInt(llvm_type, (cast(^Expression_Literal) expression).value.(u64), false);
  }
}

/**
* Emit a float expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_float :: proc(generator: ^Generator, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We cannot use generator_constant_to_llvm here as it expects the Value to be in the right type and not just f64.
  llvm_type := generator_type_to_llvm(generator, type);
  return LLVMConstReal(llvm_type, (cast(^Expression_Literal) expression).value.(f64));
}

/**
* Emit a character expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_character :: proc(generator: ^Generator, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_type := generator_type_to_llvm(generator, type);
  return LLVMConstInt(llvm_type, cast(u64) (cast(^Expression_Literal) expression).value.(rune), false);
}

/**
* Emit a string expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_string :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  value := (cast(^Expression_Literal) expression).value.(string);
  llvm_string := generator_string_to_llvm(generator, value, type_is_cstring(type));
  return llvm_string; 
}

/**
* Emit a name expression.
* 
* @param generator  The reference to the generator.
* @param expression The expression to emit.
* @param type       The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_name :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  name := (cast(^Expression_Name) expression).name;
  symbol := generator_get_resolved_symbol(generator, expression);
  return generator_emit_expression_name_symbol(generator, routine_context, expression, type, name, symbol);
}

/**
* Emit a name expression for a symbol.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @param name            The name of the symbol.
* @param symbol          The symbol corresponding to the expression.
* @return The value of the expression.
*/
generator_emit_expression_name_symbol :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression: ^Expression,
  type: ^Type,
  name: string,
  symbol: ^Symbol,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Emitting the correct named symbol is a little bit involved.
  //   1. Check for a constant which we can directly emit as is.
  //   2. Look for a local symbol.
  //   3. If the local is a parameter handle it accordingly.
  //   4. If no local symbol (or paramter) was found look for a global.

  routine_context.last_resolved_symbol_name = name;
  routine_context.last_resolved_symbol_was_parameter = false;

  // We directly generate values for constants. 
  // This also includes predefined constants like: true, false or null.
  if symbol != nil && symbol.kind == .Constant {
    return generator_constant_to_llvm(generator, symbol.value, type);
  }

  llvm_name_value: LLVMValueRef;
  symbol_type: ^Type;

  // First look for locals and parameters.
  llvm_local_symbol, found_local := generator_get_local_symbol(routine_context, name);
  if found_local {
    llvm_name_value = llvm_local_symbol.llvm_value;
    symbol_type = llvm_local_symbol.type;

    if llvm_local_symbol.is_parameter {
      // We have to be careful here as not all parameters are being passed as expected.
      // Parameters that are being passed by value are actually being passed by pointer which needs to loaded first.
      // (Unless we explictily require the raw address from it).

      routine_context.last_resolved_symbol_was_parameter = true;

      if routine_context.current_loading_mode == .Load_Address {
        return llvm_name_value;
      } else if generator_type_is_passed_as_pointer(llvm_local_symbol.type) {
        return generator_emit_load(generator, routine_context, llvm_local_symbol.type, llvm_name_value);
      } else {
        return llvm_name_value;
      }
    }
  }

  // Here we are looking for globals.
  if !found_local {
    symbol := generator_get_resolved_symbol(generator, expression);
    llvm_name_value = generator_symbol_to_llvm(generator, symbol);
    symbol_type = symbol.type;
  }

  return generator_emit_expression_name_load_symbol(generator, routine_context, symbol, symbol_type, llvm_name_value);
}

/**
* Emit a name expression loading from a symbol.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param symbol          The symbol to load from.
* @param type            The type of the expression.
* @param llvm_value      The LLVM value to load.
* @return The value of the expression.
*/
generator_emit_expression_name_load_symbol :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  symbol: ^Symbol,
  type: ^Type,
  llvm_value: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // There are a few cases where we want to return the name_value directly.
  //   1. Routines as they are always represented as pointers.
  //   2. We require it based on the current loading mode.

  if symbol != nil && symbol.kind == .Routine {
    return llvm_value;
  } else if routine_context.current_loading_mode == .Load_Address {
    return llvm_value;
  }

  // For every other case (a regular local or global), we load the value.
  return generator_emit_load(generator, routine_context, type, llvm_value);
}

/**
* Emit a cast expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_cast :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_value := generator_emit_cast_expression(generator, routine_context, expression);

  if routine_context.current_loading_mode == .Load_Address {
    type := generator_get_resolved_type(generator, expression);
    llvm_temporary := generator_emit_temporary(generator, routine_context, type, make_temp_name(routine_context));
    generator_emit_store(generator, routine_context, type, llvm_temporary, llvm_value, expression);
    // This is to allow for expressions like '(cast(*Foo) bar).foo' where 'bar' refers to a parameter.
    routine_context.last_resolved_symbol_was_parameter = false;
    return llvm_temporary;
  } else {
    return llvm_value;
  }
}

/**
* Emit a call expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_call :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_call := cast(^Expression_Call) expression;

  routine_type := generator_get_resolved_type(generator, expression_call.expression);
  type_routine := cast(^Type_Routine) routine_type;

  if .Is_Intrinsic in type_routine.routine_flags {
    return generator_emit_expression_call_intrinsic(generator, routine_context, expression_call);
  } else {
    return generator_emit_expression_call_regular(generator, routine_context, expression_call);
  }
}

/**
* Emit a call expression to an intrinsic routine.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression_call The call expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_call_intrinsic :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression_call: ^Expression_Call) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  symbol := generator_get_resolved_symbol(generator, expression_call.expression);
  assert(symbol != nil);

  // Manually reset the last resolved symbol (as it was the intrinsic routine).
  defer routine_context.last_resolved_symbol_was_parameter = false;

  intrinsic_routine := INTRINSIC_ROUTINES[symbol.name];
  switch intrinsic_routine {
    case .Entry_Point: {
      llvm_arguments := make_dynamic_array_len_cap([dynamic]LLVMValueRef, 0, 1, context.temp_allocator);
      generator_emit_expression_call_regular_context_pointer_argument(generator, routine_context, &llvm_arguments);
      llvm_entry_point_type := generator_type_to_llvm_without_routine_promotion(generator, generator.user_entry_point.type);
      llvm_entry_point := generator_symbol_to_llvm(generator, generator.user_entry_point);
      return LLVMBuildCall2(generator.builder, llvm_entry_point_type, llvm_entry_point, raw_data(llvm_arguments), cast(u32) len(llvm_arguments), "");
    }
    case .Data: {
      assert(len(expression_call.arguments) == 1);
      
      argument_expression := expression_call.arguments[0];
      argument_type := generator_get_resolved_type(generator, argument_expression);

      previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
      llvm_argument := generator_emit_expression(generator, routine_context, argument_expression, argument_type);
      generator_leave_loading_mode(routine_context, previous_loading_mode);

      // We automatically dereference pointers.
      needs_to_dereference_pointer := type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type);
      if needs_to_dereference_pointer {
        pointer_type := argument_type;
        argument_type = argument_type.base;

        if type_is_absolute_pointer(pointer_type) {
          // We exclude special cases were we do not actually need to load the pointer we got from the expression.
          if !routine_context.last_resolved_symbol_was_parameter && argument_expression.kind != .Unary {
            llvm_argument = generator_emit_load(generator, routine_context, pointer_type, llvm_argument);
          }
        } else if type_is_self_relative_pointer(pointer_type) {
          llvm_argument = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, pointer_type, llvm_argument);
        }
      }

      if type_is_array(argument_type) {
        return llvm_argument;
      }

      llvm_slice_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, generator_type_to_llvm(generator, argument_type), llvm_argument);
      if routine_context.current_loading_mode == .Load_Address {
        return llvm_slice_data_pointer;
      } else {
        return generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_slice_data_pointer);
      }
    }
    case .Length: {
      constant, found := generator_get_resolved_constant(generator, expression_call);
      if found {
        return LLVMConstInt(LLVMInt64Type(), cast(u64) constant.value.(int), false);
      }

      assert(len(expression_call.arguments) == 1);
      
      argument_expression := expression_call.arguments[0];
      argument_type := generator_get_resolved_type(generator, argument_expression);

      previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
      llvm_argument := generator_emit_expression(generator, routine_context, argument_expression, argument_type);
      generator_leave_loading_mode(routine_context, previous_loading_mode);
      
      // We automatically dereference pointers.
      needs_to_dereference_pointer := type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type);
      if needs_to_dereference_pointer {
        pointer_type := argument_type;
        argument_type = argument_type.base;

        if type_is_absolute_pointer(pointer_type) {
          // We exclude special cases were we do not actually need to load the pointer we got from the expression.
          if !routine_context.last_resolved_symbol_was_parameter && argument_expression.kind != .Unary {
            llvm_argument = generator_emit_load(generator, routine_context, pointer_type, llvm_argument);
          }
        } else if type_is_self_relative_pointer(pointer_type) {
          llvm_argument = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, pointer_type, llvm_argument);
        }
      }

      llvm_argument_type := generator_type_to_llvm(generator, argument_type);
      llvm_length_pointer: LLVMValueRef;
      if type_is_soa_or_aosoa(argument_type) {
        llvm_length_pointer = generator_emit_gep_layout_collection_length(
          generator.builder,
          routine_context,
          argument_type,
          llvm_argument_type,
          llvm_argument,
        );
      } else {
        llvm_length_pointer = generator_emit_gep_value_length(generator.builder, routine_context, llvm_argument_type, llvm_argument);
      }
      
      if routine_context.current_loading_mode == .Load_Address {
        return llvm_length_pointer;
      } else {
        return generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_length_pointer);
      }
    }
    case .Capacity: {
      assert(len(expression_call.arguments) == 1);
      
      argument_expression := expression_call.arguments[0];
      argument_type := generator_get_resolved_type(generator, argument_expression);

      previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
      llvm_argument := generator_emit_expression(generator, routine_context, argument_expression, argument_type);
      generator_leave_loading_mode(routine_context, previous_loading_mode);

      // We automatically dereference pointers.
      needs_to_dereference_pointer := type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type);
      if needs_to_dereference_pointer {
        pointer_type := argument_type;
        argument_type = argument_type.base;

        if type_is_absolute_pointer(pointer_type) {
          // We exclude special cases were we do not actually need to load the pointer we got from the expression.
          if !routine_context.last_resolved_symbol_was_parameter && argument_expression.kind != .Unary {
            llvm_argument = generator_emit_load(generator, routine_context, pointer_type, llvm_argument);
          }
        } else if type_is_self_relative_pointer(pointer_type) {
          llvm_argument = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, pointer_type, llvm_argument);
        }
      }

      llvm_argument_type := generator_type_to_llvm(generator, argument_type);
      llvm_capacity_pointer: LLVMValueRef;
      if type_is_dynamic_array_soa_or_aosoa(argument_type) {
        base := cast(^Type_Struct) argument_type.base;
        assert(type_is_struct(base));
        capacity_index := len(base.fields) - 2;
        llvm_capacity_pointer = generator_emit_gep_field(generator.builder, routine_context, llvm_argument_type, llvm_argument, capacity_index);
      } else {
        llvm_capacity_pointer = generator_emit_gep_value_capacity(generator.builder, routine_context, llvm_argument_type, llvm_argument);
      }

      if routine_context.current_loading_mode == .Load_Address {
        return llvm_capacity_pointer;
      } else {
        return generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_capacity_pointer);
      }
    }
    case .Hash_Function_Of_Type: {
      hasher_routine_type := cast(^Type_Routine) generator_get_resolved_type(generator, expression_call);
      assert(type_is_routine(hasher_routine_type));
      assert(len(hasher_routine_type.parameters) == 1);
      type := hasher_routine_type.parameters[0].type;

      hasher := generator_get_hash_function_for_type(generator, type);
      if routine_context.current_loading_mode == .Load_Address {
        return generator_emit_temporary_and_store_raw(generator, routine_context, hasher_routine_type, hasher.llvm_symbol, make_temp_name(routine_context));
      } else {
        return hasher.llvm_symbol;
      }
    }
    case .Compare_Function_Of_Type: {
      comparer_routine_type := cast(^Type_Routine) generator_get_resolved_type(generator, expression_call);
      assert(type_is_routine(comparer_routine_type));
      assert(len(comparer_routine_type.parameters) == 2);
      type := comparer_routine_type.parameters[0].type;

      comparer := generator_get_compare_function_for_type(generator, type);
      if routine_context.current_loading_mode == .Load_Address {
        return generator_emit_temporary_and_store_raw(generator, routine_context, comparer_routine_type, comparer.llvm_symbol, make_temp_name(routine_context));
      } else {
        return comparer.llvm_symbol;
      } 
    }
    case .Trap: {
      llvm_intrinsic_panic := generator.llvm_intrinsics[.Trap];
      return LLVMBuildCall2(generator.builder, llvm_intrinsic_panic.llvm_type, llvm_intrinsic_panic.llvm_symbol, nil, 0, "");
    }
    case .Type_Is_Scalar, .Type_Is_Enum, .Type_Is_Struct, .Type_Is_AoSoA, .Type_Is_Trivial_Copyable: fallthrough;
    case .Type_Enum_Item_Count, .Type_Struct_Field_Count, .Type_AoSoA_Chunk_Size, .Type_AoSoA_Items_In_Chunk: {
      constant, found := generator_get_resolved_constant(generator, expression_call);
      assert(found);

      llvm_constant := generator_constant_to_llvm(generator, constant.value, constant.type);

      if routine_context.current_loading_mode == .Load_Address {
        return generator_emit_temporary_and_store_raw(generator, routine_context, constant.type, llvm_constant, make_temp_name(routine_context));
      } else {
        return llvm_constant;
      } 
    }

    case .Invalid: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Emit a call expression to a regular routine.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression_call The call expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_call_regular :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression_call: ^Expression_Call) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  symbol := generator_get_resolved_symbol(generator, expression_call.expression);
  // We skip generating a call for disabled routines.
  if symbol != nil && .Routine_Disabled in symbol.flags {
    return nil;
  }

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_call_expression_value := generator_emit_expression(generator, routine_context, expression_call.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  routine_type := generator_get_resolved_type(generator, expression_call.expression);
  llvm_routine_type := generator_type_to_llvm_without_routine_promotion(generator, routine_type);
  type_routine := cast(^Type_Routine) routine_type;

  llvm_argument_values := make_dynamic_array_len_cap([dynamic]LLVMValueRef, 0, len(expression_call.arguments), context.temp_allocator);

  return_type := generator_get_resolved_type(generator, expression_call);
  // Structs or unions being returned as values are being handled the same as a struct/union argument.
  // We insert the to be returned struct/union as the first argument.
  // Only that we do not need to copy anything as that is what gets returned.
  llvm_return_temporary: LLVMValueRef;
  return_type_is_passed_as_pointer := generator_type_is_passed_as_pointer(return_type)
  if return_type_is_passed_as_pointer {
    llvm_return_temporary = generator_emit_temporary(generator, routine_context, return_type, make_sret_name(routine_context));
    append(&llvm_argument_values, llvm_return_temporary);
  }

  // Routines may get the 'context' as their first argument.
  if type_routine_has_context(type_routine) {
    generator_emit_expression_call_regular_context_pointer_argument(generator, routine_context, &llvm_argument_values);
  }
  // Methods get a 'self' pointer as their first (or second) argument.
  if .Is_Method in type_routine.routine_flags {
    generator_emit_expression_call_regular_self_pointer_argument(generator, routine_context, expression_call, &llvm_argument_values);
  }
  generator_emit_expression_call_regular_arguments(generator, routine_context, expression_call, type_routine, &llvm_argument_values);
  // Emit remaining default parameters that may not have been provided.
  if len(expression_call.arguments) < len(type_routine.parameters) {
    generator_emit_expression_call_regular_default_arguments(generator, routine_context, expression_call, type_routine, &llvm_argument_values);
  }

  name: cstring = "";
  if !return_type_is_passed_as_pointer && !type_is_void(return_type) {
    name = make_value_name(routine_context);
  }
  llvm_return_value := LLVMBuildCall2(
    generator.builder,
    llvm_routine_type,
    llvm_call_expression_value,
    raw_data(llvm_argument_values),
    cast(u32) len(llvm_argument_values),
    name,
  );

  routine_context.last_resolved_symbol_was_parameter = false;

  if return_type_is_passed_as_pointer {
    // For things that are being passed as pointers we have to load them from the out-parameter pointer as a final step.
    if routine_context.current_loading_mode == .Load_Address {
      return llvm_return_temporary;
    } else {
      return generator_emit_load(generator, routine_context, return_type, llvm_return_temporary);
    }
  } else {
    if routine_context.current_loading_mode == .Load_Address {
      return generator_emit_temporary_and_store_raw(generator, routine_context, type_routine.return_type, llvm_return_value, make_temp_name(routine_context));
    } else {
      return llvm_return_value;
    }
  }
}

/**
* Emit the context pointer argument for a regular call expression.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param llvm_argument_values The LLVM arguments to fill.
*/
generator_emit_expression_call_regular_context_pointer_argument :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  llvm_argument_values: ^[dynamic]LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  local_symbol, found := generator_get_local_symbol(routine_context, SPECIAL_NAME_CONTEXT);
  assert(found);

  if local_symbol.is_parameter {
    // Parameters can simply be passed along.
    append(llvm_argument_values, local_symbol.llvm_value);
  } else {
    // Actual locals have to be loaded as a pointer first. This is the case when the 'push_context' statement got used.
    llvm_context_pointer := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, local_symbol.llvm_value);
    append(llvm_argument_values, llvm_context_pointer);
  }
}

/**
* Emit the self-pointer argument for a regular call expression.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param expression_call      The call expression to emit.
* @param llvm_argument_values The LLVM arguments to fill.
*/
generator_emit_expression_call_regular_self_pointer_argument :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_call: ^Expression_Call,
  llvm_argument_values: ^[dynamic]LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // If we have a method we know that previous expression must be a field expression.
  assert(expression_call.expression.kind == .Member);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  self_expression := (cast(^Expression_Member) expression_call.expression).expression;
  llvm_self := generator_emit_expression(generator, routine_context, self_expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  
  self_type := generator_get_resolved_type(generator, self_expression);

  if LLVMIsConstant(llvm_self) {
    llvm_temp := generator_emit_temporary(generator, routine_context, self_type, make_temp_name(routine_context));
    generator_emit_store_raw(generator, llvm_temp, llvm_self);
    llvm_self = llvm_temp;
  } else if type_is_dynamic_pointer(self_type) {
    llvm_type := generator_type_to_llvm(generator, self_type);
    llvm_dynamic_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_type, llvm_self);
    llvm_dynamic_pointer_data := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_dynamic_pointer_data_pointer);
    llvm_self = llvm_dynamic_pointer_data;
  } else if !type_is_absolute_or_relative_pointer_like(self_type) && !generator_type_is_passed_as_pointer(self_type) && routine_context.last_resolved_symbol_was_parameter {
    // When we do not directly have a pointer at our hands we need to handle that.
    // This occurs when basic types are passed and directly used. For example: proc example(i: int) { i.foo(); }.
    // In this scenario we need to create a new proper storage location for the parameter.
    // But it has to be correctly inserted into the scope, so that modifications are propagated properly.
    name := routine_context.last_resolved_symbol_name;

    llvm_type := generator_type_to_llvm(generator, self_type);
    previous_block := generator_enter_llvm_block(generator, routine_context, routine_context.declaration_block);
    llvm_parameter_overwrite := LLVMBuildAlloca(generator.builder, llvm_type, make_variable_name(routine_context, name));
    generator_enter_llvm_block(generator, routine_context, previous_block);

    scope := routine_context.local_scope;
    for scope.parent != nil {
      scope = scope.parent;
    }
    generator_add_local_symbol(name, llvm_parameter_overwrite, llvm_type, self_type, false, scope);

    generator_emit_store_raw(generator, llvm_parameter_overwrite, llvm_self);
    llvm_self = llvm_parameter_overwrite;
  } else if type_is_absolute_pointer(self_type) && !routine_context.last_resolved_symbol_was_parameter {
    llvm_self = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_self);
  } else if type_is_self_relative_pointer(self_type) {
    llvm_self = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, self_type, llvm_self);
    llvm_self = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_self);
  } 

  append(llvm_argument_values, llvm_self);
}

/**
* Emit the arguments for a regular call expression.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param expression_call      The call expression to emit.
* @param type_routine         The type of the routine.
* @param llvm_argument_values The LLVM arguments to fill.
*/
generator_emit_expression_call_regular_arguments :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_call: ^Expression_Call,
  type_routine: ^Type_Routine,
  llvm_argument_values: ^[dynamic]LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  has_params := .Has_Params in type_routine.routine_flags;
  parameter_count := len(type_routine.parameters);

  // There are 3 scenarios for params arguments:
  //   1. We get no additional arguments for the params.
  //      That means we have to create an empty slice that gets passed instead.
  //   2. We get one argument whose slice type is equal to the type of the params slice parameter type.
  //      Here we can just pass along that slice as a regular argument.
  //   3. We get one or more argument where the types do not correspond to the slice parameter type.
  //      This is where we fill an array with all the arguments provided.
  //      This array is the data source for the slice that gets actually passed
  //      and whose length corresponds to the number of params arguments provided.

  params_kind := Params_Kind.None;
  params_type: ^Type;
  llvm_params_slice: LLVMValueRef;
  llvm_params_slice_array: LLVMValueRef;
  llvm_params_slice_array_type: LLVMTypeRef;

  if has_params {
    params_type = type_routine.parameters[parameter_count - 1].type;
    params_arguments_count := len(expression_call.arguments) - parameter_count + 1;

    if params_arguments_count <= 0 {
      params_kind = .Empty;
    } else if params_arguments_count == 1 {
      params_argument_type := generator_get_resolved_overwrite_type(generator, expression_call.arguments[len(expression_call.arguments) - 1]);
      params_kind = params_argument_type == params_type ? .Pass : .Fill;
    } else if params_arguments_count > 1 {
      params_kind = .Fill;
    }

    if params_kind == .Empty || params_kind == .Fill {
      llvm_params_slice = generator_emit_temporary(generator, routine_context, params_type, make_slice_name(routine_context));
      llvm_params_slice_type := generator_type_to_llvm(generator, params_type);

      if params_kind == .Empty {
        generator_emit_store_raw(generator, llvm_params_slice, LLVMConstNull(llvm_params_slice_type));
      } else {
        params_array_type := type_storage_get_or_make_type_array(generator.storage, LAYOUT_INFO_NONE, params_type.base, params_arguments_count, false);
        llvm_params_slice_array = generator_emit_temporary(generator, routine_context, params_array_type, make_array_name(routine_context));
        llvm_params_slice_array_type = generator_type_to_llvm(generator, params_array_type);

        llvm_params_slice_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_params_slice_type, llvm_params_slice);
        generator_emit_store_raw(generator, llvm_params_slice_data_pointer, llvm_params_slice_array);
        llvm_params_slice_length_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_params_slice_type, llvm_params_slice);
        generator_emit_store_raw(generator, llvm_params_slice_length_pointer, LLVMConstInt(LLVMInt64Type(), cast(u64) params_arguments_count, false));
      }
    }
  }

  for argument_expression, i in expression_call.arguments {
    argument_type: ^Type = nil;
    // We can look up the argument type as long as we have enough non c-varargs arguments.
    is_params_argument := false;
    if i < parameter_count {
      parameter := type_routine.parameters[i];
      argument_type = parameter.type;
      
      // Special case for the first params argument.
      if params_kind == .Fill && i == parameter_count - 1 {
        is_params_argument = true;
        argument_type = params_type.base;
      }
    } else if has_params {
      is_params_argument = true;
      argument_type = params_type.base;
    }

    overwrite_argument_type := generator_get_resolved_overwrite_type(generator, argument_expression);
    needs_to_load_address := argument_type == nil ? false : generator_type_requires_loading_address(argument_type, overwrite_argument_type);
    previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
    llvm_argument_value := generator_emit_expression(generator, routine_context, argument_expression, overwrite_argument_type);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    if is_params_argument && params_kind == .Fill {
      params_index := i - parameter_count + 1;
      llvm_params_array_element_pointer := generator_emit_gep_field(
        generator.builder,
        routine_context,
        llvm_params_slice_array_type,
        llvm_params_slice_array,
        params_index,
      );
      generator_emit_store(generator, routine_context, argument_type, llvm_params_array_element_pointer, llvm_argument_value, argument_expression);
    } else {
      // If we pass a struct or union by value we actually pass it in as a pointer. So what we do is the following:
      //   1. Request the regual referenced struct/union.
      //   2. Copy it to a temporary local.
      //   3. Pass in the pointer to that temporary local as the actual argument.
      // This is currently probably undefined for c-varargs.      
      if argument_type != nil && generator_type_is_passed_as_pointer(argument_type) {
        llvm_temporary := generator_emit_temporary(generator, routine_context, argument_type, make_byval_name(routine_context));
        routine_context.is_storing_byval_or_sret = true;
        generator_emit_store(generator, routine_context, argument_type, llvm_temporary, llvm_argument_value, argument_expression);
        routine_context.is_storing_byval_or_sret = false;
        llvm_argument_value = llvm_temporary;
      }
      append(llvm_argument_values, llvm_argument_value);
    }
  }

  if params_kind == .Empty || params_kind == .Fill {
    append(llvm_argument_values, llvm_params_slice);
  }
}

/**
* Emit the default arguments for a regular call expression.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param expression_call      The call expression to emit.
* @param type_routine         The type of the routine.
* @param llvm_argument_values The LLVM arguments to fill.
*/
generator_emit_expression_call_regular_default_arguments :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_call: ^Expression_Call,
  type_routine: ^Type_Routine,
  llvm_argument_values: ^[dynamic]LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  symbol := generator_get_resolved_symbol(generator, expression_call.expression);
  if symbol != nil {
    declaration := symbol.declaration;
    declaration_routine := cast(^Declaration_Routine) declaration;
    
    default_parameter_count_end := len(type_routine.parameters);
    if .Has_Params in type_routine.routine_flags {
      default_parameter_count_end -= 1;
    }

    for i := len(expression_call.arguments); i < default_parameter_count_end; i += 1 {
      parameter := type_routine.parameters[i];
      assert(parameter.is_default);
      declaration_parameter := declaration_routine.parameters[i];

      llvm_initializer_value: LLVMValueRef;
      default_initializer := declaration_parameter.initializer;
      initializer_constant, found_constant := generator_get_resolved_constant(generator, default_initializer);
      if default_initializer.kind == .Directive {
        // We handle directives explicitly, so that the correct source location is being used.
        kind := (cast(^Expression_Directive) default_initializer).directive_kind;
        assert(kind == .Line || kind == .Routine || kind == .File || kind == .Location);
        llvm_initializer_value = generator_emit_expression_directive_constant(generator, routine_context, kind, expression_call.position);
      } else if found_constant {
        llvm_initializer_value = generator_constant_to_llvm(generator, initializer_constant.value, parameter.type);
      } else {
        previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
        llvm_initializer_value = generator_emit_expression(generator, routine_context, default_initializer, parameter.type);
        generator_leave_loading_mode(routine_context, previous_loading_mode);
      }

      if generator_type_is_passed_as_pointer(parameter.type) {
        llvm_temporary := generator_emit_temporary(generator, routine_context, parameter.type, make_byval_name(routine_context));
        generator_emit_store(generator, routine_context, parameter.type, llvm_temporary, llvm_initializer_value, default_initializer);
        llvm_initializer_value = llvm_temporary;
      }

      append(llvm_argument_values, llvm_initializer_value);
    }
  }
}

/**
* Emit an index expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_index :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  
  expression_index := cast(^Expression_Index) expression;

  value_to_index_type := generator_get_resolved_type(generator, expression_index.expression);
  index_type := generator_get_resolved_type(generator, expression_index.index);
  is_using_offset_relative_pointer := type_is_offset_relative_pointer(index_type);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_index := generator_emit_expression(generator, routine_context, expression_index.index, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  needs_to_load_address := !is_using_offset_relative_pointer || type_is_self_relative_pointer(value_to_index_type);
  previous_loading_mode = generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  llvm_value_to_index := generator_emit_expression(generator, routine_context, expression_index.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  needs_to_dereference_pointer := type_is_absolute_pointer(value_to_index_type) || type_is_self_relative_pointer(value_to_index_type); 
  possible_collection_type := value_to_index_type;
  if needs_to_dereference_pointer {
    possible_collection_type = possible_collection_type.base;
  }
  if type_is_soa_or_aosoa(possible_collection_type) {
    llvm_layout_collection := generator_emit_possible_pointer_dereference(generator, routine_context, expression_index.expression, llvm_value_to_index);
    return generator_emit_layout_index_access(generator, routine_context, possible_collection_type, llvm_layout_collection, llvm_index);
  }

  llvm_pointer: LLVMValueRef;
  if is_using_offset_relative_pointer {
    if type_is_self_relative_pointer(value_to_index_type) {
      llvm_value_to_index = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, value_to_index_type, llvm_value_to_index);
    }

    // When dealing with indexing pointers through offset-relative pointer we have to calculate the proper offset based on the pointers base type.
    llvm_pointer = generator_emit_gep_pointer(generator.builder, routine_context, generator_type_to_llvm(generator, index_type.base), llvm_value_to_index, llvm_index);
  } else {
    if needs_to_dereference_pointer {
      value_to_index_type = value_to_index_type.base;
    }
    llvm_value_to_index = generator_emit_possible_pointer_dereference(generator, routine_context, expression_index.expression, llvm_value_to_index);
    llvm_value_to_index_type := generator_type_to_llvm(generator, value_to_index_type);

    if type_is_array(value_to_index_type) {
      llvm_pointer = generator_emit_gep_field_dynamic(generator.builder, routine_context, llvm_value_to_index_type, llvm_value_to_index, llvm_index);
    } else {
      // For dynamic arrays and slices we are basically doing pointer arithmetic with the corresponding 'data' pointer.
      llvm_value_to_index = generator_emit_gep_value_data(generator.builder, routine_context, llvm_value_to_index_type, llvm_value_to_index);
      slice_data_pointer_type := type_storage_get_or_make_type_pointer(generator.storage, value_to_index_type.base);
      llvm_value_to_index_type = generator_type_to_llvm(generator, value_to_index_type.base);
      llvm_value_to_index = generator_emit_load(generator, routine_context, slice_data_pointer_type, llvm_value_to_index);
      llvm_pointer = generator_emit_gep_pointer(generator.builder, routine_context, llvm_value_to_index_type, llvm_value_to_index, llvm_index);
    }
  }

  // Either we load the element at the index or return the pointer to it.
  if routine_context.current_loading_mode == .Load_Address {
    return llvm_pointer;
  } else {
    element_type := generator_get_resolved_type(generator, expression);
    return generator_emit_load(generator, routine_context, element_type, llvm_pointer);
  }
}

/**
* Emit a slice expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_slice :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_slice := cast(^Expression_Slice) expression;

  slice_expression_type := generator_get_resolved_type(generator, expression_slice.expression);
  needs_to_dereference_pointer := type_is_absolute_pointer(slice_expression_type) || type_is_self_relative_pointer(slice_expression_type);
  value_to_slice_type := slice_expression_type;
  if needs_to_dereference_pointer {
    value_to_slice_type = slice_expression_type.base;
  }

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_value_to_slice_pointer := generator_emit_expression(generator, routine_context, expression_slice.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  llvm_value_to_slice_pointer = generator_emit_possible_pointer_dereference(generator, routine_context, expression_slice.expression, llvm_value_to_slice_pointer);

  llvm_value_to_slice_type := generator_type_to_llvm(generator, value_to_slice_type);
  produces_soa_slice := type_is_soa(value_to_slice_type); // Slicing of AoSoA collections is currently not supported and therefore does not need to be handled.

  llvm_value_to_slice_length: LLVMValueRef;
  llvm_value_to_slice_data_pointer: LLVMValueRef;
  if type_is_array(value_to_slice_type) {
    // We might still end up here if we are producing an SoA slice from a fixed array but that is intentional.
    type_array := cast(^Type_Array) value_to_slice_type;
    number_of_elements := cast(u64) type_array.number_of_elements;
    llvm_value_to_slice_length = LLVMConstInt(LLVMInt64Type(), number_of_elements, false);
    llvm_value_to_slice_data_pointer = llvm_value_to_slice_pointer;
  } else if produces_soa_slice {
    // This handles getting the variable length from SoA types (meaning dynamic arrays and slices).
    llvm_value_to_slice_length = generator_emit_gep_layout_collection_length(
      generator.builder,
      routine_context,
      value_to_slice_type,
      llvm_value_to_slice_type,
      llvm_value_to_slice_pointer,
    );
  } else {
    llvm_value_to_slice_data_pointer_pointer := generator_emit_gep_value_data(
      generator.builder,
      routine_context,
      llvm_value_to_slice_type,
      llvm_value_to_slice_pointer,
    );
    llvm_value_to_slice_data_pointer = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_value_to_slice_data_pointer_pointer);
    llvm_value_to_slice_length_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_value_to_slice_type, llvm_value_to_slice_pointer);
    llvm_value_to_slice_length = generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_value_to_slice_length_pointer);
  }

  slice_result_type := generator_get_resolved_type(generator, expression);
  llvm_slice_result_type := generator_type_to_llvm(generator, slice_result_type);
  llvm_slice_result := generator_emit_temporary(generator, routine_context, slice_result_type, make_slice_name(routine_context));

  llvm_lower_bound: LLVMValueRef;
  if expression_slice.lower == nil {
    llvm_lower_bound = LLVMConstInt(LLVMInt64Type(), 0, false);
  } else {
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
    llvm_lower_bound = generator_emit_expression(generator, routine_context, expression_slice.lower, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);
  }

  llvm_higher_bound: LLVMValueRef;
  if expression_slice.higher == nil {
    llvm_higher_bound = llvm_value_to_slice_length;
  } else {
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
    llvm_higher_bound = generator_emit_expression(generator, routine_context, expression_slice.higher, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);
  }

  if produces_soa_slice {
    // For an soa slice we have to deal with multiple data pointers for each field and not just a single one.

    // We iterate over the fields of the 'regular' struct (the base of the layout struct base).
    fields := (cast(^Type_Struct) slice_result_type.base.base).fields;
    for field, i in fields {
      llvm_value_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_value_to_slice_type, llvm_value_to_slice_pointer, i);
      llvm_field_type := generator_type_to_llvm(generator, field.type);
      llvm_value_field_data_pointer := generator_emit_gep_pointer(generator.builder, routine_context, llvm_field_type, llvm_value_field_pointer, llvm_lower_bound);
      llvm_slice_result_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_slice_result_type, llvm_slice_result, i);
      generator_emit_store_raw(generator, llvm_slice_result_field_pointer, llvm_value_field_data_pointer);
    }
  } else {
    // We calculate the data pointer offset based on the lower bound.
    llvm_result_element_type := type_is_string(value_to_slice_type) ? generator_type_to_llvm(generator, generator.storage.type_u8) : generator_type_to_llvm(generator, slice_result_type.base);
    llvm_result_data := generator_emit_gep_pointer(generator.builder, routine_context, llvm_result_element_type, llvm_value_to_slice_data_pointer, llvm_lower_bound);
    llvm_slice_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_slice_result_type, llvm_slice_result);
    generator_emit_store_raw(generator, llvm_slice_data_pointer, llvm_result_data);
  }

  // We calculate the slice length based on the difference between the higher and lower bound.
  llvm_slice_length := LLVMBuildSub(generator.builder, llvm_higher_bound, llvm_lower_bound, make_value_name(routine_context));
  llvm_slice_length_pointer: LLVMValueRef;
  if produces_soa_slice {
    field_offset_from_end := 1;
    length_index := len((cast(^Type_Struct) slice_result_type.base).fields) - field_offset_from_end;
    llvm_slice_length_pointer = generator_emit_gep_field(generator.builder, routine_context, llvm_slice_result_type, llvm_slice_result, length_index);
  } else {
    llvm_slice_length_pointer = generator_emit_gep_value_length(generator.builder, routine_context, llvm_slice_result_type, llvm_slice_result);
  }
  generator_emit_store_raw(generator, llvm_slice_length_pointer, llvm_slice_length);

  if routine_context.current_loading_mode == .Load_Address {
    return llvm_slice_result;
  } else {
    return generator_emit_load(generator, routine_context, slice_result_type, llvm_slice_result);
  }
}

/**
* Emit a member expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_member :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  
  expression_member := cast(^Expression_Member) expression;

  value_type := generator_get_resolved_type(generator, expression);

  // First check if we referenced a symbol which we can just return directly (this is the case for packages or methods).
  symbol := generator_get_resolved_symbol(generator, expression);
  if symbol != nil {
    return generator_emit_expression_name_load_symbol(generator, routine_context, symbol, value_type, generator_symbol_to_llvm(generator, symbol));
  }

  name := expression_member.name;
  member_expression_type := generator_get_resolved_type(generator, expression_member.expression);
  if generator_member_is_soa_or_aosoa_access(generator, expression_member.expression) {
    return generator_emit_expression_member_layout(generator, routine_context, expression_member, value_type);
  }

  needs_to_dereference_pointer := type_is_absolute_pointer(member_expression_type) || type_is_self_relative_pointer(member_expression_type);

  // This is necessary so that chained expressions like 'foo.bar.baz' get correctly generated when 'foo' is a parameter.
  defer routine_context.last_resolved_symbol_was_parameter = false;

  if type_is_enumeration(member_expression_type) {
    // Here we know that we are referring to a enum item constant.
    resolved_constant, _ := generator_get_resolved_constant(generator, expression);
    return generator_constant_to_llvm(generator, resolved_constant.value, value_type);
  } else if type_is_struct(member_expression_type) || needs_to_dereference_pointer {
    struct_type := member_expression_type
    if (needs_to_dereference_pointer) {
      struct_type = member_expression_type.base;
    }

    resolved_member := generator_get_resolved_member(generator, expression);

    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
    llvm_struct_pointer := generator_emit_expression(generator, routine_context, expression_member.expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);
    llvm_struct_pointer = generator_emit_possible_pointer_dereference(generator, routine_context, expression_member.expression, llvm_struct_pointer);

    llvm_struct_type := generator_type_to_llvm(generator, struct_type);
    llvm_field_pointer := generator_emit_gep_member(generator.builder, routine_context, llvm_struct_type, llvm_struct_pointer, resolved_member);

    if routine_context.current_loading_mode == .Load_Address {
      return llvm_field_pointer;
    } else {
      return generator_emit_load(generator, routine_context, value_type, llvm_field_pointer);
    }
  } else if type_is_layout_pointer(member_expression_type) {
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
    llvm_layout_pointer := generator_emit_expression(generator, routine_context, expression_member.expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    layout_collection_type := member_expression_type.base;

    llvm_layout_pointer_type := generator_type_to_llvm(generator, member_expression_type);
    llvm_layout_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    llvm_layout_pointer_data := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_layout_pointer_data_pointer);
    llvm_layout_pointer_index_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    llvm_layout_pointer_index := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_layout_pointer_index_pointer);

    resolved_member := generator_get_resolved_member(generator, expression);
    return generator_emit_layout_member_access(
      generator,
      routine_context,
      layout_collection_type,
      resolved_member,
      value_type,
      llvm_layout_pointer_data,
      llvm_layout_pointer_index,
    );
  } else if type_is_dynamic_pointer(member_expression_type) {
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
    llvm_dynamic_pointer_pointer := generator_emit_expression(generator, routine_context, expression_member.expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    llvm_dynamic_pointer_type := generator_type_to_llvm(generator, member_expression_type);
    llvm_dynamic_pointer_vtable_pointer_pointer := generator_emit_gep_value_length(
      generator.builder,
      routine_context,
      llvm_dynamic_pointer_type,
      llvm_dynamic_pointer_pointer,
    );
    llvm_dynamic_pointer_vtable_pointer := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_dynamic_pointer_vtable_pointer_pointer);

    resolved_member := generator_get_resolved_member(generator, expression);
    
    llvm_vtable_type := generator_type_to_llvm(generator, member_expression_type.base);
    llvm_function_pointer_pointer := generator_emit_gep_member(
      generator.builder,
      routine_context,
      llvm_vtable_type,
      llvm_dynamic_pointer_vtable_pointer,
      resolved_member,
    );

    if routine_context.current_loading_mode == .Load_Address {
      return llvm_function_pointer_pointer;
    } else {
      return generator_emit_load(generator, routine_context, value_type, llvm_function_pointer_pointer);
    }
  }

  assert(false);
  return nil;
}

/**
* Emit a member expression for a layout collection.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param expression_member The expression to emit.
* @param value_type        The type of the member that gets accessed.
* @return The value of the expression.
*/
generator_emit_expression_member_layout :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_member: ^Expression_Member,
  value_type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Here we have two implicit SoA/AoSoA scenarios we handle (not related to SoA/AoSoA pointers):
  //   1. Accessing the field of an element -> Just access the single array/data of the appropriate field.
  //   2. Getting the address of a field of an element -> Here we can just access the single array/data buffer to get appropriate the pointer.

  assert(expression_member.expression.kind == .Index);
  expression_index := cast(^Expression_Index) expression_member.expression;

  layout_collection_type := generator_get_resolved_type(generator, expression_index.expression);
  needs_to_dereference_pointer := type_is_absolute_pointer(layout_collection_type) || type_is_self_relative_pointer(layout_collection_type);
  if needs_to_dereference_pointer {
    layout_collection_type = layout_collection_type.base;
  }
  
  // We directly get the pointer for the collection from the index expression (and skip the regular emission of the index expression).
  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_layout_collection_pointer := generator_emit_expression(generator, routine_context, expression_index.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  llvm_layout_collection_pointer = generator_emit_possible_pointer_dereference(
    generator,
    routine_context,
    expression_index.expression,
    llvm_layout_collection_pointer,
  );
  
  previous_loading_mode = generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_index := generator_emit_expression(generator, routine_context, expression_index.index, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  
  resolved_member := generator_get_resolved_member(generator, expression_member);
  return generator_emit_layout_member_access(
    generator,
    routine_context,
    layout_collection_type,
    resolved_member,
    value_type,
    llvm_layout_collection_pointer,
    llvm_index,
  );
}

/**
* Emit a compound expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_compound :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_compound := cast(^Expression_Compound) expression;
  compound_type := generator_get_resolved_type(generator, expression);

  is_empty := len(expression_compound.fields) == 0;
  if is_empty {
    llvm_initializer_value := generator_default_initializer_value(generator, compound_type);
    if routine_context.current_loading_mode == .Load_Address {
      return generator_emit_temporary_and_store_raw(generator, routine_context, compound_type, llvm_initializer_value, make_compound_name(routine_context));
    } else {
      return llvm_initializer_value;
    }
  }

  is_slice := type_is_slice(compound_type);
  slice_type: ^Type;
  if is_slice {
    slice_type = compound_type;
    compound_type = type_storage_get_or_make_type_array(generator.storage, LAYOUT_INFO_NONE, compound_type.base, len(expression_compound.fields), false);
  }

  llvm_compound: LLVMValueRef;
  if is_slice && routine_context.is_global_initializer {
    // The backing array for slices has to be created globally.
    llvm_compound = generator_emit_temporary_global(generator, compound_type);
  } else {
    llvm_compound = generator_emit_temporary(generator, routine_context, compound_type, make_compound_name(routine_context));
  }
  llvm_compound_type := generator_type_to_llvm(generator, compound_type);

  for compound_field in expression_compound.fields {
    compound_field_index := generator_get_resolved_compound_field(generator, compound_field.initializer);
    compound_field_type := compound_field_index.type;
    
    overwrite_type := generator_get_resolved_overwrite_type(generator, compound_field.initializer);
    needs_to_load_address := generator_type_requires_loading_address(compound_field_type, overwrite_type);
    previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
    llvm_field_value := generator_emit_expression(generator, routine_context, compound_field.initializer, overwrite_type);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    llvm_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_compound_type, llvm_compound, compound_field_index.index);
    generator_emit_store(generator, routine_context, compound_field_type, llvm_field_pointer, llvm_field_value, compound_field.initializer);
  }

  if is_slice {
    return generator_emit_expression_compound_slice(generator, routine_context, slice_type, len(expression_compound.fields), llvm_compound);
  } else {
    if routine_context.current_loading_mode == .Load_Value {
      return generator_emit_load(generator, routine_context, compound_type, llvm_compound);
    } else {
      return llvm_compound;
    }
  }
}

/**
* Emit a compound expression for a slice.
* 
* @param generator          The reference to the generator.
* @param routine_context    The context of the routine.
* @param slice_type         The type of the slice.
* @param element_count      The count of the elements in the slice.
* @param llvm_array_pointer The LLVM pointer to the array the slice will reference.
* @return The value of the expression.
*/
generator_emit_expression_compound_slice :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  slice_type: ^Type,
  element_count: int,
  llvm_array_pointer: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  assert(type_is_slice(slice_type));

  llvm_slice_type := generator_type_to_llvm(generator, slice_type);
  llvm_slice := generator_emit_temporary(generator, routine_context, slice_type, make_slice_name(routine_context));

  // First set the data pointer of the slice.
  llvm_slice_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_slice_type, llvm_slice);
  generator_emit_store_raw(generator, llvm_slice_data_pointer, llvm_array_pointer);

  // Now set the length of the slice.
  llvm_slice_length_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_slice_type, llvm_slice);
  llvm_slice_length_value := LLVMConstInt(LLVMInt64Type(), cast(u64) element_count, false);
  generator_emit_store_raw(generator, llvm_slice_length_pointer, llvm_slice_length_value);

  if routine_context.current_loading_mode == .Load_Address {
    return llvm_slice;
  } else {
    return generator_emit_load(generator, routine_context, slice_type, llvm_slice);
  }
}

/**
* Emit a selector expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_selector :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_selector := cast(^Expression_Selector) expression;

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_union := generator_emit_expression(generator, routine_context, expression_selector.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  llvm_union = generator_emit_possible_pointer_dereference(generator, routine_context, expression_selector.expression, llvm_union);
  
  union_type := generator_get_resolved_type(generator, expression_selector.expression);
  if type_is_absolute_pointer(union_type) || type_is_self_relative_pointer(union_type) {
    union_type = union_type.base;
  }
  llvm_union_type := generator_type_to_llvm(generator, union_type);
  llvm_union_data_pointer := generator_emit_gep_union_value(generator.builder, routine_context, llvm_union_type, llvm_union);

  routine_context.last_resolved_symbol_was_parameter = false;

  if routine_context.current_loading_mode == .Load_Address {
    return llvm_union_data_pointer;
  } else {
    selected_type := generator_get_resolved_type(generator, expression_selector.type);
    return generator_emit_load(generator, routine_context, selected_type, llvm_union_data_pointer);
  }
}

/**
* Emit an implicit selector expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_implicit_selector :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  resolved_constant, found := generator_get_resolved_constant(generator, expression);
  assert(found);
  return generator_constant_to_llvm(generator, resolved_constant.value, type);
}

/**
* Emit a unary expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_unary :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_unary := cast(^Expression_Unary) expression;
  operator := expression_unary.operator;

  if operator == .And || operator == .Multiply {
    return generator_emit_expression_unary_memory(generator, routine_context, expression_unary, type);
  } else {
    return generator_emit_expression_unary_regular(generator, routine_context, expression_unary, type);
  }
}

/**
* Emit a memory unary expression.
* 
* @param generator        The reference to the generator.
* @param routine_context  The context of the routine.
* @param expression_unary The expression to emit.
* @param type             The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_unary_memory :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_unary: ^Expression_Unary,
  type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  operator := expression_unary.operator;
  resolved_expression_type := generator_get_resolved_type(generator, expression_unary.expression);
  is_dereferencing_layout_pointer := type_is_layout_pointer(resolved_expression_type) && operator == .Multiply;
  if type_is_layout_pointer(type) || is_dereferencing_layout_pointer {
    return generator_emit_expression_unary_memory_layout(
      generator,
      routine_context,
      expression_unary,
      is_dereferencing_layout_pointer ? resolved_expression_type : type,
    );
  }
  
  is_self_relative_pointer := type_is_self_relative_pointer(resolved_expression_type);
  needs_to_load_address := operator == .And || is_self_relative_pointer;
  previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  // We explicitly do not request the provided type when dealing with pointers (as they are specially handled).
  expression_type: ^Type = operator == .And || operator == .Multiply ? nil : type;
  llvm_expression_value := generator_emit_expression(generator, routine_context, expression_unary.expression, expression_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  // First we handle the reading and writing of pointers.
  if operator == .Multiply {
    llvm_pointer_to_load := llvm_expression_value;

    if is_self_relative_pointer {
      llvm_pointer_to_load = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, resolved_expression_type, llvm_expression_value);
    }

    if routine_context.current_loading_mode == .Load_Address {
      return llvm_pointer_to_load;
    } else {
      return generator_emit_load(generator, routine_context, type, llvm_pointer_to_load);
    }
  } else {
    assert(operator == .And);

    // We should just be able to return the value, as it should already be a pointer.
    return llvm_expression_value;
  }
}

/**
* Emit a memory unary expression with layout pointers.
* 
* @param generator           The reference to the generator.
* @param routine_context     The context of the routine.
* @param expression_unary    The expression to emit.
* @param layout_pointer_type The type of the layout_pointer.
* @return The value of the expression.
*/
generator_emit_expression_unary_memory_layout :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_unary: ^Expression_Unary,
  layout_pointer_type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  operator := expression_unary.operator;
  llvm_layout_pointer_type := generator_type_to_llvm(generator, layout_pointer_type);

  if operator == .Multiply {
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
    llvm_layout_pointer := generator_emit_expression(generator, routine_context, expression_unary.expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    llvm_layout_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    llvm_layout_pointer_data := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_layout_pointer_data_pointer);
    llvm_layout_pointer_index_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    llvm_layout_pointer_index := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_layout_pointer_index_pointer);
    return generator_emit_layout_index_access(generator, routine_context, layout_pointer_type.base, llvm_layout_pointer_data, llvm_layout_pointer_index);
  } else {
    assert(operator == .And);

    assert(expression_unary.expression.kind == .Index);
    expression_index := cast(^Expression_Index) expression_unary.expression;
    
    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
    llvm_index := generator_emit_expression(generator, routine_context, expression_index.index, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    value_to_index_type := generator_get_resolved_type(generator, expression_index.expression);
    previous_loading_mode = generator_enter_loading_mode(routine_context, .Load_Address);
    llvm_value_to_index := generator_emit_expression(generator, routine_context, expression_index.expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);
    needs_to_dereference_pointer := type_is_absolute_pointer(value_to_index_type) || type_is_self_relative_pointer(value_to_index_type);
    if needs_to_dereference_pointer {
      llvm_value_to_index = generator_emit_possible_pointer_dereference(generator, routine_context, expression_index.expression, llvm_value_to_index);
      value_to_index_type = value_to_index_type.base;
    }
    llvm_value_to_index_type := generator_type_to_llvm(generator, value_to_index_type);
    llvm_value_to_index_data := generator_emit_gep_value_data(generator.builder, routine_context, llvm_value_to_index_type, llvm_value_to_index);

    llvm_layout_pointer := generator_emit_temporary(generator, routine_context, layout_pointer_type, make_temp_name(routine_context));
    llvm_layout_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    generator_emit_store_raw(generator, llvm_layout_pointer_data_pointer, llvm_value_to_index_data);
    llvm_layout_pointer_index_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
    generator_emit_store_raw(generator, llvm_layout_pointer_index_pointer, llvm_index);

    if routine_context.current_loading_mode == .Load_Address {
      return llvm_layout_pointer;
    } else {
      return generator_emit_load(generator, routine_context, layout_pointer_type, llvm_layout_pointer);
    }
  }
}

/**
* Emit a regular unary expression.
* 
* @param generator        The reference to the generator.
* @param routine_context  The context of the routine.
* @param expression_unary The expression to emit.
* @param type             The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_unary_regular :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_unary: ^Expression_Unary,
  type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  operator := expression_unary.operator;
  type := type;

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_value := generator_emit_expression(generator, routine_context, expression_unary.expression, type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  // We handle enums as their base types.
  if type_is_enumeration(type) {
    type = type.base;
  }

  llvm_result: LLVMValueRef;
  if type_is_integer(type) || type_is_float(type) {
    #partial switch operator {
      case .Add: {
        // The single unary '+' operator does not do anything to the value.
        llvm_result = llvm_value;
      }
      case .Subtract: {
        if type_is_integer(type) {
          llvm_result = LLVMBuildNeg(generator.builder, llvm_value, make_value_name(routine_context));
        } else if type_is_float(type) {
          llvm_result = LLVMBuildFNeg(generator.builder, llvm_value, make_value_name(routine_context));
        }
      }
      case .Negate: {
        if type_is_integer(type) {
          // The '~' operator is equivalent to the '^' operator with all ones: ~x == 0xFFFFFFFF ^ x
          llvm_type := generator_type_to_llvm(generator, type);
          llvm_result = LLVMBuildXor(generator.builder, LLVMConstAllOnes(llvm_type), llvm_value, make_value_name(routine_context));
        }
      }
    }
  } else if type_is_boolean(type) {
    assert(operator == .Not);
    llvm_result = LLVMBuildNot(generator.builder, llvm_value, make_value_name(routine_context));
  }

  if routine_context.current_loading_mode == .Load_Address {
    return generator_emit_temporary_and_store_raw(generator, routine_context, type, llvm_result, make_temp_name(routine_context));
  } else {
    return llvm_result;
  }
}

/**
* Emit a binary expression.
* 
* @param generator        The reference to the generator.
* @param routine_context  The context of the routine.
* @param expression        The expression to emit.
* @param type             The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_binary :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_binary := cast(^Expression_Binary) expression;
  operator := expression_binary.operator;

  llvm_result: LLVMValueRef;
  #partial switch operator {
    case .Multiply, .Divide, .Modulo: fallthrough;
    case .Add, .Subtract, .Left_Shift, .Right_Shift: fallthrough;
    case .And, .Or, .Xor: llvm_result = generator_emit_expression_binary_arithmetic(generator, routine_context, expression_binary, type);

    case .Equal, .Not_Equal: fallthrough;
    case .Less_Than, .Less_Than_Equal, .Greater_Than, .Greater_Than_Equal: llvm_result = generator_emit_expression_binary_comparison(
      generator,
      routine_context,
      expression_binary,
    );

    case .And_And, .Or_Or: llvm_result = generator_emit_expression_binary_short_circuit(generator, routine_context, expression_binary, type);
  }

  if routine_context.current_loading_mode == .Load_Address {
    return generator_emit_temporary_and_store_raw(generator, routine_context, type, llvm_result, make_temp_name(routine_context));
  } else {
    return llvm_result;
  }
}

/**
* Emit a binary arithmetic expression.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param expression_binary The expression to emit.
* @param type              The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_binary_arithmetic :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_binary: ^Expression_Binary,
  type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  left_expression := expression_binary.left;
  right_expression := expression_binary.right;

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_value_left := generator_emit_expression(generator, routine_context, left_expression, type);
  llvm_value_right := generator_emit_expression(generator, routine_context, right_expression, type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  left_type := generator_get_resolved_type(generator, left_expression);
  right_type := generator_get_resolved_type(generator, right_expression);

  return generator_emit_expression_binary_arithmetic_operation(
    generator,
    routine_context,
    expression_binary.operator,
    left_type,
    llvm_value_left,
    right_type,
    llvm_value_right,
  );
}

/**
* Emit a binary arithmetic operation.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param operator          The binary operator.
* @param left_type         The type of the left operand.
* @param llvm_value_left   The left LLVM value.
* @param right_type        The type of the right operand.
* @param llvm_value_right  The right LLVM value.
* @return The value of the expression.
*/
generator_emit_expression_binary_arithmetic_operation :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  operator: Token_Kind,
  left_type: ^Type,
  llvm_value_left: LLVMValueRef,
  right_type: ^Type,
  llvm_value_right: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  
  is_doing_pointer_arithemtic := (type_is_absolute_pointer_like(left_type) && type_is_offset_relative_pointer(right_type)) ||
     (type_is_offset_relative_pointer(left_type) && type_is_absolute_pointer_like(right_type)) || 
     (type_is_absolute_pointer_like(left_type) && type_is_absolute_pointer_like(right_type));

  if is_doing_pointer_arithemtic {
    return generator_emit_expression_binary_arithmetic_operation_pointer(
      generator,
      routine_context,
      operator,
      left_type,
      llvm_value_left,
      right_type,
      llvm_value_right,
    );
  } else {
    return generator_emit_expression_binary_arithmetic_operation_regular(
      generator,
      routine_context, 
      operator,
      left_type,
      llvm_value_left,
      right_type,
      llvm_value_right,
    );
  }
}

/**
* Emit a pointer binary arithmetic operation.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param operator          The binary operator.
* @param left_type         The type of the left operand.
* @param llvm_value_left   The left LLVM value.
* @param right_type        The type of the right operand.
* @param llvm_value_right  The right LLVM value.
* @return The value of the expression.
*/
generator_emit_expression_binary_arithmetic_operation_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  operator: Token_Kind,
  left_type: ^Type,
  llvm_value_left: LLVMValueRef,
  right_type: ^Type,
  llvm_value_right: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  #partial switch operator {
    case .Add: {
      absolute_pointer_type: ^Type;
      llvm_pointer_value: LLVMValueRef;
      llvm_offset_relative_pointer_value: LLVMValueRef;
      if type_is_absolute_pointer_like(left_type) {
        absolute_pointer_type = left_type;
        llvm_pointer_value = llvm_value_left;
        relative_base := (cast(^Type_Relative_Pointer) right_type).relative_base;
        llvm_offset_relative_pointer_value = llvm_value_right;
      } else {
        absolute_pointer_type = right_type;
        llvm_pointer_value = llvm_value_right;
        relative_base := (cast(^Type_Relative_Pointer) left_type).relative_base;
        llvm_offset_relative_pointer_value = llvm_value_left;
      }

      llvm_base_type := generator_type_to_llvm(generator, absolute_pointer_type.base);
      llvm_added_pointer := generator_emit_gep_pointer(generator.builder, routine_context, llvm_base_type, llvm_pointer_value, llvm_offset_relative_pointer_value);
      return LLVMBuildIntToPtr(generator.builder, llvm_added_pointer, generator_type_to_llvm(generator, absolute_pointer_type), make_value_name(routine_context));
    }
    case .Subtract: {
      llvm_left_int_pointer := LLVMBuildPtrToInt(generator.builder, llvm_value_left, LLVMInt64Type(), make_value_name(routine_context));
      llvm_right_int_pointer := LLVMBuildPtrToInt(generator.builder, llvm_value_right, LLVMInt64Type(), make_value_name(routine_context));
      llvm_pointer_difference := LLVMBuildSub(generator.builder, llvm_left_int_pointer, llvm_right_int_pointer, make_value_name(routine_context));
      // Because offset-relative pointers store the difference in TYPE UNITS and not bytes, we have to do the conversion here.
      // That means dividing the byte offset by the size of the base type.
      return LLVMBuildSDiv(
        generator.builder,
        llvm_pointer_difference,
        LLVMConstInt(LLVMInt64Type(), cast(u64) type_size_of(left_type), false),
        make_value_name(routine_context),
      );
    }
  }

  assert(false);
  return nil;
}

/**
* Emit a regular binary arithmetic operation.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param operator          The binary operator.
* @param left_type         The type of the left operand.
* @param llvm_value_left   The left LLVM value.
* @param right_type        The type of the right operand.
* @param llvm_value_right  The right LLVM value.
* @return The value of the expression.
*/
generator_emit_expression_binary_arithmetic_operation_regular :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  operator: Token_Kind,
  left_type: ^Type,
  llvm_value_left: LLVMValueRef,
  right_type: ^Type,
  llvm_value_right: LLVMValueRef,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // Here we know that 'type' will be the same for both left and right operands (except shifts).
  type := left_type;
  if type_is_enumeration(type) {
    type = type.base;
  } else if (type_is_offset_relative_pointer(type)) {
    type = (cast(^Type_Relative_Pointer) type).relative_base;
  }

  name := make_value_name(routine_context);

  #partial switch operator {
    case .Multiply: {
      #partial switch type.kind {
        case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: fallthrough;
        case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildMul(generator.builder, llvm_value_left, llvm_value_right, name);
        case .F32, .F64, .Untyped_Float:   return LLVMBuildFMul(generator.builder, llvm_value_left, llvm_value_right, name);
      }
    }
    case .Divide: {
      #partial switch type.kind {
        case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: return LLVMBuildSDiv(generator.builder, llvm_value_left, llvm_value_right, name);
        case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildUDiv(generator.builder, llvm_value_left, llvm_value_right, name);
        case .F32, .F64, .Untyped_Float:   return LLVMBuildFDiv(generator.builder, llvm_value_left, llvm_value_right, name);
      }
    }
    case .Modulo: {
      #partial switch type.kind {
        case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: return LLVMBuildSRem(generator.builder, llvm_value_left, llvm_value_right, name);
        case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildURem(generator.builder, llvm_value_left, llvm_value_right, name);
      }
    }
    case .Add: {
      #partial switch type.kind {
        case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: fallthrough;
        case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildAdd(generator.builder, llvm_value_left, llvm_value_right, name);
        case .F32, .F64, .Untyped_Float:   return LLVMBuildFAdd(generator.builder, llvm_value_left, llvm_value_right, name);
      }
    }
    case .Subtract: {
      #partial switch type.kind {
        case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: fallthrough;
        case .U8, .U16, .U32, .U64, .UInt: return LLVMBuildSub(generator.builder, llvm_value_left, llvm_value_right, name);
        case .F32, .F64, .Untyped_Float:   return LLVMBuildFSub(generator.builder, llvm_value_left, llvm_value_right, name);
      }
    }

    case .Left_Shift: {
      llvm_value_right := generator_emit_expression_binary_shit_type_conversion(generator, routine_context, left_type, right_type, llvm_value_right);
      return LLVMBuildShl(generator.builder, llvm_value_left, llvm_value_right, name);
    } 
    case .Right_Shift: {
      llvm_value_right := generator_emit_expression_binary_shit_type_conversion(generator, routine_context, left_type, right_type, llvm_value_right);
      return LLVMBuildLShr(generator.builder, llvm_value_left, llvm_value_right, name);
    }

    case .And: return LLVMBuildAnd(generator.builder, llvm_value_left, llvm_value_right, name);
    case .Or:  return LLVMBuildOr(generator.builder, llvm_value_left, llvm_value_right, name);
    case .Xor: return LLVMBuildXor(generator.builder, llvm_value_left, llvm_value_right, name);
  }

  assert(false);
  return nil;  
}

/**
* Emit a comparison binary expression.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param expression_binary The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_binary_comparison :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression_binary: ^Expression_Binary) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // We explicitly request the type of the lhs and rhs expressions here as the result will always be a boolean.
  type := generator_get_resolved_type(generator, expression_binary.left);
  if type_is_enumeration(type) {
    type = type.base;
  }

  // There are some types where need the address of the type and not its simple value.
  needs_to_load_address := type_is_self_relative_pointer(type) || type_is_dynamic_pointer(type) || type_is_string(type) || type_is_union(type);
  previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  llvm_value_left := generator_emit_expression(generator, routine_context, expression_binary.left, type);
  llvm_value_right := generator_emit_expression(generator, routine_context, expression_binary.right, type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  #partial switch type.kind {
    case .B8, .B16, .B32, .B64, .Bool: fallthrough;
    case .I8, .I16, .I32, .I64, .Int, .Untyped_Integer: fallthrough;
    case .U8, .U16, .U32, .U64, .UInt: fallthrough;
    case .Char, .Untyped_Char: fallthrough;
    case .Rawptr, .Pointer, .Self_Relative_Pointer, .Offset_Relative_Pointer, .Dynamic_Pointer: fallthrough; // Pointers are treated as integers.
    case .Procedure, .Function: fallthrough; // Routines are represented as pointers.
    case .Typeid, .Untyped_Null: {
      if type_is_self_relative_pointer(type) {
        // When comparing self-relative pointers we actually compare the absolute pointers they compute to.
        // That means we have to explicitly skip the compuation when comparing with the constant 'null'.
        // Because in that case we do actually compare the offset value stored in the pointer.
        if LLVMIsConstant(llvm_value_left) || LLVMIsConstant(llvm_value_right) {
          relative_base := (cast(^Type_Relative_Pointer) type).relative_base;
          // Because we previously requested the pointer when emitting the expressions in the case of self-relative pointers,
          // we know we have to manually do the loading of the offset value when comparing with 'null'.
          // This is also the reason why we get the generator_default_initializer_value for the relative base.
          // We want to compare the integer offset to the integer constant '0' otherwise we would compare with 'null'.
          llvm_pointer_to_load := LLVMIsConstant(llvm_value_left) ? llvm_value_right : llvm_value_left;
          llvm_loaded_value := generator_emit_load(generator, routine_context, relative_base, llvm_pointer_to_load);
          if LLVMIsConstant(llvm_value_left) {
            llvm_value_right = llvm_loaded_value;
            llvm_value_left = generator_default_initializer_value(generator, relative_base);
          } else {
            llvm_value_left = llvm_loaded_value;
            llvm_value_right = generator_default_initializer_value(generator, relative_base);
          }
        } else {
          llvm_value_left = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, type, llvm_value_left);
          llvm_value_right = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, type, llvm_value_right);
        }
      } else if type_is_dynamic_pointer(type) {
        if LLVMIsConstant(llvm_value_left) {
          llvm_value_left = LLVMConstNull(LLVMPointerType(LLVMVoidType(), 0));
        } else {
          llvm_value_left = generator_emit_gep_value_data(generator.builder, routine_context, generator_type_to_llvm(generator, type), llvm_value_left);
          llvm_value_left = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_value_left);
        }
        if LLVMIsConstant(llvm_value_right) {
          llvm_value_right = LLVMConstNull(LLVMPointerType(LLVMVoidType(), 0));
        } else {
          llvm_value_right = generator_emit_gep_value_data(generator.builder, routine_context, generator_type_to_llvm(generator, type), llvm_value_right);
          llvm_value_right = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_value_right);
        }
      }

      llvm_operator: LLVMIntPredicate;
      #partial switch expression_binary.operator {
        case .Equal: llvm_operator = .EQ;
        case .Not_Equal: llvm_operator = .NE;
        case .Less_Than: llvm_operator = type_is_unsigned(type) ? .ULT : .SLT;
        case .Less_Than_Equal: llvm_operator = type_is_unsigned(type) ? .ULE : .SLE;
        case .Greater_Than: llvm_operator = type_is_unsigned(type) ? .UGT : .SGT;
        case .Greater_Than_Equal: llvm_operator = type_is_unsigned(type) ? .UGE : .SGE;
        case: assert(false);
      }

      return LLVMBuildICmp(generator.builder, llvm_operator, llvm_value_left, llvm_value_right, make_value_name(routine_context));
    }

    case .F32, .F64, .Untyped_Float: {
      llvm_operator: LLVMRealPredicate;
      #partial switch expression_binary.operator {
        case .Equal: llvm_operator = .UEQ;
        case .Not_Equal: llvm_operator = .UNE;
        case .Less_Than: llvm_operator = .ULT;
        case .Less_Than_Equal: llvm_operator = .ULE;
        case .Greater_Than: llvm_operator = .UGT;
        case .Greater_Than_Equal: llvm_operator = .UGE;
        case: assert(false);
      }

      return LLVMBuildFCmp(generator.builder, llvm_operator, llvm_value_left, llvm_value_right, make_value_name(routine_context));
    }

    case .CString: {
      if LLVMIsNull(llvm_value_left) || LLVMIsNull(llvm_value_right) {
        llvm_operator: LLVMIntPredicate;
        #partial switch expression_binary.operator {
          case .Equal: llvm_operator = .EQ;
          case .Not_Equal: llvm_operator = .NE;
          case: assert(false);
        }
        return LLVMBuildICmp(generator.builder, llvm_operator, llvm_value_left, llvm_value_right, make_value_name(routine_context));
      } else {
        llvm_arguments: []LLVMValueRef = {llvm_value_left, llvm_value_right};
        llvm_comparison_value := generator_emit_call_to_builtin_routine(generator, .Compare_CString, llvm_arguments, make_value_name(routine_context));
        #partial switch expression_binary.operator {
          case .Equal: return llvm_comparison_value;
          case .Not_Equal: return LLVMBuildNot(generator.builder, llvm_comparison_value, make_value_name(routine_context));
          case: assert(false);
        }
      }
    }
    case .String, .Untyped_String: {
      // For string literals we first have to create a temporaray that we can pass in as a pointer.
      if LLVMIsConstant(llvm_value_left) {
        llvm_value_left = generator_emit_temporary_and_store_raw(
          generator,
          routine_context,
          generator.storage.type_string,
          llvm_value_left,
          make_byval_name(routine_context),
        );
      }
      if LLVMIsConstant(llvm_value_right) {
        llvm_value_right = generator_emit_temporary_and_store_raw(
          generator,
          routine_context,
          generator.storage.type_string,
          llvm_value_right,
          make_byval_name(routine_context),
        );
      }

      llvm_arguments: []LLVMValueRef = {llvm_value_left, llvm_value_right};
      llvm_comparison_value := generator_emit_call_to_builtin_routine(generator, .Compare_String, llvm_arguments, make_value_name(routine_context));
      #partial switch expression_binary.operator {
        case .Equal: return llvm_comparison_value;
        case .Not_Equal: return LLVMBuildNot(generator.builder, llvm_comparison_value, make_value_name(routine_context));
        case: assert(false);
      }
    }

    case .Union: {
      llvm_union_value := LLVMIsNull(llvm_value_left) ? llvm_value_right : llvm_value_left;
      llvm_union_type := generator_type_to_llvm(generator, type);
      llvm_union_tag_pointer := generator_emit_gep_union_tag(generator.builder, routine_context, llvm_union_type, llvm_union_value);
      llvm_union_tag := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_union_tag_pointer);
      llvm_operator: LLVMIntPredicate;
      #partial switch expression_binary.operator {
        case .Equal: llvm_operator = .EQ;
        case .Not_Equal: llvm_operator = .NE;
        case: assert(false);
      }
      return LLVMBuildICmp(generator.builder, llvm_operator, llvm_union_tag, LLVMConstInt(LLVMInt64Type(), 0, false), make_value_name(routine_context));
    }
  }

  assert(false);
  return nil;
}

/**
* Emit a short circuit binary expression.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param expression_binary The expression to emit.
* @param type              The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_binary_short_circuit :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  expression_binary: ^Expression_Binary,
  type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  // NOTE: We have to be VERY careful about the order we append the blocks here.

  is_logical_and := expression_binary.operator == .And_And;

  llvm_value_left := generator_emit_boolean_truncation(generator, routine_context, expression_binary.left, type);
  first_comparison_block := routine_context.current_block;

  // We first generate the short circuit block containing all other expressions.
  short_circuit_block := generator_append_block(routine_context);
  previous_block := generator_enter_llvm_block(generator, routine_context, short_circuit_block);
  llvm_value_right := generator_emit_boolean_truncation(generator, routine_context, expression_binary.right, type);
  last_after_block := routine_context.current_block;
  generator_leave_llvm_block(routine_context, previous_block);

  after_block := generator_append_block(routine_context);

  previous_block = generator_enter_llvm_block(generator, routine_context, last_after_block);
  LLVMBuildBr(generator.builder, after_block);
  generator_leave_llvm_block(routine_context, previous_block);

  previous_block = generator_enter_llvm_block(generator, routine_context, first_comparison_block);
  LLVMBuildCondBr(generator.builder, llvm_value_left, is_logical_and ? short_circuit_block : after_block, is_logical_and ? after_block : short_circuit_block);
  generator_leave_llvm_block(routine_context, previous_block);

  current_block := routine_context.current_block;
  generator_enter_llvm_block(generator, routine_context, after_block);

  llvm_phi_value := LLVMBuildPhi(generator.builder, LLVMInt1Type(), make_value_name(routine_context));
  llvm_phi_incoming_values: []LLVMValueRef = {LLVMConstInt(LLVMInt1Type(), is_logical_and ? 0 : 1, false), llvm_value_right};
  llvm_phi_incoming_blocks: []LLVMBasicBlockRef = {current_block, last_after_block};
  LLVMAddIncoming(llvm_phi_value, raw_data(llvm_phi_incoming_values), raw_data(llvm_phi_incoming_blocks), 2);

  return llvm_phi_value;
}

/**
* Emit a ternary expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_ternary :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_ternary := cast(^Expression_Ternary) expression;

  llvm_then_block := generator_append_block(routine_context);
  llvm_else_block := generator_append_block(routine_context);
  llvm_after_block := generator_append_block(routine_context);

  is_union := type_is_union(type);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);

  llvm_conditional_value := generator_emit_boolean_truncation(generator, routine_context, expression_ternary.condition, generator.storage.type_bool);
  LLVMBuildCondBr(generator.builder, llvm_conditional_value, llvm_then_block, llvm_else_block);

  previous_block := generator_enter_llvm_block(generator, routine_context, llvm_then_block);
  then_type := is_union ? generator_get_resolved_type(generator, expression_ternary.then_expression) : type;
  llvm_then_value := generator_emit_expression(generator, routine_context, expression_ternary.then_expression, then_type);
  if (is_union && type != then_type) {
    llvm_then_value_temporary := generator_emit_temporary(generator, routine_context, type, make_temp_name(routine_context));
    generator_emit_store_union(generator, routine_context, type, llvm_then_value_temporary, llvm_then_value, expression_ternary.then_expression);
    llvm_then_value = generator_emit_load(generator, routine_context, type, llvm_then_value_temporary);
  }
  LLVMBuildBr(generator.builder, llvm_after_block);
  generator_leave_llvm_block(routine_context, previous_block);

  previous_block = generator_enter_llvm_block(generator, routine_context, llvm_else_block);
  else_type := is_union ? generator_get_resolved_type(generator, expression_ternary.else_expression) : type;
  llvm_else_value := generator_emit_expression(generator, routine_context, expression_ternary.else_expression, else_type);
  if (is_union && type != else_type) {
    llvm_else_value_temporary := generator_emit_temporary(generator, routine_context, type, make_temp_name(routine_context));
    generator_emit_store_union(generator, routine_context, type, llvm_else_value_temporary, llvm_else_value, expression_ternary.else_expression);
    llvm_else_value = generator_emit_load(generator, routine_context, type, llvm_else_value_temporary);
  }
  LLVMBuildBr(generator.builder, llvm_after_block);
  generator_leave_llvm_block(routine_context, previous_block);

  generator_leave_loading_mode(routine_context, previous_loading_mode);

  generator_enter_llvm_block(generator, routine_context, llvm_after_block);

  llvm_type := generator_type_to_llvm(generator, type);
  llvm_ternary_value := LLVMBuildPhi(generator.builder, llvm_type, make_value_name(routine_context));
  llvm_phi_incoming_values: []LLVMValueRef = {llvm_then_value, llvm_else_value};
  llvm_phi_incoming_bocks: []LLVMBasicBlockRef = {llvm_then_block, llvm_else_block};
  LLVMAddIncoming(llvm_ternary_value, raw_data(llvm_phi_incoming_values), raw_data(llvm_phi_incoming_bocks), cast(u32) len(llvm_phi_incoming_values));

  if (routine_context.current_loading_mode == .Load_Address) {
    return generator_emit_temporary_and_store_raw(generator, routine_context, type, llvm_ternary_value, make_temp_name(routine_context));
  } else {
    return llvm_ternary_value;
  }
}

/**
* Emit a modify expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @param type            The type of the expression.
* @return The value of the expression.
*/
generator_emit_expression_modify :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression, type: ^Type) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  expression_modify := cast(^Expression_Modify) expression;

  operator := expression_modify.operator;

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_value_pointer := generator_emit_expression(generator, routine_context, expression_modify.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  // Load the value from the pointer.
  llvm_type := generator_type_to_llvm(generator, type);
  previous_loading_mode = generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_value_to_modify := generator_emit_expression(generator, routine_context, expression_modify.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  llvm_modify_constant := LLVMConstInt(llvm_type, 1, false);
  
  // Modify the value.
  llvm_modified_value: LLVMValueRef;
  if operator == .Increment {
    llvm_modified_value = LLVMBuildAdd(generator.builder, llvm_value_to_modify, llvm_modify_constant, make_value_name(routine_context));
  } else {
    assert(operator == .Decrement);
    llvm_modified_value = LLVMBuildSub(generator.builder, llvm_value_to_modify, llvm_modify_constant, make_value_name(routine_context));
  }

  // Now store the modified value back.
  // We can use 'generator_emit_store_raw' here as we will never have to deal with self-relative pointers here.
  generator_emit_store_raw(generator, llvm_value_pointer, llvm_modified_value);

  // Return the corresponding value.
  if expression_modify.is_post {
    // Post increment/decrement just returns the value not modified.
    return llvm_value_to_modify;
  } else {
    return llvm_modified_value
  }
}

/**
* Emit a query expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_query :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
  
  expression_query := cast(^Expression_Query) expression;

  switch expression_query.query_kind {
    case .Size_Of_Expression, .Size_Of_Type, .Typeid_Of_Expression, .Typeid_Of_Type: {
      if expression_query.query_kind == .Typeid_Of_Expression {
        query_expression := expression_query.value.(^Expression);
        expression_type := generator_get_resolved_type(generator, query_expression);
        if type_is_any(expression_type) {
          return generator_emit_typeid_expression_any(generator, routine_context, query_expression);
        } else if type_is_union(expression_type) {
          return generator_emit_typeid_expression_union(generator, routine_context, query_expression, expression_type);
        }
      }

      constant, found := generator_get_resolved_constant(generator, expression);
      assert(found);
      return LLVMConstInt(LLVMInt64Type(), cast(u64) constant.value.(uint), false);
    }
    case .Type_Info_Of_Expression, .Type_Info_Of_Type: {
      llvm_index: LLVMValueRef;
      if expression_query.query_kind == .Type_Info_Of_Expression {
        // For expressions we need to dynamically lookup the actual type info index in the indices table.
        previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
        llvm_type_info_indices_index := generator_emit_expression(generator, routine_context, expression_query.value.(^Expression), nil);
        generator_leave_loading_mode(routine_context, previous_loading_mode);
        llvm_type_info_indices_index_pointer := generator_emit_gep_field_dynamic(
          generator.builder,
          routine_context,
          generator.llvm_type_info_table_indices_map_type,
          generator.llvm_type_info_table_indices_map,
          llvm_type_info_indices_index,
        );
        llvm_index = generator_emit_load(generator, routine_context, generator.storage.type_uint, llvm_type_info_indices_index_pointer);
      } else {
        constant, found_constant := generator_get_resolved_constant(generator, expression);
        assert(found_constant);

        // Constants can directly be converted to the proper table index.
        index, found_index := generator.type_info_table_indices[constant.value.(uint)];
        assert(found_index)
        llvm_index = LLVMConstInt(LLVMInt32Type(), index, false);
      }

      llvm_type_info_pointer := generator_emit_gep_field_dynamic(
        generator.builder,
        routine_context,
        generator.llvm_type_info_table_type,
        generator.llvm_type_info_table,
        llvm_index,
      );

      if routine_context.current_loading_mode == .Load_Address {
        type_info_pointer_type := type_storage_get_or_make_type_pointer(generator.storage, generator.storage.cached_runtime_types.type_info);
        llvm_type_info_pointer_temporary := generator_emit_temporary(generator, routine_context, type_info_pointer_type, make_temp_name(routine_context));
        generator_emit_store_raw(generator, llvm_type_info_pointer_temporary, llvm_type_info_pointer);
        return llvm_type_info_pointer_temporary;
      } else {
        return llvm_type_info_pointer;
      }
    }
  }

  assert(false);
  return nil;
}

/**
* Emit a directive expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param expression      The expression to emit.
* @return The value of the expression.
*/
generator_emit_expression_directive :: proc(generator: ^Generator, routine_context: ^Routine_Context, expression: ^Expression) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  kind := (cast(^Expression_Directive) expression).directive_kind;
  return generator_emit_expression_directive_constant(generator, routine_context, kind, expression.position);
}

/**
* Emit a constant directive expression.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param kind            The directive expression kind.
* @param position        The position of the directive expression.
* @return The value of the expression.
*/
generator_emit_expression_directive_constant :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  kind: Expression_Directive_Kind,
  position: Source_Position,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  switch kind {
    case .Line: return generator_constant_to_llvm(generator, cast(int) position.line, generator.storage.type_int);
    case .Routine: {
      // The symbol of the procedure context may be null if we are currently emitting a global initializer.
      routine_name := "";
      if routine_context.symbol != nil {
        routine_name = routine_context.symbol.name;
      }
      return generator_constant_to_llvm(generator, routine_name, generator.storage.type_string);
    }
    case .File: return generator_constant_to_llvm(generator, position.file, generator.storage.type_string);
    case .Location: {
      llvm_line := generator_constant_to_llvm(generator, cast(int) position.line, generator.storage.type_int);
      routine_name := "";
      if routine_context.symbol != nil {
        routine_name = routine_context.symbol.name;
      }
      llvm_procedure_name := generator_constant_to_llvm(generator, routine_name, generator.storage.type_string);
      llvm_file := generator_constant_to_llvm(generator, position.file, generator.storage.type_string);

      llvm_location_elements: []LLVMValueRef = {llvm_line, llvm_procedure_name, llvm_file};
      return LLVMConstStruct(raw_data(llvm_location_elements), cast(u32) len(llvm_location_elements), false);
    }
  }

  assert(false);
  return nil;
}
