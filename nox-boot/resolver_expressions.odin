package nox

import "tracy"

/**
* Resolves an expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  return resolver_resolve_expression_expected(resolver, expression, statement_context, nil);
}

/**
* Resolves an expression as an rvalue.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_rvalue :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  return operand_decay(resolver_resolve_expression(resolver, expression, statement_context));
}

/**
* Resolves an expression as an rvalue with an expected type.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_expected_rvalue :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  statement_context: Statement_Context,
  expected_type: ^Type,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  return operand_decay(resolver_resolve_expression_expected(resolver, expression, statement_context, expected_type));
}

/**
* Resolves an expression as a condition.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_condition :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  condition := resolver_resolve_expression_rvalue(resolver, expression, statement_context);
  if !type_is_boolean(condition.type) {
    report_error_fatal(expression.position, "Conditional expression must have a boolean type. Got '%v'", condition.type.name);
  }
  return condition;
}

/**
* Resolves an expression as a constant.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_constant :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  return resolver_resolve_expression_constant_expected(resolver, expression, statement_context, nil);
}

/**
* Resolves an expression as a constant with an expected type.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_constant_expected :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  statement_context: Statement_Context,
  expected_type: ^Type,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  result := resolver_resolve_expression_expected(resolver, expression, statement_context, expected_type);
  if .Is_Constant not_in result.flags {
    report_error_fatal(expression.position, "Expected constant expression");
  }
  return result;
}

/**
* Resolves an expression with an expected type.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_expected :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  result: Operand;
  switch expression.kind {
    case .Parenthesized: result = resolver_resolve_expression_expected(resolver, (cast(^Expression_Parenthesized) expression).expression, statement_context, expected_type);

    case .Boolean: result = operand_constant(resolver.storage.type_untyped_boolean, (cast(^Expression_Literal) expression).value.(bool));
    case .Integer: result = operand_constant(resolver.storage.type_untyped_integer, (cast(^Expression_Literal) expression).value.(u64));
    case .Float: result = operand_constant(resolver.storage.type_untyped_float, (cast(^Expression_Literal) expression).value.(f64));
    case .Character: result = operand_constant(resolver.storage.type_untyped_char, (cast(^Expression_Literal) expression).value.(rune));
    case .String: result = operand_constant(resolver.storage.type_untyped_string, (cast(^Expression_Literal) expression).value.(string));
    case .Name: result = resolver_resolve_expression_name(resolver, expression, statement_context);

    case .Cast: result = resolver_resolve_expression_cast(resolver, expression, statement_context);
    case .Call: result = resolver_resolve_expression_call(resolver, expression, statement_context, expected_type);
    case .Index: result = resolver_resolve_expression_index(resolver, expression, statement_context);
    case .Slice: result = resolver_resolve_expression_slice(resolver, expression, statement_context);
    case .Member: result = resolver_resolve_expression_member(resolver, expression, statement_context);
    case .Compound: result = resolver_resolve_expression_compound(resolver, expression, statement_context, expected_type);
    case .Selector: result = resolver_resolve_expression_selector(resolver, expression, statement_context);
    case .Implicit_Selector: result = resolver_resolve_expression_implicit_selector(resolver, expression, expected_type);

    case .Unary: result = resolver_resolve_expression_unary(resolver, expression, statement_context, expected_type);
    case .Binary: result = resolver_resolve_expression_binary(resolver, expression, statement_context);
    case .Ternary: result = resolver_resolve_expression_ternary(resolver, expression, statement_context, expected_type);
    case .Modify: result = resolver_resolve_expression_modify(resolver, expression, statement_context);

    case .Query: result = resolver_resolve_expression_query(resolver, expression, statement_context);
    case .Directive: result = resolver_resolve_expression_directive(resolver, expression);

    case .None: fallthrough;
    case: assert(false);
  }

  resolver_set_resolved_type(resolver, expression, result.type);

  if statement_context.statement_restriction != .Allow_All {
    if .Is_Pure not_in result.flags {
      report_error_fatal(expression.position, "Expression in function is not pure");
    }
  }

  return result;
}

/**
* Resolves a name expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_name :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_name := cast(^Expression_Name) expression;
  name := expression_name.name;

  compiler := cast(^Compiler) context.user_ptr;
  symbol := resolver_resolve_name(resolver, expression.position, compiler.current_package, name);
  operand := resolver_resolve_expression_name_operand(resolver, expression.position, statement_context, symbol, name, expression_name.generic_types);
  resolver_set_resolved_symbol(resolver, expression, symbol);

  return operand;
}

/**
* Resolves a name expression.
*
* @param resolver          The reference to the resolver.
* @param position          The position of the name expression.
* @param statement_context The statement context to use.
* @param symbol            The symbol the name corresponds to.
* @param name              The name of the expression.
* @param generic_types     The generic types of the expression.
* @return The operand of the expression.
*/
resolver_resolve_expression_name_operand :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  statement_context: Statement_Context,
  symbol: ^Symbol,
  name: string,
  generic_types: [dynamic]^Type_Specification,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  is_immutable := .Local_Immutable in symbol.flags;

  operand: Operand;
  if symbol.kind == .Constant || (symbol.type != nil && type_is_constant(symbol.type)) {
    // The additional type check here stems from the fact that we want to allow constant values which might be defined locally.
    // This is the case for enum items for example.
    operand = operand_constant(symbol.type, symbol.value);
  } else if symbol.kind == .Global || symbol.kind == .Local {
    operand = operand_lvalue(symbol.type, symbol.kind == .Local, is_immutable);
  } else if symbol.kind == .Parameter {
    operand = operand_parameter(symbol.type, is_immutable);
  } else if symbol.kind == .Routine {
    operand = operand_rvalue(symbol.type, .Routine_Function in symbol.flags, is_immutable);
  } else if .Can_Refer_To_Enum_Item in statement_context.legal_statements && symbol.kind == .Type && type_is_enumeration(symbol.type) {
    // Enums can be referred to by name and as such we are returning them as a constant.
    // This is a little hacky because that is only ever allowed it we are part of a member expression.
    operand = operand_constant(symbol.type, nil);
  } else {
    report_error_fatal(position, "'%v' must be a constant, global, local or routine", name);
  }

  if (symbol.kind != .Routine && len(generic_types) > 0) {
    report_error_fatal(position, "'%v' does not have generic type arguments", name);
  }

  return operand;
}

/**
* Resolves a cast expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_cast :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_cast := cast(^Expression_Cast) expression;

  type := resolver_resolve_type_specification(resolver, expression_cast.type);
  operand := resolver_resolve_expression_expected_rvalue(resolver, expression_cast.expression, statement_context, type);

  if !operand_cast(resolver, expression.position, &operand, type) {
    report_error_fatal(expression.position, "Invalid type cast '%v' to '%v'", operand.type.name, type.name);
  }

  return operand;
}

/**
* Resolves a call expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_call :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_call := cast(^Expression_Call) expression;
  call_expression := expression_call.expression;

  routine := resolver_resolve_expression_rvalue(resolver, call_expression, statement_context);
  routine_type := routine.type;

  if routine_type.kind != .Procedure && routine_type.kind != .Function {
    report_error_fatal(call_expression.position, "Can only call procedures or functions");
  }

  if .Is_Intrinsic in (cast(^Type_Routine) routine_type).routine_flags {
    return resolver_resolve_expression_call_intrinsic(resolver, expression_call, statement_context, routine_type);
  }

  symbol := routine_type.symbol;
  if symbol != nil {
    if .Uninstantiated_Generic in symbol.flags {
      routine_type = resolver_resolve_generic_expression_call(resolver, expression, statement_context, routine_type, symbol);
    }
  } else {
    if call_expression.kind == .Name || call_expression.kind == .Member {
      has_generic_parameters := false;
      if call_expression.kind == .Name {
        has_generic_parameters = len((cast(^Expression_Name) call_expression).generic_types) > 0;
      } else {
        has_generic_parameters = len((cast(^Expression_Member) call_expression).generic_types) > 0;
      }
  
      if has_generic_parameters {
        report_error_fatal(call_expression.position, "Routine has no generic type parameters");
      }
    }
  }

  type_routine := cast(^Type_Routine) routine_type;
  if type_routine.calling_convention == .Nox && .Can_Call_Routine_With_Nox_Calling_Convention not_in statement_context.legal_statements {
    report_error_fatal(expression.position, "Can't call routine with 'Nox' calling convention from routine which doesn't push a context explicitly");
  }

  parameter_count := len(type_routine.parameters);
  parameter_count_without_defaults := parameter_count;
  for parameter in type_routine.parameters {
    if parameter.is_default {
      parameter_count_without_defaults -= 1;
    }
  }
  minimum_argument_count := parameter_count_without_defaults;
  has_params := .Has_Params in type_routine.routine_flags;
  if has_params {
    minimum_argument_count -= 1;
  }

  expression_argument_count := len(expression_call.arguments);

  if expression_argument_count < minimum_argument_count {
    report_error_fatal(expression.position, "Routine call with too few arguments. Expected %v got %v", minimum_argument_count, expression_argument_count);
  } else if expression_argument_count > parameter_count && !has_params && .Has_C_Varargs not_in type_routine.routine_flags {
    report_error_fatal(expression.position, "Routine call with too many arguments. Expected %v got %v", parameter_count, expression_argument_count);
  }

  return resolver_resolve_expression_call_regular(resolver, expression_call, statement_context, routine_type, expected_type);
}

/**
* Resolves an intrinsic call expression.
*
* @param resolver          The reference to the resolver.
* @param expression_call   The call expression to resolve.
* @param statement_context The statement context to use.
* @param routine_type      The type of the routine.
* @return The operand of the expression.
*/
resolver_resolve_expression_call_intrinsic :: proc(
  resolver: ^Resolver,
  expression_call: ^Expression_Call,
  statement_context: Statement_Context,
  routine_type: ^Type,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  symbol := resolver.output.resolved_symbols[expression_call.expression];
  assert(symbol != nil);
  
  intrinsic_routine, found_intrinsic_routine := INTRINSIC_ROUTINES[symbol.name];
  assert(found_intrinsic_routine);
  switch intrinsic_routine {
    case .Entry_Point: {
      if len(expression_call.arguments) != 0 {
        report_error_fatal(
          expression_call.position,
          "Invalid number of arguments to internal routine call 'entry_pointer'. Expected 0 got %v",
          len(expression_call.arguments),
        );
      }

      return operand_rvalue(resolver.storage.type_void, false, false);
    }
    case .Data: {
      if len(expression_call.arguments) != 1 {
        report_error_fatal(expression_call.position, "Invalid number of arguments to internal routine call 'data'. Expected 1 got %v", len(expression_call.arguments));
      }

      argument_expression := expression_call.arguments[0];
      argument := resolver_resolve_expression_rvalue(resolver, argument_expression, statement_context);

      argument_type := argument.type;
      if type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type) {
        argument_type = argument_type.base;
      }

      if !type_is_string(argument_type) && !type_is_array(argument_type) && !type_is_dynamic_array(argument_type) && !type_is_slice(argument_type) && !type_is_any(argument_type) {
        report_error_fatal(argument_expression.position, "Invalid argument type in internal routine call to 'data'. Expected string, array, dynamic array, slice or any type. Got '%v'", argument.type.name);
      }

      if type_is_soa_or_aosoa(argument_type) {
        report_error_fatal(argument_expression.position, "SoA/AoSoA type '%v' does not have a distinct data pointer", argument_type.name);
      }

      data_pointer_type := resolver.storage.type_rawptr;
      if !type_is_string(argument_type) && !type_is_any(argument_type) {
        data_pointer_type = type_storage_get_or_make_type_pointer(resolver.storage, argument_type.base);
      }
      return operand_rvalue(data_pointer_type, true, false);
    }
    case .Length: {
      if len(expression_call.arguments) != 1 {
        report_error_fatal(
          expression_call.position,
          "Invalid number of arguments to internal routine call 'length'. Expected 1 got %v",
          len(expression_call.arguments),
        );
      }

      argument_expression := expression_call.arguments[0];
      argument := resolver_resolve_expression_rvalue(resolver, argument_expression, statement_context);

      argument_type := argument.type;
      if type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type) {
        argument_type = argument_type.base;
      }

      if type_is_string(argument_type) {
        // For strings we do an explicit check for constants which we can simply resolve to later.
        if .Is_Constant in argument.flags {
          constant_length := operand_constant(resolver.storage.type_int, len(argument.value.(string)));
          resolver_set_resolved_constant(resolver, expression_call, constant_length);
          return constant_length;
        }
      } else if type_is_array(argument_type) {
        // We will always know the length of arrays at compile time.
        type_array := cast(^Type_Array) argument_type;
        assert(!type_array.has_incomplete_elements);

        constant_length := operand_constant(resolver.storage.type_int, type_array.number_of_elements);
        resolver_set_resolved_constant(resolver, expression_call, constant_length);
        return constant_length;
      } else if !type_is_slice(argument_type) && !type_is_dynamic_array(argument_type) && !type_is_map(argument_type) {
        report_error_fatal(
          argument_expression.position,
          "Invalid argument type in internal routine call to 'length'. Expected string, array, dynamic array, slice or map type. Got '%v'",
          argument.type.name,
        );
      }

      return operand_rvalue(resolver.storage.type_int, true, false);
    }
    case .Capacity: {
      if len(expression_call.arguments) != 1 {
        report_error_fatal(
          expression_call.position,
          "Invalid number of arguments to internal routine call 'capacity'. Expected 1 got %v",
          len(expression_call.arguments),
        );
      }

      argument_expression := expression_call.arguments[0];
      argument := resolver_resolve_expression_rvalue(resolver, argument_expression, statement_context);

      argument_type := argument.type;
      if type_is_absolute_pointer(argument_type) || type_is_self_relative_pointer(argument_type) {
        argument_type = argument_type.base;
      }

      if !type_is_dynamic_array(argument_type) && !type_is_map(argument_type) {
        report_error_fatal(
          argument_expression.position,
          "Invalid argument type in internal routine call to 'capacity'. Expected dynamic array or map type. Got '%v'",
          argument.type.name,
        );
      }

      return operand_rvalue(resolver.storage.type_int, true, false);
    }
    case .Hash_Function_Of_Type: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      if .Has_Hash_Function not_in type.flags {
        report_error_fatal(expression_call.position, "The type '%v' does not have a trivial hash function", type.name);
      }

      parameters: #soa [dynamic]Type_Routine_Parameter;
      append_soa(&parameters, Type_Routine_Parameter{type, false});
      hasher_routine_type := type_storage_get_or_make_type_routine(
        resolver.storage,
        .Function,
        .No_Context,
        parameters,
        resolver.storage.type_uint,
        type_make_routine_flags(false, false, false, false, true),
      );

      return operand_rvalue(hasher_routine_type, true, false);
    }
    case .Compare_Function_Of_Type: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      if .Has_Compare_Function not_in type.flags {
        report_error_fatal(expression_call.position, "The type '%v' does not have a trivial compare function", type.name);
      }

      parameters: #soa [dynamic]Type_Routine_Parameter;
      append_soa(&parameters, Type_Routine_Parameter{type, false});
      append_soa(&parameters, Type_Routine_Parameter{type, false});
      comparer_routine_type := type_storage_get_or_make_type_routine(
        resolver.storage,
        .Function, 
        .No_Context,
        parameters,
        resolver.storage.type_bool,
        type_make_routine_flags(false, false, false, false, true),
      );

      return operand_rvalue(comparer_routine_type, true, false);
    }
    case .Trap: {
      return operand_rvalue(resolver.storage.type_void, false, false);
    }
    case .Type_Is_Scalar: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      is_scalar := type_is_scalar(type);
      constant := operand_constant(resolver.storage.type_bool, is_scalar);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Is_Struct: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      is_struct := type_is_struct(type);
      constant := operand_constant(resolver.storage.type_bool, is_struct);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Is_Enum: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      is_enum := type_is_enumeration(type);
      constant := operand_constant(resolver.storage.type_bool, is_enum);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Is_AoSoA: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      is_aosoa := type_is_aosoa(type);
      constant := operand_constant(resolver.storage.type_bool, is_aosoa);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Is_Trivial_Copyable: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);

      is_trivial_copyable := !type_contains_self_relative_pointer_in_value(type);
      constant := operand_constant(resolver.storage.type_bool, is_trivial_copyable);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Enum_Item_Count: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);
      if !type_is_enumeration(type) {
        report_error_fatal(expression_call.position, "The type '%v' is not an enum", type.name);
      }

      item_count := len((cast(^Type_Enumeration) type).items);
      constant := operand_constant(resolver.storage.type_int, item_count);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_Struct_Field_Count: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);
      if !type_is_struct(type) {
        report_error_fatal(expression_call.position, "The type '%v' is not a struct", type.name);
      }

      field_count := len((cast(^Type_Struct) type).fields);
      constant := operand_constant(resolver.storage.type_int, field_count);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_AoSoA_Chunk_Size: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);
      if !type_is_aosoa(type) {
        report_error_fatal(expression_call.position, "The type '%v' is not an AoSoA type", type.name);
      }

      chunk_size := type_size_of((cast(^Type_Struct) type.base).fields[0].type.base);
      constant := operand_constant(resolver.storage.type_uint, chunk_size);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }
    case .Type_AoSoA_Items_In_Chunk: {
      type := resolver_resolve_expression_call_intrinsic_get_first_generic_argument(resolver, expression_call, symbol);
      if !type_is_aosoa(type) {
        report_error_fatal(expression_call.position, "The type '%v' is not an AoSoA type", type.name);
      }

      items_in_aosoa_chunk := (cast(^Type_Struct) type.base).layout_info.items_in_aosoa_chunk;
      constant := operand_constant(resolver.storage.type_int, items_in_aosoa_chunk);
      resolver_set_resolved_constant(resolver, expression_call, constant);

      return constant;
    }

    case .Invalid: fallthrough;
    case: assert(false);
  }

  report_error_fatal(expression_call.position, "Invalid call to internal routine with name '%v'", symbol.name);
  return {};
}

/**
* Resolves the first generic type argument of an intrinsic call.
*
* @param resolver        The reference to the resolver.
* @param expression_call The call expression.
* @param symbol          The symbol of the intrinsic routine.
* @return The type of the generic argument.
*/
resolver_resolve_expression_call_intrinsic_get_first_generic_argument :: proc(resolver: ^Resolver, expression_call: ^Expression_Call, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(expression_call.expression.kind == .Name);
  generic_types := (cast(^Expression_Name) expression_call.expression).generic_types;
  if len(generic_types) != 1 {
    report_error_fatal(
      expression_call.position,
      "Invalid number of generic type arguments to internal routine call '%v'. Expected 1 got %v",
      symbol.name,
      len(generic_types),
    );
  }

  generic_type := generic_types[0];
  type := resolver_resolve_type_specification(resolver, generic_type);
  resolver_complete_type(resolver, generic_type.position, type);

  return type;
}

/**
* Resolves a regular call expression.
*
* @param resolver          The reference to the resolver.
* @param expression_call   The call expression to resolve.
* @param statement_context The statement context to use.
* @param routine_type      The type of the routine.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_call_regular :: proc(
  resolver: ^Resolver,
  expression_call: ^Expression_Call,
  statement_context: Statement_Context,
  routine_type: ^Type,
  expected_type: ^Type,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_routine := cast(^Type_Routine) routine_type;
  parameter_count := len(type_routine.parameters);
  has_params := .Has_Params in type_routine.routine_flags;
  has_c_varargs := .Has_C_Varargs in type_routine.routine_flags;

  argument_count := len(expression_call.arguments);

  params_type: ^Type;
  params_argument_count := argument_count - parameter_count + 1;
  if has_params {
    params_type = type_routine.parameters[parameter_count - 1].type;
  }

  for i := 0; i < argument_count; i += 1 {
    is_params_argument := has_params && i >= parameter_count - 1;
    is_c_varargs_argument := has_c_varargs && i >= parameter_count;

    argument_expression := expression_call.arguments[i];

    if is_params_argument {
      // We can only have an expected type when we have more than one params argument.
      // For just one argument we could have either the params slice type or its base.
      expected_type := params_argument_count > 1 ? params_type.base : nil;
      argument := resolver_resolve_expression_expected_rvalue(resolver, argument_expression, statement_context, expected_type);
      if params_argument_count == 1 {
        if argument.type != params_type {
          if !operand_convert(resolver, argument_expression.position, &argument, params_type.base) {
            report_error_fatal(
              argument_expression.position,
              "Invalid type in %v. routine params argument. Expected '%v' or '%v' got '%v'",
              i + 1,
              params_type.name,
              params_type.base.name,
              argument.type.name,
            );
          }
        }
      } else {
        if !operand_convert(resolver, argument_expression.position, &argument, params_type.base) {
          report_error_fatal(
            argument_expression.position,
            "Invalid type in %v. routine params argument. Expected '%v' got '%v'",
            i + 1,
            params_type.base.name,
            argument.type.name,
          );
        }
      }
      resolver_set_resolved_overwrite_type(resolver, argument_expression, argument.type);
    } else if is_c_varargs_argument {
      resolver_resolve_expression_rvalue(resolver, argument_expression, statement_context);
    } else {
      parameter_type := type_routine.parameters[i].type;
      argument := resolver_resolve_expression_expected_rvalue(resolver, argument_expression, statement_context, parameter_type);
      if !operand_convert(resolver, argument_expression.position, &argument, parameter_type) {
        report_error_fatal(
          argument_expression.position,
          "Invalid type in %v. routine argument. Expected '%v' got '%v'",
          i + 1,
          parameter_type.name,
          argument.type.name,
        );
      }
      resolver_set_resolved_overwrite_type(resolver, argument_expression, argument.type);
    }
  } 

  if expected_type != nil && type_is_void(type_routine.return_type) {
    report_error_fatal(expression_call.position, "Routine call does not return a value and cannot be used as a value");
  }

  return operand_rvalue(type_routine.return_type, type_is_function(routine_type), false);
}

/**
* Resolves an index expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The index expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_index :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_index := cast(^Expression_Index) expression;

  index := resolver_resolve_expression_rvalue(resolver, expression_index.index, statement_context); 
  if !type_is_scalar(index.type) && !type_is_offset_relative_pointer(index.type) {
    report_error_fatal(expression_index.index.position, "Index must be a scalar or an offset-relative pointer type. Got '%v'", index.type.name);
  }

  operand := resolver_resolve_expression_rvalue(resolver, expression_index.expression, statement_context);
  type := operand.type;
  
  // We allow automatic dereferencing of pointers but only if the index that is used is not an offset-relative pointer.
  is_using_offset_relative_pointer := type_is_offset_relative_pointer(index.type);
  if is_using_offset_relative_pointer {
    if (type_is_absolute_pointer(type) || type_is_self_relative_pointer(type)) {
      if type.base != index.type.base {
        report_error_fatal(
          expression_index.expression.position,
          "Base type offset-relative pointer does not match with pointer base. Expected '%v' got '%v'",
          type.base.name,
          index.type.base.name,
        );
      }
    } else {
      report_error_fatal(expression_index.expression.position, "Indexing with offset-relative pointer requires a pointer type. Got '%v'", type.name);
    }
  } else if type_is_absolute_pointer(type) || type_is_self_relative_pointer(type) {
    type = type.base;
  }

  if !type_is_array(type) && !type_is_dynamic_array(type) && !type_is_slice(type) && !type_is_absolute_pointer(type) && !type_is_self_relative_pointer(type) {
    if !is_using_offset_relative_pointer && (type_is_absolute_pointer(operand.type) || type_is_self_relative_pointer(operand.type)) {
      report_error_fatal(expression_index.expression.position, "Indexing of pointers must be done with an offset-relative pointer type. Got '%v'", index.type.name);
    } else {
      report_error_fatal(
        expression_index.expression.position,
        "Can only index array, dynamic array, slice, absolute pointer or self-relative pointer types. Got '%v'",
        operand.type.name,
      );
    }
  }

  // Do a constant out of bounds check for arrays.
  if type_is_array(type) && .Is_Constant in index.flags {
    constant_index := index;
    operand_cast_unchecked(expression_index.expression.position, &constant_index, resolver.storage.type_int);
    index_value := constant_index.value.(int);
    if index_value < 0 || index_value >= (cast(^Type_Array) type).number_of_elements {
      report_error_fatal(expression_index.expression.position, "Index '%v' is out of bounds for array type '%v'", index_value, operand.type.name);
    }
  }

  result_type := type.base;
  if type_is_soa_or_aosoa(type) {
    result_type = type.base.base;
  }

  return operand_lvalue(result_type, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
}

/**
* Resolves a slice expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The slice expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_slice :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_slice := cast(^Expression_Slice) expression;

  operand := resolver_resolve_expression_rvalue(resolver, expression_slice.expression, statement_context);
  type := operand.type;

  if type_is_absolute_pointer(type) || type_is_self_relative_pointer(type) {
    type = type.base;
  }

  if !type_is_string(type) && !type_is_array(type) && !type_is_dynamic_array(type) && !type_is_slice(type) {
    report_error_fatal(expression_slice.expression.position, "Can only slice strings, array, dynamic array or slice types. Got '%v'", operand.type.name);
  }

  has_constant_lower_bound := false;
  lower_bound_value := 0;
  if expression_slice.lower != nil {
    lower_bound := resolver_resolve_expression(resolver, expression_slice.lower, statement_context);
    if !type_is_integer(lower_bound.type) {
      report_error_fatal(expression_slice.lower.position, "Expected integer as lower bounds in slice expression. Got '%v'", lower_bound.type.name);
    }
    if .Is_Constant in lower_bound.flags {
      has_constant_lower_bound = true;
      operand_cast_unchecked(expression_slice.lower.position, &lower_bound, resolver.storage.type_int);
      lower_bound_value = lower_bound.value.(int);
      if lower_bound_value < 0 {
        report_error_fatal(expression_slice.lower.position, "Slice lower bound '%v' can not be negative", lower_bound_value);
      }
    }
  }
  
  has_constant_higher_bound := false;
  higher_bound_value := 0;
  if expression_slice.higher != nil {
    higher_bound := resolver_resolve_expression(resolver, expression_slice.higher, statement_context);
    if !type_is_integer(higher_bound.type) {
      report_error_fatal(expression_slice.higher.position, "Expected integer as higher bounds in slice expression. Got '%v'", higher_bound.type.name);
    }
    if .Is_Constant in higher_bound.flags {
      has_constant_higher_bound = true;
      operand_cast_unchecked(expression_slice.higher.position, &higher_bound, resolver.storage.type_int);
      higher_bound_value = higher_bound.value.(int);
      if higher_bound_value < 0 {
        report_error_fatal(expression_slice.higher.position, "Slice higher bound '%v' can not be negative", higher_bound_value);
      }
    }
  }

  if has_constant_lower_bound && has_constant_higher_bound && lower_bound_value > higher_bound_value {
    report_error_fatal(expression_slice.higher.position, "Slice lower bound '%v' is greater than higher bound '%v'", lower_bound_value, higher_bound_value);
  } 

  result_type: ^Type;
  if type_is_string(type) {
    result_type = resolver.storage.type_string;
  } else if type_is_soa_or_aosoa(type) {
    layout_modifier := Layout_Modifier.None;
    if type_is_array(type) {
      layout_modifier = (cast(^Type_Array) type).layout_info.modifier;
    } else if type_is_dynamic_array(type) {
      layout_modifier = (cast(^Type_Dynamic_Array) type).layout_info.modifier;
    } else if type_is_slice(type) {
      layout_modifier = (cast(^Type_Slice) type).layout_info.modifier;
    } else {
      assert(false);
    }

    if layout_modifier == .AoSoA {
      report_error_fatal(expression.position, "Slicing an AoSoA collection is currently not allowed");
    }

    actual_base := type.base.base;
    slice_base := resolver_resolve_type_specification_layout_struct(resolver, expression.position, .Slice, actual_base, layout_modifier, 0);
    layout_info := (cast(^Type_Struct) slice_base).layout_info;
    result_type = type_storage_get_or_make_type_slice(resolver.storage, layout_info, slice_base);
  } else {
    result_type = type_storage_get_or_make_type_slice(resolver.storage, LAYOUT_INFO_NONE, type.base);
  }
  
  return operand_lvalue(result_type, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
}

/**
* Resolves a member expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The member expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_member :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_member := cast(^Expression_Member) expression;

  position := expression.position;
  name := expression_member.name;

  // First check if we reference a package.
  package_to_look_in := resolver_resolve_expression_member_try_package(resolver, expression_member.expression);
  if package_to_look_in != nil {
    symbol := package_get_symbol(package_to_look_in, position, name, .Public_Only);
    resolver_resolve_symbol(resolver, symbol);
    operand := resolver_resolve_expression_name_operand(resolver, position, statement_context, symbol, name, {});
    resolver_set_resolved_symbol(resolver, expression, symbol);
    return operand;
  }

  new_statement_context := statement_context;
  new_statement_context.legal_statements += {.Can_Refer_To_Enum_Item};
  operand := resolver_resolve_expression(resolver, expression_member.expression, new_statement_context);
  operand_remove_untyped(resolver.storage, expression_member.expression.position, &operand);
  type := operand.type;
  resolver_complete_type(resolver, position, type);

  // We do automatic dereferencing of pointers, so make sure the type is actually complete.
  dereferencing_dynamic_pointer := false;
  if type_is_absolute_pointer(type) || type_is_self_relative_pointer(type) || type_is_layout_pointer(type) || type_is_dynamic_pointer(type) {
    dereferencing_dynamic_pointer = type_is_dynamic_pointer(type);
    base := type_is_layout_pointer(type) ? type.base.base.base : type.base;
    operand = operand_lvalue(base, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
    type = operand.type;
    resolver_complete_type(resolver, position, type);
  }

  referring_to_enum_item := type_is_enumeration(type) && .Is_Constant in operand.flags; // Feels kinda hacky that we are relying on the operand being constant.
  referring_to_struct := type_is_struct(type);

  found_method := false;
  defer if !found_method && len(expression_member.generic_types) > 0 do report_error_fatal(position, "'%v' does not have generic type arguments", name);

  if referring_to_enum_item {
    enum_item_result, found_enum_item := resolver_resolve_expression_find_enum_item(resolver, expression, type, name);
    if found_enum_item {
      return enum_item_result;
    }
  } else if referring_to_struct {
    field_result, found_field := resolver_resolve_expression_member_find_struct_field(resolver, expression, type, name, operand, nil);
    if found_field {
      return field_result;
    }
  } else if dereferencing_dynamic_pointer {
    dynamic_method_result, found_dynamic_method := resolver_resolve_expression_member_find_interface_method(resolver, expression, type, name, operand);
    if found_dynamic_method {
      return dynamic_method_result;
    }
  }

  method_result: Operand;
  method_result, found_method = resolver_resolve_expression_member_find_method(resolver, expression, type, name, operand);
  if found_method {
    return method_result;
  }

  resolver_resolve_expression_member_report_no_such_member(position, expression_member.expression, type, name);
  return {};
}

/**
* Tries to get a package from a member expression.
*
* @param resolver   The reference to the resolver.
* @param expression The member expression to try.
* @return The package that got resolved.
*/
resolver_resolve_expression_member_try_package :: proc(resolver: ^Resolver, expression: ^Expression) -> ^Package {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if expression.kind == .Name {
    expression_name := cast(^Expression_Name) expression;
    name := expression_name.name;
    if (len(expression_name.generic_types) == 0) {
      compiler := cast(^Compiler) context.user_ptr;
      symbol := resolver_resolve_name(resolver, expression.position, compiler.current_package, name);
      if symbol != nil && symbol.kind == .Package {
        return symbol.value.(^Package);
      }
    }
  }
  return nil;
}

/**
* Tries to get a package from a member expression.
*
* @param resolver    The reference to the resolver.
* @param expression  The member expression to check.
* @param struct_type The type of the struct.
* @param name        The name of the field to find.
* @param operand     The operand of the member expression.
* @param indices     The indices to fill while resolving.
* @return 1. The operand of the field; 2. True if the field could be found otherwise false.
*/
resolver_resolve_expression_member_find_struct_field :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  struct_type: ^Type,
  name: string,
  operand: Operand,
  indices: ^[dynamic]int,
) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_struct := cast(^Type_Struct) struct_type;
  for field, index in type_struct.fields {
    access_modifier := field.access_modifier;

    if field.name == name {
      symbol := struct_type.symbol;
      position := symbol.declaration == nil ? SOURCE_POSITION_BUILTIN : symbol.declaration.position;
      resolver_resolve_expression_member_access_check(
        resolver,
        expression.position,
        access_modifier,
        symbol.package_file.pack,
        position.file,
        field.name,
        symbol.name,
      );

      // We avoid allocating for simple direct access with just a single index.
      if indices == nil {
        resolver_set_resolved_member(resolver, expression, {cast(int) index, {}, field.type});
      } else {
        // It might be that we have already resolved the member when we are an argument expression inside a generic routine call.
        // When trying to infer generic type parameters we already got here.
        // This is actually true for every argument expression and currently we are sort of doing the work twice.
        // However this is the only instance where we allocate additional memory and because of that we only want to do it once. 
        _, found := resolver.output.resolved_members[expression];
        if (!found) {
          append(indices, index);
          indices_copy := make_dynamic_array_len_cap([dynamic]int, len(indices), len(indices));
          copy(indices_copy[:], indices[:]);
          resolver_set_resolved_member(resolver, expression, {-1, indices_copy, field.type});
        }
      }

      is_pure := .Is_Pure in operand.flags;
      is_immutable := .Is_Immutable in operand.flags;
      if .Is_LValue in operand.flags {
        return operand_lvalue(field.type, is_pure, is_immutable), true;
      } else {
        return operand_rvalue(field.type, is_pure, is_immutable), true;
      }
    } else if field.is_composite {
      // We don't look into embedded fields we can't access normally based on the access modifier.
      if resolver.current_routine == nil || .Routine_Instantiated not_in resolver.current_routine.flags {
         compiler := cast(^Compiler) context.user_ptr;
        if access_modifier == .Internal && struct_type.symbol.package_file.pack != compiler.current_package {
          continue;
        }
        if access_modifier == .Private && struct_type.symbol.declaration.position.file != expression.position.file {
          continue;
        }
      }

      indices := indices;
      local_indices := make_dynamic_array([dynamic]int, context.temp_allocator);
      if indices == nil {
        indices = &local_indices;
      }
      append(indices, index);
      
      field_operand, found := resolver_resolve_expression_member_find_struct_field(resolver, expression, field.type, name, operand, indices);
      if found {
        return field_operand, true;
      }

      pop(indices);
    }
  }

  return {}, false;
}

/**
* Tries to find the method of an interface.
*
* @param resolver       The reference to the resolver.
* @param expression     The member expression to check.
* @param interface_type The type of the interface.
* @param name           The name of the method to find.
* @param operand        The operand of the member expression.
* @return 1. The operand of the method; 2. True if the method could be found otherwise false.
*/
resolver_resolve_expression_member_find_interface_method :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  interface_type: ^Type,
  name: string,
  operand: Operand,
) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(type_is_interface(interface_type));

  for interface_method, index in interface_type.methods {
    if interface_method.name == name {
      resolver_set_resolved_member(resolver, expression, {cast(int) index, {}, interface_method.type});
      return operand_rvalue(interface_method.type, .Is_Pure in operand.flags || interface_method.type.kind == .Function, .Is_Immutable in operand.flags), true;
    }
  }

  return {}, false;
}

/**
* Tries to find the method of a type.
*
* @param resolver   The reference to the resolver.
* @param expression The member expression to check.
* @param type       The type to check.
* @param name       The name of the method to find.
* @param operand    The operand of the member expression.
* @return 1. The operand of the method; 2. True if the method could be found otherwise false.
*/
resolver_resolve_expression_member_find_method :: proc(resolver: ^Resolver, expression: ^Expression, type: ^Type, name: string, operand: Operand) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // First we try to find a regular method.
  method, found := resolver_resolve_expression_member_find_method_in_methods(resolver, expression, {nil, type}, type.methods, name, operand);
  if found {
    return method, true;
  }

  // Now look at all the interfaces.
  for interface_type in type.interfaces {
    method, found := resolver_resolve_expression_member_find_method_in_methods(resolver, expression, {interface_type, type}, interface_type.methods, name, operand);
    if found {
      return method, true;
    } 
  }

  return {}, false;
}

/**
* Tries to find a method in a collection.
*
* @param resolver   The reference to the resolver.
* @param expression The member expression to check.
* @param key        The key of the type that implements the methods.
* @param name       The name of the method to find.
* @param operand    The operand of the member expression.
* @return 1. The operand of the method; 2. True if the method could be found otherwise false.
*/
resolver_resolve_expression_member_find_method_in_methods :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  key: Resolved_Implementation_Key,
  methods: #soa [dynamic]Type_Method,
  name: string,
  operand: Operand,
) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for method in methods {
    if method.name == name {
      symbol_name := key.implementation_type.symbol == nil ? key.implementation_type.name : key.implementation_type.symbol.name;
      resolver_resolve_expression_member_access_check(
        resolver,
        expression.position,
        method.access_modifier,
        method.pack,
        method.file,
        method.name,
        symbol_name,
      );

      // We need to find the appropriate symbol in the corresponding implementation.
      implementations := resolver.output.resolved_implementations[key];
      for implementation in implementations {
        symbol, found := implementation.symbols[name];
        if found {
          resolver_resolve_symbol(resolver, symbol);
          resolver_set_resolved_symbol(resolver, expression, symbol);
          
          return operand_rvalue(method.type, .Is_Pure in operand.flags || method.type.kind == .Function, .Is_Immutable in operand.flags), true;
        }
      }
    }
  }

  return {}, false;
}

/**
* Does an access check for a member.
*
* @param resolver        The reference to the resolver.
* @param position        The position of the member expression.
* @param access_modifier The access modifier of the member.
* @param pack            The package the member belongs to.
* @param file            The file the member belongs to.
* @param member_name     The name of the member.
* @param symbol_name     The name of the symbol.
*/
resolver_resolve_expression_member_access_check :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  access_modifier: Access_Modifier,
  pack: ^Package,
  file: string,
  member_name: string,
  symbol_name: string,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // Check if we are allowed to access. However we exclude instantiated generic routines.
  if resolver.current_routine == nil || .Routine_Instantiated not_in resolver.current_routine.flags {
    if access_modifier == .Internal {
      compiler := cast(^Compiler) context.user_ptr;
      if pack != compiler.current_package {
        report_error_fatal(position, "Trying to access internal member '%v' of type '%v'", member_name, symbol_name);
      }
    } else if access_modifier == .Private {
      if file != position.file {
        report_error_fatal(position, "Trying to access private member '%v' of type '%v'", member_name, symbol_name);
      }
    }
  }
}

/**
* Reports that no member could be found for a member expression.
*
* @param position          The position of the member expression.
* @param member_expression The expression of the member expression.
* @param type              The type of the member expression.
* @parma name              The name of the member that could not be found.
*/
resolver_resolve_expression_member_report_no_such_member :: proc(position: Source_Position, member_expression: ^Expression, type: ^Type, name: string) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if member_expression.kind == .Name {
    type_name := (cast(^Expression_Name) member_expression).name;
    report_error_fatal(position, "'%v' of type '%v' has no member '%v'", type_name, type.name, name);
  } else if member_expression.kind == .Member {
    type_name := (cast(^Expression_Member) member_expression).name;
    report_error_fatal(position, "'%v' of type '%v' has no member '%v'", type_name, type.name, name);
  } else {
    report_error_fatal(position, "The type '%v' has no member '%v'", type.name, name);
  }
}

/**
* Resolves a compound expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The compound expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_compound :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_compound := cast(^Expression_Compound) expression;

  if (expected_type == nil || type_is_any(expected_type)) && expression_compound.type == nil {
    report_error_fatal(expression.position, "Cannot infer type of compound literal. Missing explicit type");
  }

  type: ^Type;
  if expression_compound.type != nil {
    type = resolver_resolve_type_specification(resolver, expression_compound.type);
  } else {
    type = expected_type;
  }
  type = type_unqualify(type);
  result_type := type;

  resolver_complete_type(resolver, expression.position, type);

  // We allow an empty compound literal ('{}') to be a constant for every type.
  if len(expression_compound.fields) == 0 {
    assert(!type_is_untyped(result_type))
    return operand_constant(result_type, value_default(result_type));
  }

  // For now we simply disallow initializing SoA/AoSoA types through compound expressions.
  // It could however in theory be supported as there is no technical reason which would make it impossible.
  if type_is_soa_or_aosoa(type) {
    report_error_fatal(expression.position, "Compound expression for SoA/AoSoA type '%v' is not supported", type.name);
  }

  if type_is_array(type) {
    array_type := resolver_resolve_expression_compound_array(resolver, expression_compound, statement_context, type);
    // We overwrite the result only for incomplete arrays.
    if (cast(^Type_Array) type).has_incomplete_elements {
      result_type = array_type;
    }
  } else if type_is_slice(type) {
    resolver_resolve_expression_compound_slice(resolver, expression_compound, statement_context, type);
  } else if type_is_tuple(type) {
    resolver_resolve_expression_compound_tuple(resolver, expression_compound, statement_context, type);
  } else if type_is_struct(type) {
    resolver_resolve_expression_compound_struct(resolver, expression_compound, statement_context, type);
  } else {
    report_error_fatal(expression.position, "Illegal compound literal for type '%v'", type.name)
  }

  return operand_lvalue(result_type, true, false);
}

/**
* Resolves an array compound expression.
*
* @param resolver            The reference to the resolver.
* @param expression_compound The compound expression to resolve.
* @param statement_context   The statement context to use.
* @param type                The type of the compound expression.
* @return The type of the array.
*/
resolver_resolve_expression_compound_array :: proc(
  resolver: ^Resolver,
  expression_compound: ^Expression_Compound,
  statement_context: Statement_Context,
  type: ^Type,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_array := cast(^Type_Array) type;
  has_incomplete_elements := type_array.has_incomplete_elements;
  number_of_elements := type_array.number_of_elements;

  index := 0;
  max_index := 0;
  for compound_field in expression_compound.fields {
    if compound_field.kind == .Name {
      report_error_fatal(compound_field.position, "Illegal name field initializer in compound literal");
    } else if compound_field.kind == .Index {
      operand := resolver_resolve_expression_constant(resolver, compound_field.value.(^Expression), statement_context);
      if type_is_enumeration(operand.type) || type_is_char(operand.type) {
        operand_cast(resolver, compound_field.position, &operand, resolver.storage.type_int);
      } else {
        if !operand_convert(resolver, compound_field.position, &operand, resolver.storage.type_int) {
          report_error_fatal(compound_field.position, "Index compound field initializer must have an integer type. Got '%v'", operand.type.name);
        }
      }
      index = operand.value.(int);
      if index < 0 {
        report_error_fatal(compound_field.position, "Index compound field initializer cannot be negative");
      }
      if !has_incomplete_elements && index >= number_of_elements {
        report_error_fatal(compound_field.position, "Compound index '%v' is out of bounds for array type '%v'", number_of_elements, type.name);
      }
    }

    if !has_incomplete_elements && index >= number_of_elements {
      report_error_fatal(compound_field.position, "Array compound literal out of range. Expected '%v' got '%v'", number_of_elements, len(expression_compound.fields));
    }

    resolver_resolve_compound_expression_initializer(resolver, compound_field, statement_context, type.base, index);

    max_index = max(max_index, index);
    index += 1;
  }

  if has_incomplete_elements {
    // We should never get here with an SoA/AoSoA array.
    return type_storage_get_or_make_type_array(resolver.storage, LAYOUT_INFO_NONE, type.base, max_index + 1, false);
  } else {
    return type;
  }
}

/**
* Resolves a slice compound expression.
*
* @param resolver            The reference to the resolver.
* @param expression_compound The compound expression to resolve.
* @param statement_context   The statement context to use.
* @param type                The type of the compound expression.
*/
resolver_resolve_expression_compound_slice :: proc(resolver: ^Resolver, expression_compound: ^Expression_Compound, statement_context: Statement_Context, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  index := 0;
  for compound_field in expression_compound.fields {
    if compound_field.kind == .Name {
      report_error_fatal(compound_field.position, "Illegal name field initializer in compound literal");
    } else if compound_field.kind == .Index {
      report_error_fatal(compound_field.position, "Illegal index field initializer in compound literal");
    }

    resolver_resolve_compound_expression_initializer(resolver, compound_field, statement_context, type.base, index);

    index += 1;
  }
}

/**
* Resolves a tuple compound expression.
*
* @param resolver            The reference to the resolver.
* @param expression_compound The compound expression to resolve.
* @param statement_context   The statement context to use.
* @param type                The type of the compound expression.
*/
resolver_resolve_expression_compound_tuple :: proc(resolver: ^Resolver, expression_compound: ^Expression_Compound, statement_context: Statement_Context, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_tuple := cast(^Type_Tuple) type;

  if len(expression_compound.fields) > 0 {
    if len(expression_compound.fields) != len(type_tuple.elements) {
      report_error_fatal(
        expression_compound.position,
        "Mismatched initializer expression count in compound literal. Expected '%v' got '%v'",
        len(type_tuple.elements),
        len(expression_compound.fields),
      );
    }
  }

  index := 0;
  for compound_field in expression_compound.fields {
    if compound_field.kind == .Name {
      report_error_fatal(compound_field.position, "Illegal name field initializer in compound literal");
    } else if compound_field.kind == .Index {
      report_error_fatal(compound_field.position, "Illegal index field initializer in compound literal");
    }

    element_type := type_tuple.elements[index].type;
    resolver_resolve_compound_expression_initializer(resolver, compound_field, statement_context, element_type, index);

    index += 1;
  }
}

/**
* Resolves a struct compound expression.
*
* @param resolver            The reference to the resolver.
* @param expression_compound The compound expression to resolve.
* @param statement_context   The statement context to use.
* @param type                The type of the compound expression.
*/
resolver_resolve_expression_compound_struct :: proc(
  resolver: ^Resolver,
  expression_compound: ^Expression_Compound,
  statement_context: Statement_Context,
  type: ^Type,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_struct := cast(^Type_Struct) type;

  if len(expression_compound.fields) > 0 {
    if len(expression_compound.fields) != len(type_struct.fields) {
      report_error_fatal(
        expression_compound.position,
        "Mismatched initializer expression count in compound literal. Expected '%v' got '%v'",
        len(type_struct.fields),
        len(expression_compound.fields),
      );
    }
  }

  index := 0;
  for compound_field in expression_compound.fields {
    if compound_field.kind == .Index {
      report_error_fatal(compound_field.position, "Illegal index field initializer in compound literal");
    } else if compound_field.kind == .Name {
      struct_field_name := compound_field.value.(string);
      index = type_get_struct_field_index(type, struct_field_name);
      if index < 0 {
        report_error_fatal(compound_field.position, "Referenced field '%v' in compound literal does not exist on struct of type '%v'", struct_field_name, type.name);
      }
    }

    field_type := type_struct.fields[index].type;
    resolver_resolve_compound_expression_initializer(resolver, compound_field, statement_context, field_type, index);

    index += 1;
  }
}

/**
* Resolves a compound field initializer.
*
* @param resolver          The reference to the resolver.
* @param compound_field    The compound field whose initializer to resolve.
* @param statement_context The statement context to use.
* @param field_type        The type of the compound field.
* @param index             The index of the compound field.
*/
resolver_resolve_compound_expression_initializer :: proc(
  resolver: ^Resolver,
  compound_field: Expression_Compound_Field,
  statement_context: Statement_Context,
  field_type: ^Type,
  index: int,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  compound_field_type, success := resolver_resolve_initializer_typed(resolver, statement_context, field_type, compound_field.initializer);
  if !success {
    report_error_fatal(compound_field.position, "Invalid type in compound literal. Expected '%v' got '%v'", field_type.name, compound_field_type.name);
  }
  resolver_set_resolved_compound_field(resolver, compound_field.initializer, {index, field_type});
}

/**
* Resolves a selector expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The selector expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_selector :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_selector := cast(^Expression_Selector) expression;

  operand := resolver_resolve_expression(resolver, expression_selector.expression, statement_context);
  union_type := operand.type;
  if type_is_absolute_pointer(union_type) || type_is_self_relative_pointer(union_type) {
    union_type = union_type.base;
  }
  if !type_is_union(union_type) {
    report_error_fatal(expression_selector.expression.position, "Selector expressions are only valid on union types. Got '%v'", operand.type.name);
  }
  resolver_complete_type(resolver, expression_selector.expression.position, union_type);

  selected_type := resolver_resolve_type_specification(resolver, expression_selector.type);
  resolver_complete_type(resolver, expression_selector.type.position, selected_type);

  is_type_in_union := false;
  for variant in (cast(^Type_Union) union_type).variants {
    if variant == selected_type {
      is_type_in_union = true;
      break;
    }
  }

  if !is_type_in_union {
    report_error_fatal(expression.position, "The type '%v' is not a variant of the union '%v'", selected_type.name, operand.type.name);
  }

  return operand_rvalue(selected_type, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
}

/**
* Resolves an implicit selector expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The implicit selector expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_implicit_selector :: proc(resolver: ^Resolver, expression: ^Expression, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_implicit_selector := cast(^Expression_Implicit_Selector) expression;

  if expected_type == nil {
    report_error_fatal(expression.position, "Implicit selector cannot infer type. Use explicit enum access");
  }

  if !type_is_enumeration(expected_type) {
    report_error_fatal(expression.position, "Implicit selector only works for enumerations. Got '%v'", expected_type.name);
  }

  enum_item, found_enum_item := resolver_resolve_expression_find_enum_item(resolver, expression, expected_type, expression_implicit_selector.name);
  if found_enum_item {
    return enum_item;
  } else {
    report_error_fatal(expression.position, "Enumeration type '%v' does not have an item named '%v'", expected_type.name, expression_implicit_selector.name);
    return {};
  }
}

/**
* Resolves an unary expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The unary expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_unary :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_unary := cast(^Expression_Unary) expression;

  if expression_unary.operator == .And {
    return resolver_resolve_expression_unary_address(resolver, expression_unary, statement_context, expected_type);    
  } else {
    return resolver_resolve_expression_unary_basic(resolver, expression_unary, statement_context);
  }
}

/**
* Resolves an address unary expression.
*
* @param resolver          The reference to the resolver.
* @param expression_unary  The unary expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_unary_address :: proc(
  resolver: ^Resolver,
  expression_unary: ^Expression_Unary,
  statement_context: Statement_Context,
  expected_type: ^Type,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  operand: Operand;
  if expected_type != nil && (type_is_absolute_pointer(expected_type) || type_is_self_relative_pointer(expected_type)) {
    operand = resolver_resolve_expression_expected(resolver, expression_unary.expression, statement_context, expected_type.base);
  } else {
    operand = resolver_resolve_expression(resolver, expression_unary.expression, statement_context);
  }
  if .Is_LValue not_in operand.flags {
    report_error_fatal(expression_unary.position, "Cannot take address of non-lvalue");
  }
  if .Is_Immutable in operand.flags {
    // For now we don't want to disallow taking the address of members of immutables.
    // Something like this: 'foreach (foo in foos) { p := &foo.bar; }' should work even though 'foo' itself is immutable.
    if expression_unary.expression.kind == .Name {
      report_error_fatal(expression_unary.position, "Cannot take address of immutable");
    }
  }
  if .Is_Parameter in operand.flags {
    report_error_fatal(expression_unary.position, "Cannot take address of parameter");
  }

  result_type: ^Type;
  if expression_unary.expression.kind == .Index {
    expression_index := cast(^Expression_Index) expression_unary.expression;
    index_expression_type, found := resolver.output.resolved_types[expression_index.expression];
    assert(found);
    if type_is_absolute_pointer(index_expression_type) || type_is_self_relative_pointer(index_expression_type) {
      index_expression_type = index_expression_type.base;
    }

    // We have to remember that the base of an SoA/AoSoA pointer is the actual layout collection.
    if type_is_aosoa(index_expression_type) {
      result_type = type_storage_get_or_make_type_aosoa_pointer(resolver.storage, index_expression_type);
    } else if type_is_soa(index_expression_type) {
      result_type = type_storage_get_or_make_type_soa_pointer(resolver.storage, index_expression_type);
    }
  }
  if result_type == nil {
    if expected_type != nil && type_is_self_relative_pointer(expected_type) {
      relative_base := (cast(^Type_Relative_Pointer) expected_type).relative_base;
      result_type = type_storage_get_or_make_type_self_relative_pointer(resolver.storage, operand.type, relative_base);
    } else {
      result_type = type_storage_get_or_make_type_pointer(resolver.storage, operand.type);
    }
  }

  return operand_rvalue(result_type, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
}

/**
* Resolves a basic unary expression.
*
* @param resolver          The reference to the resolver.
* @param expression_unary  The unary expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_unary_basic :: proc(resolver: ^Resolver, expression_unary: ^Expression_Unary, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  operator := expression_unary.operator;

  operand := resolver_resolve_expression(resolver, expression_unary.expression, statement_context);
  type := operand.type;

  #partial switch operator {
    case .Multiply: {
      if !type_is_absolute_pointer(type) && !type_is_self_relative_pointer(type) && !type_is_layout_pointer(type) {
        if type_is_offset_relative_pointer(type) {
          report_error_fatal(expression_unary.position, "Cannot directly dereference offset-relative pointer type. Use offset dereference expression instead");
        } else {
          report_error_fatal(expression_unary.position, "Cannot dereference non-pointer type '%v'", type.name);
        }
      }
      if type_is_raw_pointer(type) {
        report_error_fatal(expression_unary.position, "Cannot dereference rawptr");
      }

      base := type.base;
      if type_is_layout_pointer(type) {
        base = type.base.base.base;
      }
      return operand_lvalue(base, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
    }
    case .Add, .Subtract: {
      if !type_is_arithmetic(type) {
        report_error_fatal(expression_unary.position, "Can only use unary '%v' with arithmetic types", lexer_token_kind_name(operator));
      }
      return resolver_resolve_operation_unary_basic(resolver, operator, operand);
    }
    case .Negate: {
      if !type_is_integer(type) && !type_is_enumeration(type) {
        report_error_fatal(expression_unary.position, "Can only use '~' with integer or enum types");
      }
      return resolver_resolve_operation_unary_basic(resolver, operator, operand);
    }
    case .Not: {
      if !type_is_boolean(type) {
        report_error_fatal(expression_unary.position, "Can only use '!' with bool type");
      }
      return resolver_resolve_operation_unary_basic(resolver, operator, operand);
    }
  }

  assert(false);
  return {};
}

/**
* Resolves a basic unary operation.
*
* @param resolver The reference to the resolver.
* @param operator The operator of the oepration.
* @param operand  The operand of the unary expression.
* @return The operand of the operation.
*/
resolver_resolve_operation_unary_basic :: proc(resolver: ^Resolver, operator: Token_Kind, operand: Operand) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Is_Constant in operand.flags {
    return operand_constant(operand.type, resolver_evaluate_operation_unary(operator, operand.type, operand.value));
  } else {
    return operand;
  }
}

/**
* Resolves a binary expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The binary expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_binary :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_binary := cast(^Expression_Binary) expression;

  // We explicitly check for implicit selector expressions here to allow for something like: foo == .Foo.
  // And to resolve that properly we need to figure out the expected type first.
  left_is_implicit_selector := expression_binary.left.kind == .Implicit_Selector;
  right_is_implicit_selector := expression_binary.right.kind == .Implicit_Selector;

  left: Operand;
  right: Operand;
  if left_is_implicit_selector || right_is_implicit_selector {
    if left_is_implicit_selector {
      right = resolver_resolve_expression_rvalue(resolver, expression_binary.right, statement_context);
      left = resolver_resolve_expression_expected_rvalue(resolver, expression_binary.left, statement_context, right.type);
    } else {
      left = resolver_resolve_expression_rvalue(resolver, expression_binary.left, statement_context);
      right = resolver_resolve_expression_expected_rvalue(resolver, expression_binary.right, statement_context, left.type);
    }
  } else {
    left = resolver_resolve_expression_rvalue(resolver, expression_binary.left, statement_context);
    right = resolver_resolve_expression_rvalue(resolver, expression_binary.right, statement_context);
  }

  return resolver_resolve_expression_binary_operation(
    resolver,
    expression.position,
    expression_binary.operator,
    left,
    right,
    expression_binary.left,
    expression_binary.right,
  );
}

/**
* Resolves a binary expression operation.
*
* @param resolver         The reference to the resolver.
* @param position         The position of the operation.
* @param operator         The operator of the operation.
* @param left             The left operand.
* @param right            The right operand.
* @param left_expression  The left expression.
* @param right_expression The right expression.
* @return The operand of the operation.
*/
resolver_resolve_expression_binary_operation :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  operator: Token_Kind,
  left: Operand,
  right: Operand,
  left_expression: ^Expression,
  right_expression: ^Expression,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  left_type := left.type;
  right_type := right.type;

  #partial switch operator {
    case .Multiply, .Divide: {
      if !type_is_arithmetic(left_type) {
        report_error_fatal(position, "Left operand of '%v' must have an arithmetic type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
      }
      if !type_is_arithmetic(right_type) {
        report_error_fatal(position, "Right operand of '%v' must have an arithmetic type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
      }
      return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
    }
    case .Modulo: {
      if !type_is_integer(left_type) {
        report_error_fatal(position, "Left operand of '%' must have an integer type. Got '%v'", left_type.name);
      }
      if !type_is_integer(right_type) {
        report_error_fatal(position, "Right operand of '%' must have an integer type. Got '%v'", right_type.name);
      }
      return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
    }
    case .Add: {
      is_doing_pointer_arithmetic := type_is_absolute_pointer(left_type) || type_is_absolute_pointer(right_type);
      if is_doing_pointer_arithmetic {
        return resolver_resolve_operation_binary_pointer_arithmetic(resolver, position, operator, left, right);
      } else {
        if !type_is_arithmetic(left_type) {
          report_error_fatal(position, "Left operand of '+' must have an arithmetic type. Got '%v'", left_type.name);
        }
        if !type_is_arithmetic(right_type) {
          report_error_fatal(position, "Right operand of '+' must have an arithmetic type. Got '%v'", right_type.name);
        }
        return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
      }
    }
    case .Subtract: {
      is_doing_pointer_arithmetic := type_is_absolute_pointer(left_type) || type_is_absolute_pointer(right_type);
      if is_doing_pointer_arithmetic {
        return resolver_resolve_operation_binary_pointer_arithmetic(resolver, position, operator, left, right);
      } else {
        if !type_is_arithmetic(left_type) {
          report_error_fatal(position, "Left operand of '%v' must have an arithmetic type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
        }
        if !type_is_arithmetic(right_type) {
          report_error_fatal(position, "Right operand of '%v' must have an arithmetic type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
        }
        return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
      }
    }

    case .Left_Shift, .Right_Shift: {
      if !type_is_integer(left_type) {
        report_error_fatal(position, "Left operand of '%v' must have an integer type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
      }
      if !type_is_unsigned(right_type) && right_type.kind != .Untyped_Integer {
        report_error_fatal(position, "Right operand of '%v' must have an unsigned integer type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
      }

      if .Is_Constant in left.flags && .Is_Constant in right.flags {
        return operand_constant(left_type, resolver_evaluate_operation_binary_shift(operator, left, right));
      } else {
        return operand_rvalue(left_type, .Is_Pure in left.flags && .Is_Pure in right.flags, .Is_Immutable in left.flags && .Is_Immutable in right.flags);
      }
    }

    case .And, .Or, .Xor: {
      if !type_is_integer(left_type) && !type_is_enumeration(left_type) {
        report_error_fatal(position, "Left operand of '%v' must have an integer or enum type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
      }
      if !type_is_integer(right_type) && !type_is_enumeration(right_type) {
        report_error_fatal(position, "Right operand of '%v' must have an integer or enum type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
      }
      return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
    }

    case .Equal, .Not_Equal: {
      if !type_is_scalar(left_type) &&
        !type_is_string_like(left_type) &&
        !type_is_absolute_or_relative_pointer_like(left_type) &&
        !type_is_dynamic_pointer(left_type) &&
        !type_is_union(left_type) &&
        !type_is_typeid(left_type) {
        report_error_fatal(
          position,
          "Left operand of '%v' must have a scalar, string, pointer, union or typeid type. Got '%v'",
          lexer_token_kind_name(operator),
          left_type.name,
        );
      }
      if !type_is_scalar(right_type) &&
        !type_is_string_like(right_type) &&
        !type_is_absolute_or_relative_pointer_like(right_type) &&
        !type_is_dynamic_pointer(left_type) &&
        !type_is_union(right_type) &&
        !type_is_typeid(right_type) {
        report_error_fatal(
          position,
          "Right operand of '%v' must have a scalar, string, pointer, union or typeid type. Got '%v'",
          lexer_token_kind_name(operator),
          right_type.name,
        );
      }

      result := resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
      operand_cast_unchecked(position, &result, resolver.storage.type_bool);
      return result;
    }

    case .Less_Than, .Less_Than_Equal, .Greater_Than, .Greater_Than_Equal: {
      if !type_is_scalar(left_type) && !type_is_absolute_or_relative_pointer_like(left_type) {
        report_error_fatal(position, "Left operand of '%v' must have a scalar or pointer type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
      }
      if !type_is_scalar(right_type) && !type_is_absolute_or_relative_pointer_like(right_type) {
        report_error_fatal(position, "Right operand of '%v' must have a scalar or pointer type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
      }

      result := resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
      operand_cast_unchecked(position, &result, resolver.storage.type_bool);
      return result;
    }

    case .And_And, .Or_Or: {
      if !type_is_boolean(left_type) {
        report_error_fatal(position, "Left operand of '%v' must have a boolean type. Got '%v'", lexer_token_kind_name(operator), left_type.name);
      }
      if !type_is_boolean(right_type) {
        report_error_fatal(position, "Right operand of '%v' must have a boolean type. Got '%v'", lexer_token_kind_name(operator), right_type.name);
      }
      return resolver_resolve_operation_binary(resolver, position, operator, left, right, left_expression, right_expression);
    }
  }

  assert(false);
  return {};
}

/**
* Resolves a binary operation.
*
* @param resolver         The reference to the resolver.
* @param position         The position of the operation.
* @param operator         The operator of the operation.
* @param left             The left operand.
* @param right            The right operand.
* @param left_expression  The left expression.
* @param right_expression The right expression.
* @return The operand of the operation.
*/
resolver_resolve_operation_binary :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  operator: Token_Kind,
  left: Operand,
  right: Operand,
  left_expression: ^Expression,
  right_expression: ^Expression,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  left := left;
  right := right;

  left_type := left.type;
  right_type := right.type;

  // First coerce types to be properly compatible.
  type: ^Type = left.type;
  if type_is_untyped(left_type) && !type_is_untyped(right_type) {
    // We try to coerce the untyped left into the typed right.
    if !operand_convert(resolver, position, &left, right.type) {
      report_error_fatal(position, "Operands of '%v' must have the same type. Got '%v' and '%v'", lexer_token_kind_name(operator), left.type.name, right.type.name);
    }
    type = right.type;
  } else if !type_is_untyped(left_type) && type_is_untyped(right_type) {
    // We try to coerce the untyped right into the typed left.
    if !operand_convert(resolver, position, &right, left.type) {
      report_error_fatal(position, "Operands of '%v' must have the same type. Got '%v' and '%v'", lexer_token_kind_name(operator), left.type.name, right.type.name);
    }
  } else if !type_is_untyped(left_type) && !type_is_untyped(right_type) {
    if (type_is_union(left_type)) {
      report_error_fatal(position, "Can only compare unions with 'null'");
    }
    // If both are not untyped, we try to coerce the right into the left type.
    if !operand_convert(resolver, position, &right, left.type) {
      report_error_fatal(position, "Operands of '%v' must have the same type. Got '%v' and '%v'", lexer_token_kind_name(operator), left.type.name, right.type.name);
    }
  } else if type_is_untyped_null(left_type) && type_is_untyped_null(right_type) {
    report_error_fatal(position, "Operands of '%v' cannot both be 'null'", lexer_token_kind_name(operator));
  }

  resolver_set_resolved_type(resolver, left_expression, left.type);
  resolver_set_resolved_type(resolver, right_expression, right.type);

  result_type := type;
  if operator >= .First_Comparison && operator <= .Last_Comparison {
    // The result of a comparison is always a boolean.
    result_type = resolver.storage.type_bool;
  }

  if .Is_Constant in right.flags && (operator == .Divide || operator == .Modulo) {
    resolver_resolve_expression_binary_check_for_zero(resolver, position, right);
  }

  if .Is_Constant in left.flags && .Is_Constant in right.flags {
    return operand_constant(result_type, resolver_evaluate_operation_binary(operator, type, left.value, right.value));
  } else {
    return operand_rvalue(result_type, .Is_Pure in left.flags && .Is_Pure in right.flags, .Is_Immutable in left.flags && .Is_Immutable in right.flags);
  }
}

/**
* Resolves an arithmetic binary operation.
*
* @param resolver The reference to the resolver.
* @param position The position of the operation.
* @param operator The operator of the operation.
* @param left     The left operand.
* @param right    The right operand.
* @return The operand of the operation.
*/
resolver_resolve_operation_binary_pointer_arithmetic :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  operator: Token_Kind,
  left: Operand,
  right: Operand,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  left_type := left.type;
  right_type := right.type;

  #partial switch operator {
    case .Add: {
      if type_is_absolute_pointer(left_type) {
        if !type_is_offset_relative_pointer(right_type) {
          report_error_fatal(position, "Right operand of '+' must be an offset-relative pointer. Got '%v'", right_type.name);
        } else if right_type.base != left_type.base {
          report_error_fatal(
            position,
            "Right operand of '+' must be an offset-relative pointer with the same base. Expected '%v' as base got '%v'",
            left_type.base.name,
            right_type.base.name,
          );
        } else {
          return operand_rvalue(left_type, .Is_Pure in left.flags && .Is_Pure in right.flags, .Is_Immutable in left.flags && .Is_Immutable in right.flags);
        }
      } else {
        if !type_is_offset_relative_pointer(left_type) {
          report_error_fatal(position, "Left operand of '+' must be an offset-relative pointer. Got '%v'", left_type.name);
        } else if left_type.base != right_type.base {
          report_error_fatal(
            position,
            "Left operand of '+' must be an offset-relative pointer with the same base. Expected '%v' as base got '%v'",
            right_type.base.name,
            left_type.base.name,
          );
        } else {
          return operand_rvalue(right_type, .Is_Pure in left.flags && .Is_Pure in right.flags, .Is_Immutable in left.flags && .Is_Immutable in right.flags);
        }
      }
    }
    case .Subtract: {
      if !type_is_absolute_pointer(left_type) {
        report_error_fatal(position, "Left operand of '-' must be an absolute pointer. Got '%v'", left_type.name);
      } else if !type_is_absolute_pointer(right_type) {
        report_error_fatal(position, "Right operand of '-' must be an absolute pointer. Got '%v'", left_type.name);
      }
      offset_relative_pointer_type := type_storage_get_or_make_type_offset_relative_pointer(resolver.storage, left_type.base, resolver.storage.type_int);
      return operand_rvalue(
        offset_relative_pointer_type,
        .Is_Pure in left.flags && .Is_Pure in right.flags, .Is_Immutable in left.flags && .Is_Immutable in right.flags,
      );
    }
  }

  assert(false);
  return {};
}

/**
* Checks for zero in an operand.
*
* @param resolver The reference to the resolver.
* @param position The position of the operand expression.
* @param operand  The operand to check.
*/
resolver_resolve_expression_binary_check_for_zero :: proc(resolver: ^Resolver, position: Source_Position, operand: Operand) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Is_Constant not_in operand.flags {
    return;
  }

  if type_is_integer(operand.type) {
    value: u64;
    #partial switch operand.type.kind {
      case .I8:              value = cast(u64) operand.value.(i8);
      case .I16:             value = cast(u64) operand.value.(i16);
      case .I32:             value = cast(u64) operand.value.(i32);
      case .I64:             value = cast(u64) operand.value.(i64);
      case .Int:             value = cast(u64) operand.value.(int);
      case .U8:              value = cast(u64) operand.value.(u8);
      case .U16:             value = cast(u64) operand.value.(u16);
      case .U32:             value = cast(u64) operand.value.(u32);
      case .U64:             value = cast(u64) operand.value.(u64);
      case .UInt:            value = cast(u64) operand.value.(uint);
      case .Untyped_Integer: value = cast(u64) operand.value.(int);
      case: assert(false);
    }

    if value == 0 {
      report_error_fatal(position, "Division by zero is not allowed");
    }
  } else if type_is_float(operand.type) {
    value: f64;
    #partial switch operand.type.kind {
      case .F32:           value = cast(f64) operand.value.(f32);
      case .F64:           value = cast(f64) operand.value.(f64);
      case .Untyped_Float: value = cast(f64) operand.value.(f64);
      case: assert(false);
    }

    if value == 0.0 {
      report_error_fatal(position, "Division by zero is not allowed");
    }
  } else {
    assert(false);
  }
}

/**
* Resolves a ternary expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The ternary expression to resolve.
* @param statement_context The statement context to use.
* @param expected_type     The type to expect.
* @return The operand of the expression.
*/
resolver_resolve_expression_ternary :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context, expected_type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_ternary := cast(^Expression_Ternary) expression;

  condition := resolver_resolve_expression_rvalue(resolver, expression_ternary.condition, statement_context);
  if !type_is_boolean(condition.type) {
    report_error_fatal(expression_ternary.condition.position, "Ternary condition must be a boolean. Got '%v'", condition.type.name);
  }

  then_operand := resolver_resolve_expression_expected_rvalue(resolver, expression_ternary.then_expression, statement_context, expected_type);
  else_operand := resolver_resolve_expression_expected_rvalue(resolver, expression_ternary.else_expression, statement_context, expected_type);
  
  then_untyped := type_is_untyped(then_operand.type);
  else_untyped := type_is_untyped(else_operand.type);
  both_untyped := then_untyped && else_untyped;
  
  if !both_untyped {
    if then_untyped {
      if !operand_convert(resolver, expression.position, &then_operand, else_operand.type) {
        report_error_fatal(expression.position, "Incompatible types in ternary expression. '%v' and '%v'", else_operand.type.name, then_operand.type.name);
      }
    } else {
      if !operand_convert(resolver, expression.position, &else_operand, then_operand.type) {
        report_error_fatal(expression.position, "Incompatible types in ternary expression. '%v' and '%v'", then_operand.type.name, else_operand.type.name);
      }
    }
  } else {
    operand_remove_untyped(resolver.storage, expression_ternary.then_expression.position, &then_operand);
    operand_remove_untyped(resolver.storage, expression_ternary.else_expression.position, &else_operand);
  }

  if then_operand.type == else_operand.type {
    if expected_type == nil && type_is_untyped_null(then_operand.type) {
      report_error_fatal(expression.position, "Cannot determine type from 'untyped null' in ternary expression");  
    }

    result_type := both_untyped && expected_type != nil ? expected_type : then_operand.type;
    if .Is_Constant in condition.flags && .Is_Constant in then_operand.flags && .Is_Constant in else_operand.flags {
      return operand_constant(result_type, condition.value.(bool) ? then_operand.value : else_operand.value);
    } else {
      return operand_rvalue(result_type, .Is_Pure in then_operand.flags, .Is_Immutable in then_operand.flags);
    }
  } else if type_is_absolute_pointer_like(then_operand.type) && type_is_untyped_null(else_operand.type) { 
    return operand_rvalue(then_operand.type, .Is_Pure in then_operand.flags, .Is_Immutable in then_operand.flags);
  } else if type_is_untyped_null(then_operand.type) && type_is_absolute_pointer_like(else_operand.type) { 
    return operand_rvalue(else_operand.type, .Is_Pure in else_operand.flags, .Is_Immutable in else_operand.flags);
  } else {
    report_error_fatal(expression.position, "Incompatible types in ternary expression. '%v' and '%v'", then_operand.type.name, else_operand.type.name);
    return {};
  }
}

/**
* Resolves a modify expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The modify expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_modify :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_modify := cast(^Expression_Modify) expression;

  operand := resolver_resolve_expression(resolver, expression_modify.expression, statement_context);
  type := operand.type;
  resolver_complete_type(resolver, expression.position, type);

  if .Is_LValue not_in operand.flags {
    report_error_fatal(expression.position, "Cannot modify non-lvalue");
  }
  if .Is_Parameter in operand.flags {
    report_error_fatal(expression.position, "Cannot modify parameter");
  }
  if .Non_Modifiable in type.flags {
    report_error_fatal(expression.position, "Cannot modify non-modifiable type");
  }
  if !type_is_integer(type) {
    report_error_fatal(expression.position, "'%s' is only valid for integer types", lexer_token_kind_name(expression_modify.operator));
  }

  return operand_rvalue(type, .Is_Pure in operand.flags, .Is_Immutable in operand.flags);
}

/**
* Resolves a query expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The query expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_query :: proc(resolver: ^Resolver, expression: ^Expression, statement_context: Statement_Context) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_query := cast(^Expression_Query) expression;

  result: Operand;
  constant: Operand;
  switch expression_query.query_kind {
    case .Size_Of_Expression: {
      resolved_expression := resolver_resolve_expression_query_resolve_expression(resolver, expression_query, statement_context);
      result = resolver_resolve_expression_query_size_of_type(resolver, expression.position, resolved_expression.type);
      constant = result;
    }
    case .Size_Of_Type: {
      type := resolver_resolve_type_specification(resolver, expression_query.value.(^Type_Specification));
      result = resolver_resolve_expression_query_size_of_type(resolver, expression.position, type);
      constant = result;
    }
    case .Typeid_Of_Expression: {
      resolved_expression := resolver_resolve_expression_query_resolve_expression(resolver, expression_query, statement_context);
      // The 'typeid_of' expression is special for 'any' and union has it does not produce the typeid of 'any'/union but actually the 'typeid' they contain.
      if type_is_any(resolved_expression.type) || type_is_union(resolved_expression.type) {
        result = operand_rvalue(resolver.storage.type_typeid, .Is_Pure in resolved_expression.flags, .Is_Immutable in resolved_expression.flags);
      } else {
        resolver_mark_type_for_rttr_generation(resolver, expression_query.value.(^Expression).position, resolved_expression.type);
        result = operand_constant(resolver.storage.type_typeid, resolved_expression.type.id);
        constant = result;
      }
    }
    case .Typeid_Of_Type: {
      type_specification := expression_query.value.(^Type_Specification);
      type := resolver_resolve_type_specification(resolver, type_specification)
      resolver_mark_type_for_rttr_generation(resolver, type_specification.position, type);
      result = operand_constant(resolver.storage.type_typeid, type.id);
      constant = result;
    }
    case .Type_Info_Of_Expression: {
      resolved_expression := resolver_resolve_expression_query_resolve_expression(resolver, expression_query, statement_context);
      position := expression_query.value.(^Expression).position;
      if !operand_convert(resolver, position, &resolved_expression, resolver.storage.type_typeid) {
        report_error_fatal(position, "Expected 'typeid' in 'type_info_of' expression. Got '%v'", resolved_expression.type.name);
      }
      result = operand_rvalue(type_storage_get_or_make_type_pointer(resolver.storage, resolver.storage.cached_runtime_types.type_info), true, false);
    }
    case .Type_Info_Of_Type: {
      type_specification := expression_query.value.(^Type_Specification);
      type := resolver_resolve_type_specification(resolver, type_specification)
      resolver_mark_type_for_rttr_generation(resolver, type_specification.position, type);
      result = operand_rvalue(type_storage_get_or_make_type_pointer(resolver.storage, resolver.storage.cached_runtime_types.type_info), true, false);
      constant = operand_constant(resolver.storage.type_typeid, type.id);
    }
    case: assert(false);
  }

  resolver_set_resolved_constant(resolver, expression, constant);
  return result;
}

/**
* Resolves an expression of a query expression.
*
* @param resolver          The reference to the resolver.
* @param expression_query  The query expression whose expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_query_resolve_expression :: proc(
  resolver: ^Resolver,
  expression_query: ^Expression_Query,
  statement_context: Statement_Context,
) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression := expression_query.value.(^Expression);
  resolved_expression := resolver_resolve_expression(resolver, expression, statement_context);
  operand_remove_untyped(resolver.storage, expression.position, &resolved_expression);
  return resolved_expression;
}

/**
* Resolves the size of a query expression.
*
* @param resolver The reference to the resolver.
* @param position The position of the query expression.
* @param type     The type whose size to get.
* @return The operand of the expression.
*/
resolver_resolve_expression_query_size_of_type :: proc(resolver: ^Resolver, position: Source_Position, type: ^Type) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_complete_type(resolver, position, type);
  
  // The 'size_of' expression always returns a 'uint'.
  return operand_constant(resolver.storage.type_uint, cast(uint) type_size_of(type));
}

/**
* Resolves a directive expression.
*
* @param resolver          The reference to the resolver.
* @param expression        The directive expression to resolve.
* @param statement_context The statement context to use.
* @return The operand of the expression.
*/
resolver_resolve_expression_directive :: proc(resolver: ^Resolver, expression: ^Expression) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression_directive := cast(^Expression_Directive) expression;
  
  // The actual constant values of these expressions are created by the generator.
  switch expression_directive.directive_kind {
    case .Line: return operand_constant(resolver.storage.type_int, cast(int) expression.position.line);
    case .Routine: return operand_constant(resolver.storage.type_string, resolver.current_routine.name);
    case .File: return operand_constant(resolver.storage.type_string, expression.position.file);
    case .Location: return operand_constant(resolver.storage.cached_runtime_types.source_location, nil);
    case: assert(false);
  }

  assert(false);
  return {};
}

/**
* Finds an item in an enum.
*
* @param resolver   The reference to the resolver.
* @param expression The name expression.
* @param enum_type  The type of the enum.
* @param name       The name of the enum item.
* @return The operand of the expression.
*/
resolver_resolve_expression_find_enum_item :: proc(resolver: ^Resolver, expression: ^Expression, enum_type: ^Type, name: string) -> (Operand, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_enumeration := cast(^Type_Enumeration) enum_type;
  for item in type_enumeration.items {
    if item.name == name {
      operand := operand_constant(enum_type, item.value);
      resolver_set_resolved_constant(resolver, expression, operand)
      return operand, true;
    }
  }

  return {}, false;
}
