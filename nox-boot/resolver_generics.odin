package nox

import "core:strings"
import "tracy"

/**
* Represents a generic contxt.
*/
Generic_Context :: struct {
  type_names: [dynamic]string,   // The names of the generic type parameters.
  generic_types: [dynamic]^Type, // The types to instantiate for the generic type parameters.
}

/**
* Represents an instantiated generic routine.
*/
Instantiated_Generic_Routine :: struct {
  generic_types: [dynamic]^Type, // The generic types used to instantiate the routine.
  symbol: ^Symbol,               // The symbol of the instantiated routine.
}

/**
* Represents a nested index for resolving generic type parameters.
*/
Nested_Index :: struct {
  kind: Type_Specification_Kind, // The kind of the type specification that got resolved.
  index: int,                    // The resolved nested index (semantics depend on the type specification kind).
}

/**
* Represents a generic type parameter to infer.
*/
Parameter_To_Infer :: struct {
  argument_index: int,                  // The index of the argument that gets used to infer the parameter.
  generic_type_index: int,              // The index of the generic type that needs to be inferred.

  // This dynamic array tells us how deep we need to look in a type to find the actual generic type parameter.
  // This is necessary to automatically infer the generic type parameter in complex scenarios.
  // For example: proc foo!(T1, T2)(f: Foo!(Foo!(T1)), i: []*T2).
  // Here T1 and T2 are nested inside other type specifications and the indices tell us where to look.
  // That means the actual semantic of an index depends on the type we currently look in.
  // To properly check we are looking into the correct type the nested index stores the kind of the type specification it came from with it.
  nested_indices: [dynamic]Nested_Index, // The indices that are needed to resolve nested types.
}

/**
* Gets the generic type corresponding to a generic type parameter name.
* 
* @param generic_context The generic context to look in.
* @param name            The name of the generic type parameter.
* @return The generic type.
*/
resolver_resolve_generic_type_by_name :: proc(generic_context: Generic_Context, name: string) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  for generic_type_name, i in generic_context.type_names {
    if name == generic_type_name {
      return generic_context.generic_types[i];
    }
  }

  return nil;
}

/**
* Resolves a generic type specification name.
*
* @param resolver           The reference to the resolver.
* @param symbol             The generic symbol.
* @param type_specification The generic type specification to resolve.
* @param generic_contex     The generic context to use.
* @return The resolved generic type.
*/
resolver_resolve_generic_type_specification_name :: proc(
  resolver: ^Resolver,
  symbol: ^Symbol,
  type_specification: ^Type_Specification,
  generic_context: Generic_Context,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  type_specification_name := cast(^Type_Specification_Name) type_specification;

  declaration := symbol.declaration;
  generic_type_names: [dynamic]string;
  if declaration.kind == .Struct {
    generic_type_names = (cast(^Declaration_Struct) declaration).generic_type_names;
  } else if declaration.kind == .Type_Alias {
    generic_type_names = (cast(^Declaration_Type_Alias) declaration).generic_type_names;
  }
  
  if len(generic_type_names) != len(type_specification_name.generic_types) {
    report_error_fatal(
      type_specification.position,
      "Non-matching number of generic type parameters. Expected %v got %v",
      len(generic_type_names),
      len(type_specification_name.generic_types),
    );
  }

  is_unininstantiated_generic := false;
  generic_types := make_dynamic_array_len_cap([dynamic]^Type, 0, len(generic_type_names), context.temp_allocator);
  for generic_type_spec in type_specification_name.generic_types {
    generic_type := resolver_resolve_type_specification(resolver, generic_type_spec, generic_context);
    if .Uninstantiated_Generic in generic_type.flags {
      is_unininstantiated_generic = true;
    }
    append(&generic_types, generic_type);
  }

  if declaration.kind == .Struct {
    instantiated_type := type_storage_get_cached_type_instantiated(resolver.storage, symbol, generic_types);
    if instantiated_type != nil {
      return instantiated_type;
    }

    // We need to copy the temporarily allocated generic types to a permament array that the instantiated type can hold.
    instantiated_generic_types := make_dynamic_array_len_cap([dynamic]^Type, len(generic_types), cap(generic_types));
    copy(instantiated_generic_types[:], generic_types[:]);

    instantiated_type = type_storage_make_type_instantiated(resolver.storage, symbol, instantiated_generic_types);

    instantiated_symbol := symbol_make(resolver, symbol.kind, .Public, symbol.package_file, instantiated_type.name, declaration);
    instantiated_symbol.state = .Resolved;
    instantiated_symbol.flags += {.Reachable};
    if is_unininstantiated_generic {
      instantiated_symbol.flags += {.Uninstantiated_Generic};
    }
    instantiated_type.symbol = instantiated_symbol;
    instantiated_symbol.type = instantiated_type;

    return instantiated_type;
  } else if declaration.kind == .Type_Alias {
    aliased_type := (cast(^Declaration_Type_Alias) declaration).type;

    // It is very important that we resolve the instantiated type alias with the correct package set.
    // Meaning the package in which the uninstantiated symbol was declared in.
    // Otherwise we will fail to resolve the correct types.
    previous_package := package_enter(symbol.package_file.pack);
    new_generic_context: Generic_Context;
    new_generic_context.generic_types = generic_types;
    new_generic_context.type_names = generic_type_names;
    type := resolver_resolve_type_specification(resolver, aliased_type, new_generic_context);
    package_leave(previous_package);

    return type;
  } else {
    assert(false);
    return nil;
  }
}

/**
* Resolves a generic expression call.
*
* @param resolver               The reference to the resolver.
* @param expression             The generic expression call.
* @param statement_context      The statement context to use.
* @param routine_type           The type of the uninstantiated routine.
* @param uninstantiated_routine The symbol of the uninstantiated routine.
* @return The resolved generic routine.
*/
resolver_resolve_generic_expression_call :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  statement_context: Statement_Context,
  routine_type: ^Type,
  uninstantiated_routine: ^Symbol,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  // For generic routines, We need to:
  //   1. Try to infer generic types if none are provided.
  //   2. Resolve the generic types that are used in the function call.
  //   3. Check if we have already instantiated the generic routine with the same generic type combination and if so use this cached version.
  //   4. If no cached version is available create a new symbol to hold the instantiated routine.
  //   5. Clone the routine declaration of the uninstantiated symbol.
  //   6. Actually instantiate the routine by replacing all generic type names in the cloned declaration with the actual type specification being used.
  //   7. For all cloned generic type specifications we need to set the overwrite type so that symbols we referred to from different packages are handled correctly.
  //   8. Validate the generic types againts the provided constraints (can't be done sooner as they need to be instantiated themselves).
  //   9. Lastly we need to overwrite the resolved symbol for the expression we are trying to call.

  call_expression := (cast(^Expression_Call) expression).expression;
  if call_expression.kind != .Name && call_expression.kind != .Member {
    report_error_fatal(call_expression.position, "Can only call generic procedures or functions by name");
  }

  declaration := uninstantiated_routine.declaration;
  declaration_routine := cast(^Declaration_Routine) declaration;
  generic_type_names := declaration_routine.generic_type_names;
  
  generic_type_specifications: [dynamic]^Type_Specification;
  if call_expression.kind == .Name {
    generic_type_specifications = (cast(^Expression_Name) call_expression).generic_types;
  } else {
    generic_type_specifications = (cast(^Expression_Member) call_expression).generic_types;
  }

  should_infer_generic_types := len(generic_type_specifications) == 0;
  generic_types: [dynamic]^Type;
  if should_infer_generic_types {
    resolver_infer_generic_types_from_arguments(resolver, expression, statement_context, routine_type, declaration, &generic_types);
  } else {
    if len(generic_type_names) != len(generic_type_specifications) {
      report_error_fatal(
        call_expression.position,
        "Generic routine call with invalid type arguments. Expected %v got %v",
        len(generic_type_names),
        len(generic_type_specifications),
      );
    }

    for generic_type_spec in generic_type_specifications {
      // Nested generics should not need to be handled here.
      // The instantiation of generics already properly resolves all calls to nested generic routines.
      generic_type := resolver_resolve_type_specification(resolver, generic_type_spec);
      append(&generic_types, generic_type);
    }
  }

  // At this point we should have a proper mapping between generic type names and their corresponding resolved types.
  assert(len(generic_type_names) == len(generic_types));
  
  instantiated_routine := resolver_get_instantiated_routine(resolver, uninstantiated_routine, generic_types);
  uninstantiated_package_file := uninstantiated_routine.package_file;
  needs_to_instantiate_routine := instantiated_routine == nil;
  
  // Only instantiate the generic routine if we haven't already done so with the same generic type combination.
  if needs_to_instantiate_routine {
    instantiated_routine = resolver_make_instantiated_routine_symbol(resolver, uninstantiated_routine, generic_types);

    // We allocate the instantiated AST in the package file of the uninstantiated symbol.
    ast_allocator := uninstantiated_package_file.ast_allocator;

    instantiated_declaration := ast_declaration_clone_routine(declaration, ast_allocator);
    
    generic_type_specifications_to_instantiate := generic_type_specifications;
    if should_infer_generic_types {
      // We need to create type specifications for our inferred types.
      // For that we have to remember to use the AST allocator from the uninstanted symbol.
      context.allocator = ast_allocator;
      for generic_type in generic_types {
        generic_type_specification := resolver_make_type_specification_from_type(resolver, generic_type, expression.position);
        append(&generic_type_specifications_to_instantiate, generic_type_specification);
      }
    } else {
      assert(len(generic_types) == len(generic_type_specifications_to_instantiate));
      // This is necessary so that symbols in different packages do not need to be resolved from inside the package the instantiated routine lives in.
      for generic_type_specification, i in generic_type_specifications_to_instantiate {
        resolver_set_overwrite_type_for_generic_type_specification(generic_type_specification, generic_types[i]);
      }
    }

    instantiation_context := Instantiation_Context{generic_type_names, generic_type_specifications_to_instantiate};
    ast_declaration_routine_instantiate(instantiated_declaration, instantiation_context, ast_allocator);
    if should_infer_generic_types {
      // No destroying of 'generic_type_specifications' necessary here because we have a dedicated AST allocator.
      delete(generic_type_specifications_to_instantiate);
    }

    instantiated_routine.declaration = instantiated_declaration; 
    append(&instantiated_routine.package_file.declarations, instantiated_declaration);

    instantiated_generic_routines := resolver.instantiated_generic_routines[uninstantiated_routine];
    append(&instantiated_generic_routines, Instantiated_Generic_Routine{generic_types, instantiated_routine});
    resolver.instantiated_generic_routines[uninstantiated_routine] = instantiated_generic_routines;

    resolver_resolve_symbol(resolver, instantiated_routine); 
  }

  resolver_check_generic_constraints(resolver, expression.position, instantiated_routine.declaration, uninstantiated_package_file, generic_type_names, generic_types);

  if !needs_to_instantiate_routine {
    // We have to check for cyclic dependencies here as this might happen in case we are using the generic routine as a default parameter initializer.
    if instantiated_routine.state == .Resolving {
      report_error_fatal(instantiated_routine.declaration.position, "Cyclic dependency of symbol: '%v'", instantiated_routine.name);
    }
    delete(generic_types);
  }

  // We update the expression so that it refers to the instantiated symbol instead of the uninstantiated one.
  resolver_set_resolved_symbol(resolver, call_expression, instantiated_routine);
  resolver_set_resolved_type(resolver, call_expression, instantiated_routine.type);

  return instantiated_routine.type;
}

/**
* Infers generic type parameters for a generic routine from provided arguments.
*
* @param resolver          The reference to the resolver.
* @param expression        The generic expression call.
* @param statement_context The statement context to use.
* @param routine_type      The type of the uninstantiated routine.
* @param declaration       The declaration of the uninstantiated routine.
* @param generic_types     The generic types that will be inferred and filled in.
*/
resolver_infer_generic_types_from_arguments :: proc(
  resolver: ^Resolver,
  expression: ^Expression,
  statement_context: Statement_Context,
  routine_type: ^Type,
  declaration: ^Declaration,
  generic_types: ^[dynamic]^Type,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  expression_call := cast(^Expression_Call) expression;
  
  declaration_routine := cast(^Declaration_Routine) declaration;
  type_routine := cast(^Type_Routine) routine_type;

  parameters_to_infer := make_dynamic_array([dynamic]Parameter_To_Infer, context.temp_allocator);
  
  // We need to figure out which paramaters refer to a generic type parameter.
  nested_indices := make_dynamic_array([dynamic]Nested_Index, context.temp_allocator);
  for parameter, parameter_index in declaration_routine.parameters {
    // We only have to look at parameters which are not yet instantiated.
    if .Uninstantiated_Generic in type_routine.parameters[parameter_index].type.flags {
      resolver_try_infer_type_for_parameter(
        resolver,
        parameter.type,
        parameter_index,
        declaration_routine.generic_type_names,
        &parameters_to_infer,
        &nested_indices,
        .None,
        -1,
      );
    }
  }

  if len(parameters_to_infer) == 0 || len(parameters_to_infer) != len(declaration_routine.generic_type_names) {
    report_error_fatal(expression.position, "Cannot automatically infer generic type parameters");
  }

  has_params := .Has_Params in type_routine.routine_flags;
  params_index := len(type_routine.parameters) - 1;
  params_arguments_count := len(expression_call.arguments) - params_index;

  // We can't infer the generic type parameter for the params parameter if we don't get an argument for it.
  if has_params && len(expression_call.arguments) <= params_index {
    report_error_fatal(expression.position, "Cannot automatically infer generic type parameters");
  }

  resize(generic_types, len(parameters_to_infer));
  for parameter_to_infer in parameters_to_infer {
    parameter := declaration_routine.parameters[parameter_to_infer.argument_index];
    is_default_parameter := parameter.initializer != nil;

    generic_type: ^Type;
    if is_default_parameter {
      initializer := resolver_resolve_expression(resolver, parameter.initializer, statement_context);
      operand_remove_untyped(resolver.storage, parameter.position, &initializer);
      generic_type = initializer.type;
    } else {
      if parameter_to_infer.argument_index >= len(expression_call.arguments) {
        expected_parameter_count := len(declaration_routine.parameters);
        for parameter in declaration_routine.parameters {
          if parameter.initializer != nil {
            expected_parameter_count -= 1;
          }
        }
        if has_params do expected_parameter_count -= 1;
        report_error_fatal(expression.position, "Routine call with too few arguments. Expected %v got %v", expected_parameter_count, len(expression_call.arguments));
      }

      argument := expression_call.arguments[parameter_to_infer.argument_index];
      argument_operand := resolver_resolve_expression(resolver, argument, statement_context);
      operand_remove_untyped(resolver.storage, argument.position, &argument_operand);
      argument_type := argument_operand.type;
      
      // We have to explicitly handle 'null' as that is the only untyped type which we can never infer.
      if type_is_untyped_null(argument_type) {
        report_error_fatal(argument.position, "Cannot automatically infer generic type parameter from 'null'");
      }

      nested_indices := parameter_to_infer.nested_indices;

      // We have to check the params type explicitly.
      if has_params && parameter_to_infer.argument_index == params_index {
        assert(len(nested_indices) >= 1);
        if params_arguments_count == 1 {
          if type_is_slice(argument_type) {
            generic_type = argument_type.base;
          } else {
            generic_type = argument_type;
          }
        } else {
          generic_type = argument_type;
        }
      } else {
        generic_type = resolver_try_get_nested_generic_type(argument_type, &nested_indices, len(nested_indices));
        if generic_type == nil {
          parameter_type := type_routine.parameters[parameter_to_infer.argument_index].type;
          report_error_fatal(
            argument.position,
            "Invalid type in %v. routine call argument. Expected '%v' got '%v'",
            parameter_to_infer.argument_index + 1,
            parameter_type.name,
            argument_type.name,
          );
        }
      }
    }
    
    generic_types[parameter_to_infer.generic_type_index] = generic_type;

    delete(parameter_to_infer.nested_indices);
  }
}

/**
* Recursively tries to infer the (possibly nested) generic type parameter.
*
* @param resolver            The reference to the resolver.
* @param type_spec           The type specification of the parameter to infer.
* @param parameter_index     The index of the parameter.
* @param generic_type_names  The names of the generic type parameters.
* @param parameters_to_infer The parameters to infer that will get filled in.
* @param nested_indices      The nested indices that will be filled while trying to infer.
* @param nested_kind         The kind of the current nested type.
* @param nested_index        The index of the current nested type (semantics depend on the kind).
*/
resolver_try_infer_type_for_parameter :: proc(
  resolver: ^Resolver,
  type_spec: ^Type_Specification,
  parameter_index: int,
  generic_type_names: [dynamic]string,
  parameters_to_infer: ^[dynamic]Parameter_To_Infer,
  nested_indices: ^[dynamic]Nested_Index,
  nested_kind: Type_Specification_Kind,
  nested_index: int,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  append(nested_indices, Nested_Index{nested_kind, nested_index});
  defer pop(nested_indices);

  kind := type_spec.kind;
  switch kind {
    case .Name: {
      type_specification_name := cast(^Type_Specification_Name) type_spec;
      for generic_type_name, generic_type_name_index in generic_type_names {
        if generic_type_name == type_specification_name.name {
          // We do not want to infer types multiple times.
          for parameter_to_infer in parameters_to_infer {
            if parameter_to_infer.generic_type_index == generic_type_name_index {
              return;
            }
          }

          // We skip the first index when copying as it contains the '-1' index from the first recursive call which we do not want.
          nested_indices_copy := make_dynamic_array_len([dynamic]Nested_Index, len(nested_indices) - 1);
          copy(nested_indices_copy[:], nested_indices[1:]);
          append(parameters_to_infer, Parameter_To_Infer{parameter_index, generic_type_name_index, nested_indices_copy});
          return;
        }

        // The generic type may be nested inside.
        for generic_type, generic_index in type_specification_name.generic_types {
          resolver_try_infer_type_for_parameter(resolver, generic_type, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, generic_index);
        }
      }
    }
    case .Pointer: {
      type_specification_pointer := cast(^Type_Specification_Pointer) type_spec;
      base := type_specification_pointer.base;
      resolver_try_infer_type_for_parameter(resolver, base, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 0);

      relative_base := type_specification_pointer.relative_base;
      if relative_base != nil {
        resolver_try_infer_type_for_parameter(resolver, relative_base, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 1);
      }
    }
    case .Array: {
      type_specification_array := cast(^Type_Specification_Array) type_spec;
      base := type_specification_array.base;
      resolver_try_infer_type_for_parameter(resolver, base, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 0);
    }
    case .Slice: {
      type_specification_slice := cast(^Type_Specification_Slice) type_spec;
      base := type_specification_slice.base;
      resolver_try_infer_type_for_parameter(resolver, base, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 0);
    }
    case .Map: {
      type_specification_map := cast(^Type_Specification_Map) type_spec;
      key := type_specification_map.key;
      resolver_try_infer_type_for_parameter(resolver, key, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 0);
      value := type_specification_map.value;
      resolver_try_infer_type_for_parameter(resolver, value, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, 1);
    }
    case .Tuple: {
      type_specification_tuple := cast(^Type_Specification_Tuple) type_spec;
      for element, element_index in type_specification_tuple.elements {
        resolver_try_infer_type_for_parameter(resolver, element, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, element_index);
      }
    }

    case .Procedure, .Function: {
      type_specification_routine := cast(^Type_Specification_Routine) type_spec;
      parameter_count := len(type_specification_routine.parameters)
      for parameter, p_index in type_specification_routine.parameters {
        resolver_try_infer_type_for_parameter(resolver, parameter, parameter_index, generic_type_names, parameters_to_infer, nested_indices, kind, p_index);
      }

      if type_specification_routine.return_type != nil {
        resolver_try_infer_type_for_parameter(
          resolver,
          type_specification_routine.return_type,
          parameter_index,
          generic_type_names,
          parameters_to_infer,
          nested_indices,
          kind,
          parameter_count,
        );
      }
    }
    
    case .None: fallthrough;
    case: assert(false);
  }
}

/**
* Recursively tries to get the nested generic type from a provided argument.
*
* @param type           The type of the provided argument.
* @param nested_indices The nested indices to use.
* @param nested_depth   The current nested depth.
* @return The nested generic type (or null if none can be found).
*/
resolver_try_get_nested_generic_type :: proc(type: ^Type, nested_indices: ^[dynamic]Nested_Index, nested_depth: int) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  if nested_depth == 0 {
    return type;
  }

  // The nested indices have been added as a stack, so we know we can always look at the front when removing it at the same time.
  nested_index := nested_indices[len(nested_indices) - nested_depth];

  nested_type: ^Type;
  #partial switch type.kind {
    case .Pointer: {
      if nested_index.kind != .Pointer do return nil;
      nested_type = type.base;
    }
    case .Array, .Dynamic_Array, .Slice: {
      if type.kind == .Slice {
        if nested_index.kind != .Slice do return nil;
      } else {
        if nested_index.kind != .Array do return nil;
      }
      
      if type_is_soa_or_aosoa(type) {
        nested_type = type.base.base;
      } else {
        nested_type = type.base;
      }
    }
    case .Self_Relative_Pointer, .Offset_Relative_Pointer: {
      if nested_index.kind != .Pointer do return nil;
      type_relative_pointer := cast(^Type_Relative_Pointer) type;
      nested_type = nested_index.index == 0 ? type.base : type_relative_pointer.relative_base;
    }
    case .Map: {
      if nested_index.kind != .Map do return nil;
      type_map := cast(^Type_Map) type;
      nested_type = nested_index.index == 0 ? type_map.key : type_map.value;
    }
    case .Tuple: {
      if nested_index.kind != .Tuple do return nil;
      type_tuple := cast(^Type_Tuple) type;
      nested_type = type_tuple.elements[nested_index.index].type;
    }
    
    case .Struct: {
      if nested_index.kind != .Name do return nil;
      type_struct := cast(^Type_Struct) type;
      if nested_index.index > len(type_struct.generic_types) - 1 do return nil;
      nested_type = type_struct.generic_types[nested_index.index];
    }

    case .Procedure, .Function: {
      if type.kind == .Procedure {
        if nested_index.kind != .Procedure do return nil;
      } else {
        if nested_index.kind != .Function do return nil;
      }
      type_routine := cast(^Type_Routine) type;
      parameter_count := len(type_routine.parameters);
      if nested_index.index == parameter_count {
        nested_type = type_routine.return_type;
      } else {
        nested_type = type_routine.parameters[nested_index.index].type;
      }
    }
  }

  if nested_type == nil {
    // If we get here we know that the type passed can't possibly be the required parameter type.
    // That is, we have run out of nested types to look in.
    return nil;
  } else {
    return resolver_try_get_nested_generic_type(nested_type, nested_indices, nested_depth - 1);
  }
}

/**
* Tries to get the instantiated routine for an uninstantiated routine based on given generic type parameters.
*
* @param resolver              The reference to the resolver.
* @param uninstantiated_symbol The uninstantiated symbol
* @param generic_types         The generic types of the instantiated version.
*/
resolver_get_instantiated_routine :: proc(resolver: ^Resolver, uninstantiated_symbol: ^Symbol, generic_types: [dynamic]^Type) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  instantiated_generic_routines, found := resolver.instantiated_generic_routines[uninstantiated_symbol];
  if found {
    outer: for instantiated_generic_routine in instantiated_generic_routines {
      assert(len(instantiated_generic_routine.generic_types) == len(generic_types));
      for generic_type, i in instantiated_generic_routine.generic_types {
        if generic_type != generic_types[i] {
          continue outer;
        }
      }
      return instantiated_generic_routine.symbol;
    }
  }
  return nil;
}

/**
* Makes a symbol for an instantiated routine.
*
* @param resolver              The reference to the resolver.
* @param uninstantiated_symbol The uninstantiated symbol
* @param generic_types         The generic types of the instantiated version.
* @return The instantiated routine symbol.
*/
resolver_make_instantiated_routine_symbol :: proc(resolver: ^Resolver, uninstantiated_routine: ^Symbol, generic_types: [dynamic]^Type) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  builder := strings.builder_make(resolver.name_allocator);
  strings.write_string(&builder, uninstantiated_routine.name);
  strings.write_string(&builder, "!(");
  for generic_type, i in generic_types {
    strings.write_string(&builder, generic_type.name);
    if i < len(generic_types) - 1 {
      strings.write_string(&builder, ", ");
    }
  }
  strings.write_string(&builder, ")");
  instantiated_symbol_name := strings.to_string(builder);

  instantiated_symbol := symbol_make(
    resolver,
    uninstantiated_routine.kind,
    .Public,
    uninstantiated_routine.package_file,
    instantiated_symbol_name,
    uninstantiated_routine.declaration,
  );

  // We need to copy over all relevant flags and the value as that is needed when the generic is a method.
  instantiated_symbol.flags = uninstantiated_routine.flags;
  instantiated_symbol.flags -= {.Uninstantiated_Generic};
  instantiated_symbol.flags -= {.Reachable};
  instantiated_symbol.flags += {.Routine_Instantiated};
  instantiated_symbol.value = uninstantiated_routine.value;

  return instantiated_symbol;
}

/**
* Checks for duplicate generic type names.
*
* @param position   The position of the generic type names.
* @parma type_names The generic type names to check.
*/
resolver_check_for_duplicate_generic_type_names :: proc(position: Source_Position, type_names: [dynamic]string) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);

  for i := 0; i < len(type_names) - 1; i += 1 {
    type_name_a := type_names[i];
    type_name_b := type_names[i + 1];
    if type_name_a == type_name_b {
      report_error_fatal(position, "Duplicate generic type parameter '%v'", type_name_a);
    }
  }
}

/**
* Makes a type specification from a type.
*
* @param resolver The reference to the resolver.
* @param type     The type to get the specification of.
* @param position The generic types of the instantiated version.
* @return The specification corresponding to the type.
*/
resolver_make_type_specification_from_type :: proc(resolver: ^Resolver, type: ^Type, position: Source_Position) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);
  
  switch type.kind {
    case .B8, .B16, .B32, .B64, .Bool: fallthrough;
    case .I8, .I16, .I32, .I64, .Int, .U8, .U16, .U32, .U64, .UInt: fallthrough;
    case .Char, .F32, .F64, .Enumeration: fallthrough;
    case .String, .CString, .Rawptr, .Constant, .Struct, .Union, .Any, .Typeid, .Interface: {
      type_name := type.name;
      // The type named might contain nested generic type parameters which we do not care about.
      // We only want the name without all generics.
      name_index_generic_parameters := strings.index(type_name, "!");
      if name_index_generic_parameters != -1 {
        type_name = type_name[:name_index_generic_parameters];
      }

      type_specification := ast_type_specification_make_name(position, "", type_name, nil);
      type_specification_name := cast(^Type_Specification_Name) type_specification;

      // This is necessary so that symbols in different packages do not need to be resolved from inside the package the instantiated routine lives in.
      type_specification_name.overwrite_type = type;
      
      generic_types: [dynamic]^Type_Specification;
      if type_is_struct(type) {
        type_struct := cast(^Type_Struct) type;
        for generic_type in type_struct.generic_types {
          append(&generic_types, resolver_make_type_specification_from_type(resolver, generic_type, position));
        }
      }
      type_specification_name.generic_types = generic_types;
      
      return type_specification;
    }

    case .Pointer, .Self_Relative_Pointer, .Offset_Relative_Pointer, .SoA_Layout_Pointer, .AoSoA_Layout_Pointer, .Dynamic_Pointer: {
      base := resolver_make_type_specification_from_type(resolver, type.base, position);
      
      pointer_kind: Type_Specification_Pointer_Kind;
      #partial switch type.kind {
        case .Pointer:                 pointer_kind = .Absolute;
        case .Self_Relative_Pointer:   pointer_kind = .Self_Relative;
        case .Offset_Relative_Pointer: pointer_kind = .Offset_Relative;
        case .SoA_Layout_Pointer:      pointer_kind = .SoA_Layout;
        case .AoSoA_Layout_Pointer:    pointer_kind = .AoSoA_Layout;
        case .Dynamic_Pointer:         pointer_kind = .Dynamic;
      }
      
      relative_base: ^Type_Specification = nil;
      if type.kind == .Self_Relative_Pointer || type.kind == .Offset_Relative_Pointer {
        relative_base = resolver_make_type_specification_from_type(resolver, (cast(^Type_Relative_Pointer) type).relative_base, position);
      }
      return ast_type_specification_make_pointer(position, pointer_kind, base, relative_base);
    }

    case .Array: {
      type_array := cast(^Type_Array) type;
      base_type := type_array.layout_info.modifier == .None ? type.base : type.base.base;
      base := resolver_make_type_specification_from_type(resolver, base_type, position);
      size_expression: ^Expression;
      if !type_array.has_incomplete_elements {
        size_expression = ast_expression_make_integer(position, cast(u64) type_array.number_of_elements);
      }
      return ast_type_specification_make_array(position, .Fixed, type_array.layout_info.modifier, base, size_expression);
    }
    case .Dynamic_Array: {
      type_dynamic_array := cast(^Type_Dynamic_Array) type;
      base_type := type_dynamic_array.layout_info.modifier == .None ? type.base : type.base.base;
      base := resolver_make_type_specification_from_type(resolver, base_type, position);
      return ast_type_specification_make_array(position, .Dynamic, type_dynamic_array.layout_info.modifier, base, nil);
    }
    case .Slice: {
      type_slice := cast(^Type_Slice) type;
      base_type := type_slice.layout_info.modifier == .None ? type.base : type.base.base;
      base := resolver_make_type_specification_from_type(resolver, base_type, position);
      return ast_type_specification_make_slice(position, type_slice.layout_info.modifier, base);
    }
    case .Map: {
      type_map := cast(^Type_Map) type;
      key := resolver_make_type_specification_from_type(resolver, type_map.key, position);
      value := resolver_make_type_specification_from_type(resolver, type_map.value, position);
      return ast_type_specification_make_map(position, key, value);
    }
    case .Tuple: {
      type_tuple := cast(^Type_Tuple) type;
      elements: [dynamic]^Type_Specification;
      for element in type_tuple.elements {
        append(&elements, resolver_make_type_specification_from_type(resolver, element.type, position));
      }
      return ast_type_specification_make_tuple(position, elements);
    }

    case .Procedure, .Function: {
      type_routine := cast(^Type_Routine) type;
      parameters: [dynamic]^Type_Specification;
      for parameter in type_routine.parameters {
        append(&parameters, resolver_make_type_specification_from_type(resolver, parameter.type, position));
      }
      return_type: ^Type_Specification;
      if !type_is_void(type_routine.return_type) {
        return_type = resolver_make_type_specification_from_type(resolver, type_routine.return_type, position);
      }
      kind: Type_Specification_Kind = type.kind == .Procedure ? .Procedure : .Function;
      return ast_type_specification_make_routine(
        kind,
        position,
        type_routine.calling_convention,
        parameters,
        .Has_Params in type_routine.routine_flags,
        .Has_C_Varargs in type_routine.routine_flags,
        return_type,
        .Is_Pure_Function in type_routine.routine_flags,
      );
    }

    case .None, .Void, .Generic: fallthrough;
    case .Untyped_Boolean, .Untyped_Integer, .Untyped_Float, .Untyped_Char, .Untyped_String, .Untyped_Null: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Sets the overwritten type for a generic type specification.
*
* @param type_specification The type specification to set the overwritten type of.
* @param type               The overwritten type for the type specification.
*/
resolver_set_overwrite_type_for_generic_type_specification :: proc(type_specification: ^Type_Specification, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);
  
  switch type_specification.kind {
    case .Name: {
      type_specification_name := cast(^Type_Specification_Name) type_specification;

      // We never set the overwrite type more than once (This might happen in nested scenarios).
      if type_specification_name.overwrite_type != nil {
        break;
      }

      type_specification_name.overwrite_type = type;
    }
    case .Pointer: {
      type_specification_pointer := cast(^Type_Specification_Pointer) type_specification;
      resolver_set_overwrite_type_for_generic_type_specification(type_specification_pointer.base, type.base);
      pointer_kind := type_specification_pointer.pointer_kind;
      if pointer_kind == .Self_Relative || pointer_kind == .Offset_Relative && type_specification_pointer.relative_base != nil {
        resolver_set_overwrite_type_for_generic_type_specification(type_specification_pointer.relative_base, (cast(^Type_Relative_Pointer) type).relative_base);
      }
    }
    case .Array: {
      resolver_set_overwrite_type_for_generic_type_specification((cast(^Type_Specification_Array) type_specification).base, type.base);
    }
    case .Slice: {
      resolver_set_overwrite_type_for_generic_type_specification((cast(^Type_Specification_Slice) type_specification).base, type.base);
    }
    case .Map: {
      type_specification_map := cast(^Type_Specification_Map) type_specification;
      type_map := cast(^Type_Map) type;
      resolver_set_overwrite_type_for_generic_type_specification(type_specification_map.key, type_map.key);
      resolver_set_overwrite_type_for_generic_type_specification(type_specification_map.value, type_map.value);
    }
    case .Tuple: {
      type_tuple := cast(^Type_Tuple) type;
      type_specification_tuple := cast(^Type_Specification_Tuple) type_specification;
      for element, i in type_specification_tuple.elements {
        resolver_set_overwrite_type_for_generic_type_specification(element, type_tuple.elements[i].type);
      }
    }
    case .Procedure, .Function: {
      type_routine := cast(^Type_Routine) type;
      type_specification_routine := cast(^Type_Specification_Routine) type_specification;
      for parameter, i in type_specification_routine.parameters {
        resolver_set_overwrite_type_for_generic_type_specification(parameter, type_routine.parameters[i].type);
      }
      resolver_set_overwrite_type_for_generic_type_specification(type_specification_routine.return_type, type_routine.return_type);
    }

    case .None: fallthrough;
    case: assert(false);
  }
}

/**
* Resolves the constraints for a generic routine.
*
* @param resolver                    The reference to the resolver.
* @param position                    The position of the generic constraints.
* @param instantiated_declaration    The declaration of the instantiated routine.
* @param uninstantiated_package_file The package file of the uninstantiated routine.
* @param generic_type_names          The generic type names of the generic routine.
* @param generic_types               The instantiated generic types of the generic routine.
*/
resolver_check_generic_constraints :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  instantiated_declaration: ^Declaration,
  uninstantiated_package_file: ^Package_File,
  generic_type_names: [dynamic]string,
  generic_types: [dynamic]^Type,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER_GENERIC);
  
  instantiated_declaration_routine := cast(^Declaration_Routine) instantiated_declaration;

  // Now that we have properly resolved the generic types we need to validate them againts the provided constraints.
  for constraint, i in instantiated_declaration_routine.generic_constraints {
    fulfills_constraint := false;

    switch constraint.kind {
      case .Expression: {
        constraint_expression := constraint.value.(Declaration_Routine_Generic_Constraint_Expression);
        
        // We have to remeber that we are currently inside the block of some routine.
        // That means we first have to enter the correct package in which the uninstantiated routine resides.
        // Additionally the constraint should not have access to the current local scope.
        previous_local_scope := resolver.local_scope;
        resolver.local_scope = nil;
        previous_package := package_enter(uninstantiated_package_file.pack);
        operand := resolver_resolve_expression_constant(resolver, constraint_expression.expression, STATEMENT_CONTEXT);
        package_leave(previous_package);
        resolver.local_scope = previous_local_scope;

        if !operand_convert(resolver, constraint_expression.expression.position, &operand, resolver.storage.type_bool) {
          report_error_fatal(constraint_expression.expression.position, "Generic expression constraint must be a boolean type");
        }
        fulfills_constraint = operand.value.(bool);
      }
      case .Interface: {
        constraint_interface := constraint.value.(Declaration_Routine_Generic_Constraint_Interface);

        generic_type_index := -1;
        for generic_type_name, i in generic_type_names {
          if generic_type_name == constraint_interface.generic_type_name {
            generic_type_index = i;
          }
        }

        if generic_type_index == -1 {
          report_error_fatal(constraint.position, "Interface constraint references undeclared generic type name '%v'", constraint_interface.generic_type_name);
        }

        generic_type := generic_types[generic_type_index];

        interface_type := resolver_resolve_type_specification(resolver, constraint_interface.type);
        // For now we just allow interfaces as type constraints.
        // Anything else does not really makes sense right now.
        if !type_is_interface(interface_type) {
          report_error_fatal(constraint_interface.type.position, "Type constraint must be an interface. Got '%v'", interface_type.name);
        }

        for interface in generic_type.interfaces {
          if interface_type == interface {
            fulfills_constraint = true;
            break;
          }
        }
      }
      case: assert(false);
    }

    if !fulfills_constraint {
      report_error_fatal(position, "The %v. generic constraint of routine '%v' ist not fullfilled", i + 1, instantiated_declaration.name);
    }
  }
}
