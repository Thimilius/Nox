package nox

import "tracy"

/**
* Resolves a type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved type.
*/
resolver_resolve_type_specification :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context = {}) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type: ^Type = nil;
  switch type_specification.kind {
    case .Name: type = resolver_resolve_type_specification_name(resolver, type_specification, generic_context);
    case .Pointer: type = resolver_resolve_type_specification_pointer(resolver, type_specification, generic_context);
    case .Array: type = resolver_resolve_type_specification_array(resolver, type_specification, generic_context);
    case .Slice: type = resolver_resolve_type_specification_slice(resolver, type_specification, generic_context);
    case .Map: type = resolver_resolve_type_specification_map(resolver, type_specification, generic_context);
    case .Tuple: type = resolver_resolve_type_specification_tuple(resolver, type_specification, generic_context);
    case .Procedure, .Function: type = resolver_resolve_type_specification_routine(resolver, type_specification, type_specification.kind, generic_context);

    case .None: fallthrough;
    case: assert(false);
  }

  resolver_set_resolved_type(resolver, type_specification, type);

  return type;
}

/**
* Resolves a name type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The name type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved name type.
*/
resolver_resolve_type_specification_name :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_name := cast(^Type_Specification_Name) type_specification;
  package_name := type_specification_name.package_name;
  name := type_specification_name.name;

  refers_to_package := package_name != "";

  if !refers_to_package {
    generic_type := resolver_resolve_generic_type_by_name(generic_context, name);
    if generic_type != nil {
      return generic_type;
    }
  }

  // We might have an overwrite type set when we are coming from an instantiated routine.
  // In that case we got already resolved and can just return the overwritten type.
  if type_specification_name.overwrite_type != nil {
    return type_specification_name.overwrite_type;
  }

  // First resolve the package we might be referring to.
  compiler := cast(^Compiler) context.user_ptr;
  package_to_look_in := compiler.current_package;
  if refers_to_package {
    package_symbol := package_get_symbol(package_to_look_in, type_specification.position, package_name, .All);
    if package_symbol.kind != .Package {
      report_error_fatal(type_specification.position, "'%v' must reference a package", package_name);
    }
    package_to_look_in = package_symbol.value.(^Package);
  }  

  symbol := package_get_symbol(package_to_look_in, type_specification.position, name, refers_to_package ? .Public_Only : .All);
  if symbol.kind != .Type {
    report_error_fatal(type_specification.position, "'%v' is not a type", name);
  }

  resolver_resolve_symbol(resolver, symbol);
  
  result_type := symbol.type;
  if .Uninstantiated_Generic in symbol.flags {
    result_type = resolver_resolve_generic_type_specification_name(resolver, symbol, type_specification, generic_context);
  } else if len(type_specification_name.generic_types) > 0 {
    report_error_fatal(type_specification.position, "Type '%v' has no generic type parameters", result_type.name);
  }

  return result_type;
}

/**
* Resolves a pointer type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The pointer type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved pointer type.
*/
resolver_resolve_type_specification_pointer :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_pointer := cast(^Type_Specification_Pointer) type_specification;

  base := resolver_resolve_type_specification(resolver, type_specification_pointer.base, generic_context);

  switch type_specification_pointer.pointer_kind {
    case .Absolute: {
      if .Not_Allowed_As_Pointer in base.flags {
        report_error_fatal(type_specification_pointer.base.position, "Type '%v' is not allowed to be used as a pointer base", base.name);
      }
      return type_storage_get_or_make_type_pointer(resolver.storage, base);
    }
    case .Self_Relative, .Offset_Relative: {
      if .Not_Allowed_As_Pointer in base.flags {
        report_error_fatal(type_specification_pointer.base.position, "Type '%v' is not allowed to be used as a pointer base", base.name);
      }

      relative_base := type_specification_pointer.pointer_kind == .Self_Relative ? resolver.storage.type_i32 : resolver.storage.type_int;
      if type_specification_pointer.relative_base != nil {
        relative_base = resolver_resolve_type_specification(resolver, type_specification_pointer.relative_base, generic_context);
      }

      if !type_is_integer(relative_base) {
        report_error_fatal(
          type_specification_pointer.relative_base.position,
          "Relative base type for relative pointer must be an integer. Got '%v'",
          relative_base.name,
        );
      }

      if type_specification_pointer.pointer_kind == .Self_Relative {
        return type_storage_get_or_make_type_self_relative_pointer(resolver.storage, base, relative_base);
      } else {
        return type_storage_get_or_make_type_offset_relative_pointer(resolver.storage, base, relative_base);
      }
    }
    case .SoA_Layout, .AoSoA_Layout: {
      if .Not_Allowed_As_Pointer in base.flags {
        report_error_fatal(type_specification_pointer.base.position, "Type '%v' is not allowed to be used as a pointer base", base.name);
      }
      
      resolver_complete_type(resolver, type_specification_pointer.base.position, base);
      if type_specification_pointer.pointer_kind == .SoA_Layout {
        if !type_is_soa(base) {
          report_error_fatal(type_specification_pointer.base.position, "Base type of an SoA pointer must be an SoA collection. Got '%v'", base.name);
        }
        return type_storage_get_or_make_type_soa_pointer(resolver.storage, base);
      } else {
        if !type_is_aosoa(base) {
          report_error_fatal(type_specification_pointer.base.position, "Base type of an AoSoA pointer must be an AoSoA collection. Got '%v'", base.name);
        }
        return type_storage_get_or_make_type_aosoa_pointer(resolver.storage, base);
      }
    }
    case .Dynamic: {
      if !type_is_interface(base) {
        report_error_fatal(type_specification_pointer.base.position, "Dynamic pointer must have an interface as its base. Got '%v'", base.name);
      }

      // A dynamic pointer has a special effect on the reachability of symbols as we can't determine at compiletime which routines might get called.
      // Because of that we set all implemented routine symbols for all types that implement the interface as reachable.
      if .Interface_Used_In_Dynamic_Pointer not_in base.flags {
        types_implementing_interface := resolver.types_implementing_interface[base];
        for type in types_implementing_interface {
          key := Resolved_Implementation_Key{base, type};
          resolved_implementations, found := resolver.output.resolved_implementations[key];
          assert(len(resolved_implementations) == 1);
          resolved_implementation := resolved_implementations[0];
          for _, symbol in resolved_implementation.symbols {
            resolver_resolve_symbol(resolver, symbol);
          }
        }
      }
      return type_storage_get_or_make_type_dynamic_pointer(resolver.storage, base);
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves an array type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The array type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved array type.
*/
resolver_resolve_type_specification_array :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_array := cast(^Type_Specification_Array) type_specification;

  base := resolver_resolve_type_specification(resolver, type_specification_array.base, generic_context);
  if .Not_Allowed_As_Base in base.flags {
    report_error_fatal(type_specification_array.base.position, "Type '%v' is not allowed to be used as an array base", base.name);
  }

  array_kind := type_specification_array.array_kind;
  layout_modifier := type_specification_array.layout_modifier;

  // For regular dynamic arrays we don't need to complete the type (as the necessary info required is handled by the library implementation).
  if layout_modifier != .None || array_kind == .Fixed {
    resolver_complete_type(resolver, type_specification_array.base.position, base);
  }
  
  if layout_modifier != .None && !type_is_struct(base) && !type_is_generic(base) {
    report_error_fatal(type_specification_array.base.position, "Base type of an SoA/AoSoA array must be a struct. Got '%v'", base.name);
  }

  number_of_elements := 0;
  has_incomplete_elements := type_specification_array.size == nil;
  if array_kind == .Fixed {
    if has_incomplete_elements && layout_modifier != .None {
      report_error_fatal(type_specification.position, "Incomplete soa array is not allowed");
    } else if !has_incomplete_elements {
      operand := resolver_resolve_expression_constant(resolver, type_specification_array.size, STATEMENT_CONTEXT);
      if (!operand_cast(resolver, type_specification_array.size.position, &operand, resolver.storage.type_int)) {
        report_error_fatal(type_specification.position, "Array size constant expression must be castable to an integer");
      }
      number_of_elements = operand.value.(int);
      if number_of_elements < 0 {
        report_error_fatal(type_specification_array.size.position, "Array size constant must be positive");
      }
    }
  }

  if !type_is_generic(base) && layout_modifier != .None {
    collection_type := array_kind == .Fixed ? Collection_Type.Array : Collection_Type.Dynamic_Array;
    base = resolver_resolve_type_specification_layout_struct(
      resolver,
      type_specification_array.base.position,
      collection_type,
      base,
      layout_modifier,
      number_of_elements,
    );
  }

  layout_info := Layout_Info{layout_modifier, 0};

  if array_kind == .Fixed {
    return type_storage_get_or_make_type_array(resolver.storage, layout_info, base, number_of_elements, has_incomplete_elements);
  } else {
    assert(array_kind == .Dynamic);
    return type_storage_get_or_make_type_dynamic_array(resolver.storage, layout_info, base);
  }
}

/**
* Resolves a slice type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The slice type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved slice type.
*/
resolver_resolve_type_specification_slice :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_slice := cast(^Type_Specification_Slice) type_specification;

  base := resolver_resolve_type_specification(resolver, type_specification_slice.base, generic_context);
  if .Not_Allowed_As_Base in base.flags {
    report_error_fatal(type_specification_slice.base.position, "Type '%v' is not allowed to be used as a slice base", base.name);
  }
  
  layout_modifier := type_specification_slice.layout_modifier;
  layout_info: Layout_Info;
  if layout_modifier == .SoA {
    resolver_complete_type(resolver, type_specification_slice.base.position, base);

    if !type_is_struct(base) && !type_is_generic(base) {
      report_error_fatal(type_specification_slice.base.position, "Base type of an SoA/AoSoA slice must be a struct. Got '%v'", base.name);
    }

    if !type_is_generic(base) {
      base = resolver_resolve_type_specification_layout_struct(resolver, type_specification_slice.base.position, .Slice, base, layout_modifier, 0);
      layout_info = (cast(^Type_Struct) base).layout_info;
    }
  } else if layout_modifier == .AoSoA {
    report_error_fatal(type_specification.position, "AoSoA slices are currently not supported");
  }

  return type_storage_get_or_make_type_slice(resolver.storage, layout_info, base);
}

/**
* Resolves a map type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The map type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved map type.
*/
resolver_resolve_type_specification_map :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_map := cast(^Type_Specification_Map) type_specification;

  key := resolver_resolve_type_specification(resolver, type_specification_map.key, generic_context);
  resolver_complete_type(resolver, type_specification_map.key.position, key);
  if .Not_Allowed_As_Base in key.flags || (!type_is_generic(key) && (.Has_Hash_Function not_in key.flags || .Has_Compare_Function not_in key.flags)) {
    report_error_fatal(type_specification_map.key.position, "Type '%v' is not allowed to be used as a map key", key.name);
  }

  value := resolver_resolve_type_specification(resolver, type_specification_map.value, generic_context);
  resolver_complete_type(resolver, type_specification_map.value.position, value);
  if .Not_Allowed_As_Base in value.flags {
    report_error_fatal(type_specification_map.value.position, "Type '%v' is not allowed to be used as a map value", value.name);
  }

  return type_storage_get_or_make_type_map(resolver.storage, key, value);
}

/**
* Resolves a tuple type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The tuple type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved tuple type.
*/
resolver_resolve_type_specification_tuple :: proc(resolver: ^Resolver, type_specification: ^Type_Specification, generic_context: Generic_Context) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_tuple := cast(^Type_Specification_Tuple) type_specification;

  elements: #soa [dynamic]Type_Tuple_Element;
  for element in type_specification_tuple.elements {
    element_type := resolver_resolve_type_specification(resolver, element, generic_context);
    if .Not_Allowed_As_Tuple in element_type.flags {
      report_error_fatal(element.position, "Type '%v' is not allowed to be part of a tuple", element_type.name);
    }
    resolver_complete_type(resolver, element.position, element_type);
    append_soa(&elements, Type_Tuple_Element{element_type, 0})
  }

  return type_storage_get_or_make_type_tuple(resolver.storage, elements);
}

/**
* Resolves a routine type specification.
* 
* @param resolver           The reference to the resolver.
* @param type_specification The routine type specification to resolve.
* @param generic_context    The generic context to use.
* @return The resolved routine type.
*/
resolver_resolve_type_specification_routine :: proc(
  resolver: ^Resolver,
  type_specification: ^Type_Specification,
  kind: Type_Specification_Kind,
  generic_context: Generic_Context,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_specification_routine := cast(^Type_Specification_Routine) type_specification;

  parameters: #soa [dynamic]Type_Routine_Parameter;
  for parameter in type_specification_routine.parameters {
    parameter_type := resolver_resolve_type_specification(resolver, parameter, generic_context);
    if .Not_Allowed_As_Parameter_Or_Return_Type in parameter_type.flags {
      report_error_fatal(parameter.position, "Type '%v' is not allowed as parameter type", parameter_type.name);
    }
    append_soa(&parameters, Type_Routine_Parameter{parameter_type, false});
  }

  return_type := resolver.storage.type_void;
  if type_specification_routine.return_type != nil {
    return_type = resolver_resolve_type_specification(resolver, type_specification_routine.return_type, generic_context);
    if .Not_Allowed_As_Parameter_Or_Return_Type in return_type.flags {
      report_error_fatal(type_specification_routine.return_type.position, "Type '%v' is not allowed as return type", return_type.name);
    }
  }

  has_params := type_specification_routine.has_params;
  if has_params {
    params_index := len(parameters) - 1;
    params_type := parameters[params_index].type;
    if !type_is_slice(params_type) {
      report_error_fatal(type_specification_routine.parameters[params_index].position, "Parameter marked 'params' must be a slice type. Got '%v'", params_type.name);
    }
  }
  
  return type_storage_get_or_make_type_routine(
    resolver.storage,
    kind == .Procedure ? .Procedure : .Function,
    type_specification_routine.calling_convention,
    parameters,
    return_type,
    type_make_routine_flags(has_params, type_specification_routine.has_c_varargs, false, false, type_specification_routine.is_pure),
  );
}

/**
* Resolves a layout struct for a type specification.
* 
* @param resolver          The reference to the resolver.
* @param position          The position of the type specification.
* @param collection_type   The collection type the layout struct is being used in.
* @param base              The base struct in AoS form the layout struct is based on.
* @param layout_modifier   The layout modifier of the layout struct.
* @param number_of_element The number of elements (in case on array).
* @return The resolved layout struct type.
*/
resolver_resolve_type_specification_layout_struct :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  collection_type: Collection_Type,
  base: ^Type,
  layout_modifier: Layout_Modifier,
  number_of_elements: int,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(layout_modifier != .None);

  cached_layout_struct := type_storage_get_cached_type_layout_struct(resolver.storage, collection_type, base, layout_modifier, number_of_elements);  
  if cached_layout_struct == nil {
    soa_symbol_name_suffix: string;
    switch collection_type {
      case .Array: soa_symbol_name_suffix = "Array";
      case .Dynamic_Array: soa_symbol_name_suffix = "Dynamic_Array";
      case .Slice: soa_symbol_name_suffix = "Slice";
      case .None: fallthrough;
      case: assert(false);
    }
    soa_symbol_name_infix := layout_modifier == .SoA ? "SoA" : "AoSoA";

    soa_symbol_name := resolver_make_formatted_symbol_name(resolver, "%v_%v_%v", base.symbol.name, soa_symbol_name_infix, soa_symbol_name_suffix);
    soa_symbol := symbol_make(resolver, .Type, .Private, base.symbol.package_file, soa_symbol_name, nil);

    aosoa_chunk_symbol: ^Symbol;
    items_in_aosoa_chunk: int;
    if layout_modifier == .AoSoA {
      aosoa_chunk_symbol_name := resolver_make_formatted_symbol_name(resolver, "%v_Base", soa_symbol_name);
      aosoa_chunk_symbol = symbol_make(resolver, .Type, .Private, base.symbol.package_file, aosoa_chunk_symbol_name, nil);

      // Currently we allow 'items_in_aosoa_chunk' to be equal to 1.
      // This is not really a problem but is just not really practial as it does not use the properties of AoSoA at all.
      // Its memory properties are similiar to that of a regular AoS.
      biggest_field: Type_Struct_Field;
      items_in_aosoa_chunk, biggest_field = type_calculate_items_in_aosoa_chunk(base);
      if items_in_aosoa_chunk <= 0 {
        report_error_fatal(
          position,
          "The struct '%v' is not allowed as a base for AoSoA as the field '%v' has a size which exceeds the limit of %v bytes for an AoSoA chunk",
          base.name,
          biggest_field.name,
          L1_CACHE_LINE_SIZE,
        );
      }
    }

    layout_struct_type := type_storage_make_type_layout_struct(
      resolver.storage,
      soa_symbol,
      collection_type,
      base,
      layout_modifier,
      number_of_elements,
      aosoa_chunk_symbol,
      items_in_aosoa_chunk,
    );
    soa_symbol.type = layout_struct_type;

    return layout_struct_type;
  } else {
    return cached_layout_struct;
  }
}
