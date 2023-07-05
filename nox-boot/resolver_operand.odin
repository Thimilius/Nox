package nox

import "core:fmt"
import "tracy"

I8_MIN: i8 : -128;
I8_MAX: i8 : 127;
I16_MIN: i16 : -32768;
I16_MAX: i16 : 32767;
I32_MIN: i32 : -2147483648;
I32_MAX: i32 : 2147483647;
I64_MIN: i64 : -9223372036854775808;
I64_MAX: i64 : 9223372036854775807;
INT_MIN: int : -9223372036854775808;
INT_MAX: int : 9223372036854775807;
U8_MIN: u8 : 0;
U8_MAX: u8 : 255;
U16_MIN: u16 : 0;
U16_MAX: u16 : 65535;
U32_MIN: u32 : 0;
U32_MAX: u32 : 4294967295;
U64_MIN: u64 : 0;
U64_MAX: u64 : 18446744073709551615;
UINT_MIN: uint : 0;
UINT_MAX: uint : 18446744073709551615;

/**
* Represents flags for an operand.
*/
Operand_Flag :: enum {
  Is_LValue,    // Is the operand an lvalue?
  Is_Constant,  // Is the operand a constant?
  Is_Pure,      // Is the operand pure?
  Is_Immutable, // Is the operand immutable?
  Is_Parameter, // Is the operand a parameter?
}

Operand_Flags :: bit_set[Operand_Flag];

/**
* Represents an operand.
*/
Operand :: struct {
  type: ^Type,          // The type of the operand.
  flags: Operand_Flags, // The flags of the operand.
  value: Value,         // The value of the operand (if constant).
}

/**
* Creates an rvalue operand.
*
* @param type         The type of the operand.
* @param is_pure      Is the operand pure?
* @param is_immutable Is the operand immutable?
* @return The rvalue operand.
*/
operand_rvalue :: proc(type: ^Type, is_pure: bool, is_immutable: bool) -> Operand {
  flags: Operand_Flags;
  if is_pure {
    flags += {.Is_Pure};
  }
  if is_immutable {
    flags += {.Is_Immutable};
  }
  return { type_unqualify(type), flags, nil };
}

/**
* Creates an lvalue operand.
*
* @param type         The type of the operand.
* @param is_pure      Is the operand pure?
* @param is_immutable Is the operand immutable?
* @return The lvalue operand.
*/
operand_lvalue :: proc(type: ^Type, is_pure: bool, is_immutable: bool) -> Operand {
  flags: Operand_Flags = {.Is_LValue};
  if is_pure {
    flags += {.Is_Pure};
  }
  if is_immutable {
    flags += {.Is_Immutable};
  }
  return { type_unqualify(type), flags, nil };
}

/**
* Creates a constant operand.
*
* @param type  The type of the operand.
* @param value The constant value of the operand.
* @return The constant operand.
*/
operand_constant :: proc(type: ^Type, value: Value) -> Operand {
  return { type_unqualify(type), {.Is_Constant, .Is_Pure, .Is_Immutable}, value };
}

/**
* Creates a parameter operand.
*
* @param type         The type of the operand.
* @param is_immutable Is the operand immutable?
* @return The parameter operand.
*/
operand_parameter :: proc(type: ^Type, is_immutable: bool) -> Operand {
  flags: Operand_Flags = {.Is_LValue, .Is_Pure, .Is_Parameter};
  if is_immutable {
    flags += {.Is_Immutable};
  }
  return { type_unqualify(type), flags, nil };
}

/**
* Trys to convert an operand into a given type.
*
* @param resolver The reference to the resolver.
* @param position The position of the conversion.
* @param operand  The operand to convert.
* @param type     The type to convert into.
* @return True if the conversion was successful otherwise false.
*/
operand_convert :: proc(resolver: ^Resolver, position: Source_Position, operand: ^Operand, type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_complete_type(resolver, position, type);
  actual_type_to_cast_to, is_convertible := operand_is_convertible(resolver, position, operand, type);
  if is_convertible {
    operand_cast_unchecked(position, operand, actual_type_to_cast_to);
    operand.flags -= {.Is_LValue};
    return true;
  }
  return false;
}

/**
* Checks whether or not a conversion is possible.
*
* @param resolver The reference to the resolver.
* @param position The position of the conversion.
* @param operand  The operand to check the conversion for.
* @param type     The type to convert into.
* @return 1. The type that can be converted into; 2. True if the conversion is possible otherwise false.
*/
operand_is_convertible :: proc(resolver: ^Resolver, position: Source_Position, operand: ^Operand, type: ^Type) -> (^Type, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  destination_type := type_unqualify(type);
  source_type := type_unqualify(operand.type);

  // It should never occur that we are trying to convert into an untyped type.
  assert(!type_is_untyped(destination_type));

  if source_type == destination_type {
    return type, true;
  } if type_is_union(destination_type) {
    return operand_can_convert_to_union(resolver, position, operand, destination_type);
  } else if type_is_any(destination_type) {
    // Converting something to an 'any' automatically marks the source type for RTTR.
    resolver_mark_type_for_rttr_generation(resolver, position, source_type);
    return type, !type_is_void(source_type) && !type_is_routine_intrinsic(source_type);
  } else if type_is_untyped_boolean(source_type) {
    return type, type_is_boolean(destination_type);
  } else if type_is_untyped_integer(source_type) {
    // Untyped integers are allowed to be converted to floats as well.
    return type, type_is_integer(destination_type) || type_is_float(destination_type) || type_is_offset_relative_pointer(destination_type);
  } else if type_is_untyped_float(source_type) {
    return type, type_is_float(destination_type);
  } else if type_is_untyped_char(source_type) {
    return type, type_is_char(destination_type);
  } else if type_is_untyped_string(source_type) {
    return type, type_is_string_like(destination_type);
  }
  
  // The reason we seperate these checks from the ones before is the handling of distintly defined types through 'type_define' declarations.

  result := false;
  if type_is_slice(source_type) {
    result = type_is_slice(destination_type) && destination_type.base == source_type.base;
  } else if type_is_untyped_null(source_type) {
    result = type_is_string_like(destination_type) || type_is_absolute_or_relative_pointer_like(destination_type) || type_is_dynamic_pointer(destination_type) || type_is_union(destination_type);
  } else if type_is_raw_pointer(destination_type) {
    result = type_is_absolute_pointer_like(source_type);
  } else if type_is_self_relative_pointer(destination_type) {
    result = type_is_absolute_pointer(source_type) && destination_type.base == source_type.base;
  } else if type_is_offset_relative_pointer(destination_type) {
    result = (cast(^Type_Relative_Pointer) destination_type).relative_base == source_type;
  } else if type_is_dynamic_pointer(destination_type) {
    if type_is_absolute_pointer(source_type) {
      for interface in source_type.base.interfaces {
        if interface == destination_type.base {
          result = true;
          break;
        }
      }
    }
  }

  if .Defined_Distinct in source_type.flags {
    result = false;
  }

  return type, result;
}

/**
* Checks whether or not type can be converted into a union.
*
* @param resolver         The reference to the resolver.
* @param position         The position of the conversion.
* @param operand          The operand to check the conversion for.
* @param destination_type The type to convert into.
* @return 1. The type that can be converted into; 2. True if the conversion is possible otherwise false.
*/
operand_can_convert_to_union :: proc(resolver: ^Resolver, position: Source_Position, operand: ^Operand, destination_type: ^Type) -> (^Type, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // For unions our source must match with any of the types in the union.
  type_union := cast(^Type_Union) destination_type;
  can_convert_to_union := false;

  // We allow 'null' as a default value for unions (as we also allow comparing with 'null').
  if (type_is_untyped_null(operand.type)) {
    return destination_type, true;
  }

  ambiguous_types := make_dynamic_array_len_cap([dynamic]^Type, 0, len(type_union.variants), context.temp_allocator);
  for variant_type in type_union.variants {
    _, convertible := operand_is_convertible(resolver, position, operand, variant_type);
    if convertible {
      can_convert_to_union = true;
      append(&ambiguous_types, variant_type);
    }
  }

  if len(ambiguous_types) > 1 {
    ambiguous_type_names := type_get_type_names(ambiguous_types);
    report_error_fatal(position, "Cannot implicitly convert type '%v' to union. Ambigous types:\n%v", operand.type.name, ambiguous_type_names);
  }

  actual_union_type: ^Type = nil;
  if len(ambiguous_types) > 0 {
    actual_union_type = ambiguous_types[0];
  }

  if !can_convert_to_union {
    possible_types := type_get_type_names(type_union.variants);
    report_error_fatal(position, "Cannot implicitly convert type '%v' to union '%v'. Possible types:\n%v", operand.type.name, destination_type.name, possible_types);
  }

  return actual_union_type, can_convert_to_union;
}

/**
* Checks whether or not a cast is possible.
*
* @param resolver The reference to the resolver.
* @param position The position of the cast.
* @param operand  The operand to check the cast for.
* @param type     The type to convert into.
* @return 1. The type that can be converted into; 2. True if the cast is possible otherwise false.
*/
operand_is_castable :: proc(resolver: ^Resolver, position: Source_Position, operand: ^Operand, type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  destination_type := type_unqualify(type);
  source_type := type_unqualify(operand.type);

  _, is_convertible := operand_is_convertible(resolver, position, operand, type);
  if is_convertible {
    return true;
  } else if type_is_scalar(source_type) && type_is_scalar(destination_type) {
    if type_is_untyped_integer(source_type) {
      // Untyped integers can be casted to: bool, char, enums and any integer type of course.
      return type_is_boolean(destination_type) || type_is_char(destination_type) || type_is_enumeration(destination_type) || type_is_integer(destination_type);
    } else if type_is_untyped_float(source_type) {
      // Untyped floats can only be casted to typed floats.
      return type_is_float(destination_type);
    } else if type_is_untyped_char(source_type) {
      // Untyped chars can be casted to: any integer or char of course.
      return type_is_integer(destination_type) || type_is_char(destination_type);
    } else if type_is_float(source_type) && type_is_boolean(destination_type) {
      // Floats cannot be casted to bool.
      return !type_is_boolean(destination_type);
    } else if type_is_boolean(source_type) {
      // Bool cannot be casted to float.
      return !type_is_float(destination_type);
    } else {
      return true;
    }
  } else if type_is_absolute_pointer_like(source_type) || source_type.kind == .CString {
    // We treat cstrings as pointers (which they are).
    return type_is_absolute_pointer_like(destination_type) || destination_type.kind == .UInt || destination_type.kind == .CString;
  } else if type_is_absolute_pointer_like(destination_type) || destination_type.kind == .CString  {
    return type_is_absolute_pointer_like(source_type) || type_is_self_relative_pointer(source_type) || source_type.kind == .UInt || source_type.kind == .CString;
  } else {
    return false;
  }
}

/**
* Trys to cast an operand into a given type.
*
* @param resolver The reference to the resolver.
* @param position The position of the cast.
* @param operand  The operand to cast.
* @param type     The type to cast into.
* @return True if the cast was successful otherwise false.
*/
operand_cast :: proc(resolver: ^Resolver, position: Source_Position, operand: ^Operand, type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if operand_is_castable(resolver, position, operand, type_unqualify(type)) {
    operand_cast_unchecked(position, operand, type);
    return true;
  } else {
    return false;
  }
}

/**
* Makes an unchecked cast.
*
* @param position The position of the cast.
* @param operand  The operand to cast.
* @param type     The type to cast into.
*/
operand_cast_unchecked :: proc(position: Source_Position, operand: ^Operand, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);
  
  destination_type := type_unqualify(type);
  source_type := type_unqualify(operand.type);

  // We only need to perform the actual cast when we have a constant we are working with.
  if .Is_Constant in operand.flags && !type_is_any(destination_type) {
    real_type := destination_type;

    // Convert enums to their base type.
    if type_is_enumeration(source_type) {
      source_type = source_type.base;
    }
    if type_is_enumeration(destination_type) {
      destination_type = destination_type.base;
    }

    // Convert offset-relative pointers to their relative base.
    if type_is_offset_relative_pointer(destination_type) {
      destination_type = (cast(^Type_Relative_Pointer) destination_type).relative_base;
    }

    #partial switch source_type.kind {
      case .B8:   operand_cast_boolean(operand, destination_type, b8);
      case .B16:  operand_cast_boolean(operand, destination_type, b16);
      case .B32:  operand_cast_boolean(operand, destination_type, b32);
      case .B64:  operand_cast_boolean(operand, destination_type, b64);
      case .Bool: operand_cast_boolean(operand, destination_type, bool);

      case .I8:   operand_cast_integer(operand, destination_type, i8);
      case .I16:  operand_cast_integer(operand, destination_type, i16);
      case .I32:  operand_cast_integer(operand, destination_type, i32);
      case .I64:  operand_cast_integer(operand, destination_type, i64);
      case .Int:  operand_cast_integer(operand, destination_type, int);
      case .U8:   operand_cast_integer(operand, destination_type, u8);
      case .U16:  operand_cast_integer(operand, destination_type, u16);
      case .U32:  operand_cast_integer(operand, destination_type, u32);
      case .U64:  operand_cast_integer(operand, destination_type, u64);
      case .UInt: operand_cast_integer(operand, destination_type, uint);
      case .Untyped_Integer: operand_cast_untyped_integer(position, operand, destination_type, real_type);

      case .F32: operand_cast_float(operand, destination_type, f32);
      case .F64: operand_cast_float(operand, destination_type, f64);
      case .Untyped_Float: operand_cast_untyped_float(operand, destination_type);

      case .Char, .Untyped_Char: operand_cast_char(operand, destination_type);

      // For these we do not need to convert the values as setting the new type is enough.
      case .Untyped_String, .Untyped_Null, .Typeid:

      case: operand.flags -= {.Is_Constant};
    }
  }

  operand.type = type;
}

/**
* Casts an operand as a boolean.
*
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
* @param T                The type of the operand value.
*/
operand_cast_boolean :: proc(operand: ^Operand, destination_type: ^Type, $T: typeid) {
  #partial switch destination_type.kind {
    case .B8:   operand.value = cast(b8)   operand.value.(T);
    case .B16:  operand.value = cast(b16)  operand.value.(T);
    case .B32:  operand.value = cast(b32)  operand.value.(T);
    case .B64:  operand.value = cast(b64)  operand.value.(T);
    case .Bool: operand.value = cast(bool) operand.value.(T);
    case .I8:   operand.value = cast(i8)   operand.value.(T);
    case .I16:  operand.value = cast(i16)  operand.value.(T);
    case .I32:  operand.value = cast(i32)  operand.value.(T);
    case .I64:  operand.value = cast(i64)  operand.value.(T);
    case .Int:  operand.value = cast(int)  operand.value.(T);
    case .U8:   operand.value = cast(u8)   operand.value.(T);
    case .U16:  operand.value = cast(u16)  operand.value.(T);
    case .U32:  operand.value = cast(u32)  operand.value.(T);
    case .U64:  operand.value = cast(u64)  operand.value.(T);
    case .UInt: operand.value = cast(uint) operand.value.(T);
    case: assert(false);
  }
}

/**
* Casts an operand as an integer.
*
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
* @param T                The type of the operand value.
*/
operand_cast_integer :: proc(operand: ^Operand, destination_type: ^Type, $T: typeid) {
  #partial switch destination_type.kind {
    case .B8:   operand.value = cast(b8)   operand.value.(T);
    case .B16:  operand.value = cast(b16)  operand.value.(T);
    case .B32:  operand.value = cast(b32)  operand.value.(T);
    case .B64:  operand.value = cast(b64)  operand.value.(T);
    case .Bool: operand.value = cast(bool) operand.value.(T);
    case .I8:   operand.value = cast(i8)   operand.value.(T);
    case .I16:  operand.value = cast(i16)  operand.value.(T);
    case .I32:  operand.value = cast(i32)  operand.value.(T);
    case .I64:  operand.value = cast(i64)  operand.value.(T);
    case .Int:  operand.value = cast(int)  operand.value.(T);
    case .U8:   operand.value = cast(u8)   operand.value.(T);
    case .U16:  operand.value = cast(u16)  operand.value.(T);
    case .U32:  operand.value = cast(u32)  operand.value.(T);
    case .U64:  operand.value = cast(u64)  operand.value.(T);
    case .UInt: operand.value = cast(uint) operand.value.(T);
    case .F32:  operand.value = cast(f32)  operand.value.(T);
    case .F64:  operand.value = cast(f64)  operand.value.(T);
    case .Char: operand.value = cast(rune) operand.value.(T);
    case: assert(false);
  }
}

/**
* Casts an operand as an untyped integer.
*
* @param position         The position of the cast.
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
* @param real_type        The real type of the operand value.
*/
operand_cast_untyped_integer :: proc(position: Source_Position, operand: ^Operand, destination_type: ^Type, real_type: ^Type) {
  #partial switch destination_type.kind {
    case .B8:   operand.value = cast(b8)   operand.value.(u64);
    case .B16:  operand.value = cast(b16)  operand.value.(u64);
    case .B32:  operand.value = cast(b32)  operand.value.(u64);
    case .B64:  operand.value = cast(b64)  operand.value.(u64);
    case .Bool: operand.value = cast(bool) operand.value.(u64);
    case .I8:   operand_cast_untyped_integer_signed(position, operand, destination_type, real_type, i8, I8_MIN, I8_MAX);
    case .I16:  operand_cast_untyped_integer_signed(position, operand, destination_type, real_type, i16, I16_MIN, I16_MAX);
    case .I32:  operand_cast_untyped_integer_signed(position, operand, destination_type, real_type, i32, I32_MIN, I32_MAX);
    case .I64:  operand_cast_untyped_integer_signed(position, operand, destination_type, real_type, i64, I64_MIN, I64_MAX);
    case .Int:  operand_cast_untyped_integer_signed(position, operand, destination_type, real_type, int, INT_MIN, INT_MAX);
    case .U8:   operand_cast_untyped_integer_unsigned(position, operand, destination_type, real_type, u8, U8_MIN, U8_MAX);
    case .U16:  operand_cast_untyped_integer_unsigned(position, operand, destination_type, real_type, u16, U16_MIN, U16_MAX);
    case .U32:  operand_cast_untyped_integer_unsigned(position, operand, destination_type, real_type, u32, U32_MIN, U32_MAX);
    case .U64:  operand_cast_untyped_integer_unsigned(position, operand, destination_type, real_type, u64, U64_MIN, U64_MAX);
    case .UInt: operand_cast_untyped_integer_unsigned(position, operand, destination_type, real_type, uint, UINT_MIN, UINT_MAX);
    case .F32:  operand.value = cast(f32)  operand.value.(u64);
    case .F64:  operand.value = cast(f64)  operand.value.(u64);
    case .Char: operand.value = cast(rune) operand.value.(u64);
    case .Offset_Relative_Pointer: operand_cast_untyped_integer(position, operand, destination_type.base, real_type);
    case: assert(false);
  }
}

/**
* Casts an operand as an untyped signed integer.
*
* @param position  The position of the cast.
* @param operand   The operand to cast.
* @param type      The destination type to cast into.
* @param real_type The real type of the operand value.
* @param T         The type of the operand value.
* @parma min       The minimum value.
* @parma min       The maximum value.
*/
operand_cast_untyped_integer_signed :: proc(position: Source_Position, operand: ^Operand, type: ^Type, real_type: ^Type, $T: typeid, min: T, max: T) {
  value := cast(i64) operand.value.(u64);

  if value < cast(i64) min || value > cast(i64) max {
    if type_is_enumeration(real_type) {
      report_error_fatal(position, "Integer constant is out of range for enumeration '%v' with underlying type '%v'", real_type.name, real_type.base.name);
    } else {
      report_error_fatal(position, "Integer constant is out of range for type '%v'", real_type.name);
    }
  }

  operand.value = cast(T) value;
}

/**
* Casts an operand as an untyped unsigned integer.
*
* @param position  The position of the cast.
* @param operand   The operand to cast.
* @param type      The destination type to cast into.
* @param real_type The real type of the operand value.
* @param T         The type of the operand value.
* @parma min       The minimum value.
* @parma min       The maximum value.
*/
operand_cast_untyped_integer_unsigned :: proc(position: Source_Position, operand: ^Operand, type: ^Type, real_type: ^Type, $T: typeid, min: T, max: T) {
  value := operand.value.(u64);

  if value > cast(u64) max {
    if type_is_enumeration(real_type) {
      report_error_fatal(position, "Integer constant is out of range for enumeration '%v' with underlying type '%v'", real_type.name, real_type.base.name);
    } else {
      report_error_fatal(position, "Integer constant is out of range for type '%v'", real_type.name);
    }
  }

  operand.value = cast(T) value;
}

/**
* Casts an operand as a float.
*
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
* @param T                The type of the operand value.
*/
operand_cast_float :: proc(operand: ^Operand, destination_type: ^Type, $T: typeid) {
  #partial switch destination_type.kind {
    case .I8:   operand.value = cast(i8)   operand.value.(T);
    case .I16:  operand.value = cast(i16)  operand.value.(T);
    case .I32:  operand.value = cast(i32)  operand.value.(T);
    case .I64:  operand.value = cast(i64)  operand.value.(T);
    case .Int:  operand.value = cast(int)  operand.value.(T);
    case .U8:   operand.value = cast(u8)   operand.value.(T);
    case .U16:  operand.value = cast(u16)  operand.value.(T);
    case .U32:  operand.value = cast(u32)  operand.value.(T);
    case .U64:  operand.value = cast(u64)  operand.value.(T);
    case .UInt: operand.value = cast(uint) operand.value.(T);
    case .F32:  operand.value = cast(f32)  operand.value.(T);
    case .F64:  operand.value = cast(f64)  operand.value.(T);
    case: assert(false);
  }
}

/**
* Casts an operand as an untyped float.
*
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
*/
operand_cast_untyped_float :: proc(operand: ^Operand, destination_type: ^Type) {
  #partial switch destination_type.kind {
    case .F32: operand.value = cast(f32) operand.value.(f64);
    case .F64: operand.value = cast(f64) operand.value.(f64);
    case: assert(false);
  }
}

/**
* Casts an operand as a char.
*
* @param operand          The operand to cast.
* @param destination_type The destination type to cast into.
*/
operand_cast_char :: proc(operand: ^Operand, destination_type: ^Type) {
  #partial switch destination_type.kind {
    case .I8:   operand.value = cast(i8)   operand.value.(rune);
    case .I16:  operand.value = cast(i16)  operand.value.(rune);
    case .I32:  operand.value = cast(i32)  operand.value.(rune);
    case .I64:  operand.value = cast(i64)  operand.value.(rune);
    case .Int:  operand.value = cast(int)  operand.value.(rune);
    case .U8:   operand.value = cast(u8)   operand.value.(rune);
    case .U16:  operand.value = cast(u16)  operand.value.(rune);
    case .U32:  operand.value = cast(u32)  operand.value.(rune);
    case .U64:  operand.value = cast(u64)  operand.value.(rune);
    case .UInt: operand.value = cast(uint) operand.value.(rune);
    case .Char: operand.value = cast(rune) operand.value.(rune);
    case: assert(false);
  }
}

/**
* Removes an untyped type from an operand.
* 
* @param storage  The reference to the type storage.
* @param position The position of the removal.
* @param operand  The operand to remove the untyped type of.
*/
operand_remove_untyped :: proc(storage: ^Type_Storage, position: Source_Position, operand: ^Operand) {
  if type_is_untyped_boolean(operand.type) {
    operand_cast_unchecked(position, operand, storage.type_bool);
  } else if type_is_untyped_integer(operand.type) {
    operand_cast_unchecked(position, operand, storage.type_int);
  } else if type_is_untyped_float(operand.type) {
    operand_cast_unchecked(position, operand, storage.type_f64);
  } else if type_is_untyped_char(operand.type) {
    operand_cast_unchecked(position, operand, storage.type_char);
  } else if type_is_untyped_string(operand.type) {
    operand_cast_unchecked(position, operand, storage.type_string);
  }
}

/**
* Decays an operand.
* 
* @param operand The operand to decay.
* @return The decayed operand.
*/
operand_decay :: proc(operand: Operand) -> Operand {
  result := operand;
  result.flags -= {.Is_LValue};
  return result;
}
