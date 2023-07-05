package nox

import "tracy"

/**
* Resolves a unary operation.
*
* @param operator The operator of the operation.
* @param type     The type of the operands.
* @param value    The constant value to operand on.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_unary :: proc(operator: Token_Kind, type: ^Type, value: Value) -> Value {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);
  
  type := type;
  if (type_is_enumeration(type)) {
    type = type.base;
  }

  #partial switch operator {
    case .Add: // We don't need to do anything here.
    case .Subtract: {
      #partial switch type.kind {
        case .I8:              return -value.(i8);
        case .I16:             return -value.(i16);
        case .I32:             return -value.(i32);
        case .I64:             return -value.(i64);
        case .Int:             return -value.(int);
        case .U8:              return -value.(u8);
        case .U16:             return -value.(u16);
        case .U32:             return -value.(u32);
        case .U64:             return -value.(u64);
        case .UInt:            return -value.(uint);
        case .Untyped_Integer: return -value.(u64);
        case .F32:             return -value.(f32);
        case .F64:             return -value.(f64);
        case .Untyped_Float:   return -value.(f64);
        case: assert(false);
      }
    }
    case .Negate: {
      #partial switch type.kind {
        case .I8:              return ~value.(i8);
        case .I16:             return ~value.(i16);
        case .I32:             return ~value.(i32);
        case .I64:             return ~value.(i64);
        case .Int:             return ~value.(int);
        case .U8:              return ~value.(u8);
        case .U16:             return ~value.(u16);
        case .U32:             return ~value.(u32);
        case .U64:             return ~value.(u64);
        case .UInt:            return ~value.(uint);
        case .Untyped_Integer: return ~value.(u64);
        case: assert(false);
      }
    }
    case .Not: {
      #partial switch type.kind {
        case .B8:              return cast(b8) !value.(b8);
        case .B16:             return cast(b16) !value.(b16);
        case .B32:             return cast(b32) !value.(b32);
        case .B64:             return cast(b64) !value.(b64);
        case .Bool:            return cast(bool) !value.(bool);
        case .Untyped_Boolean: return cast(bool) !value.(bool);
        case: assert(false);
      }
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves a binary operation.
*
* @param operator The operator of the operation.
* @param type     The type of the operands.
* @param left     The left constant value operand.
* @param right    The right constant value operand.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_binary :: proc(operator: Token_Kind, type: ^Type, left: Value, right: Value) -> Value {
  // Binary operations can be untyped but that means that both operands are.

  type := type;
  if (type_is_enumeration(type)) {
    type = type.base;
  }

  #partial switch operator {
    case .Multiply: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   * right.(i8);
        case .I16:             return left.(i16)  * right.(i16);
        case .I32:             return left.(i32)  * right.(i32);
        case .I64:             return left.(i64)  * right.(i64);
        case .Int:             return left.(int)  * right.(int);
        case .U8:              return left.(u8)   * right.(u8);
        case .U16:             return left.(u16)  * right.(u16);
        case .U32:             return left.(u32)  * right.(u32);
        case .U64:             return left.(u64)  * right.(u64);
        case .UInt:            return left.(uint) * right.(uint);
        case .Untyped_Integer: return left.(u64)  * right.(u64);
        case .F32:             return left.(f32)  * right.(f32);
        case .F64:             return left.(f64)  * right.(f64);
        case .Untyped_Float:   return left.(f64)  * right.(f64);
      }
    }
    case .Divide: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   / right.(i8);
        case .I16:             return left.(i16)  / right.(i16);
        case .I32:             return left.(i32)  / right.(i32);
        case .I64:             return left.(i64)  / right.(i64);
        case .Int:             return left.(int)  / right.(int);
        case .U8:              return left.(u8)   / right.(u8);
        case .U16:             return left.(u16)  / right.(u16);
        case .U32:             return left.(u32)  / right.(u32);
        case .U64:             return left.(u64)  / right.(u64);
        case .UInt:            return left.(uint) / right.(uint);
        case .Untyped_Integer: return left.(u64)  / right.(u64);
        case .F32:             return left.(f32)  / right.(f32);
        case .F64:             return left.(f64)  / right.(f64);
        case .Untyped_Float:   return left.(f64)  / right.(f64);
      }
    }
    case .Modulo: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   % right.(i8);
        case .I16:             return left.(i16)  % right.(i16);
        case .I32:             return left.(i32)  % right.(i32);
        case .I64:             return left.(i64)  % right.(i64);
        case .Int:             return left.(int)  % right.(int);
        case .U8:              return left.(u8)   % right.(u8);
        case .U16:             return left.(u16)  % right.(u16);
        case .U32:             return left.(u32)  % right.(u32);
        case .U64:             return left.(u64)  % right.(u64);
        case .UInt:            return left.(uint) % right.(uint);
        case .Untyped_Integer: return left.(u64)  % right.(u64);
      }
    }
    case .Add: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   + right.(i8);
        case .I16:             return left.(i16)  + right.(i16);
        case .I32:             return left.(i32)  + right.(i32);
        case .I64:             return left.(i64)  + right.(i64);
        case .Int:             return left.(int)  + right.(int);
        case .U8:              return left.(u8)   + right.(u8);
        case .U16:             return left.(u16)  + right.(u16);
        case .U32:             return left.(u32)  + right.(u32);
        case .U64:             return left.(u64)  + right.(u64);
        case .UInt:            return left.(uint) + right.(uint);
        case .Untyped_Integer: return left.(u64)  + right.(u64);
        case .F32:             return left.(f32)  + right.(f32);
        case .F64:             return left.(f64)  + right.(f64);
        case .Untyped_Float:   return left.(f64)  + right.(f64);
      }
    }
    case .And: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   & right.(i8);
        case .I16:             return left.(i16)  & right.(i16);
        case .I32:             return left.(i32)  & right.(i32);
        case .I64:             return left.(i64)  & right.(i64);
        case .Int:             return left.(int)  & right.(int);
        case .U8:              return left.(u8)   & right.(u8);
        case .U16:             return left.(u16)  & right.(u16);
        case .U32:             return left.(u32)  & right.(u32);
        case .U64:             return left.(u64)  & right.(u64);
        case .UInt:            return left.(uint) & right.(uint);
        case .Untyped_Integer: return left.(u64)  & right.(u64);
      }
    }
    case .Or: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   | right.(i8);
        case .I16:             return left.(i16)  | right.(i16);
        case .I32:             return left.(i32)  | right.(i32);
        case .I64:             return left.(i64)  | right.(i64);
        case .Int:             return left.(int)  | right.(int);
        case .U8:              return left.(u8)   | right.(u8);
        case .U16:             return left.(u16)  | right.(u16);
        case .U32:             return left.(u32)  | right.(u32);
        case .U64:             return left.(u64)  | right.(u64);
        case .UInt:            return left.(uint) | right.(uint);
        case .Untyped_Integer: return left.(u64)  | right.(u64);
      }
    }
    case .Xor: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   ~ right.(i8);
        case .I16:             return left.(i16)  ~ right.(i16);
        case .I32:             return left.(i32)  ~ right.(i32);
        case .I64:             return left.(i64)  ~ right.(i64);
        case .Int:             return left.(int)  ~ right.(int);
        case .U8:              return left.(u8)   ~ right.(u8);
        case .U16:             return left.(u16)  ~ right.(u16);
        case .U32:             return left.(u32)  ~ right.(u32);
        case .U64:             return left.(u64)  ~ right.(u64);
        case .UInt:            return left.(uint) ~ right.(uint);
        case .Untyped_Integer: return left.(u64)  ~ right.(u64);
      }
    }
    case .Subtract: {
      #partial switch type.kind {
        case .I8:              return left.(i8)   - right.(i8);
        case .I16:             return left.(i16)  - right.(i16);
        case .I32:             return left.(i32)  - right.(i32);
        case .I64:             return left.(i64)  - right.(i64);
        case .Int:             return left.(int)  - right.(int);
        case .U8:              return left.(u8)   - right.(u8);
        case .U16:             return left.(u16)  - right.(u16);
        case .U32:             return left.(u32)  - right.(u32);
        case .U64:             return left.(u64)  - right.(u64);
        case .UInt:            return left.(uint) - right.(uint);
        case .Untyped_Integer: return left.(u64)  - right.(u64);
        case .F32:             return left.(f32)  - right.(f32);
        case .F64:             return left.(f64)  - right.(f64);
        case .Untyped_Float:   return left.(f64)  - right.(f64);
      }
    }

    case .Equal: {
      #partial switch type.kind {
        case .B8:              return cast(bool) (left.(b8)   == right.(b8));
        case .B16:             return cast(bool) (left.(b16)  == right.(b16));
        case .B32:             return cast(bool) (left.(b32)  == right.(b32));
        case .B64:             return cast(bool) (left.(b64)  == right.(b64));
        case .Bool:            return cast(bool) (left.(bool) == right.(bool));
        case .Untyped_Boolean: return cast(bool) (left.(bool) == right.(bool));
        case .I8:              return cast(bool) (left.(i8)   == right.(i8));
        case .I16:             return cast(bool) (left.(i16)  == right.(i16));
        case .I32:             return cast(bool) (left.(i32)  == right.(i32));
        case .I64:             return cast(bool) (left.(i64)  == right.(i64));
        case .Int:             return cast(bool) (left.(int)  == right.(int));
        case .U8:              return cast(bool) (left.(u8)   == right.(u8));
        case .U16:             return cast(bool) (left.(u16)  == right.(u16));
        case .U32:             return cast(bool) (left.(u32)  == right.(u32));
        case .U64:             return cast(bool) (left.(u64)  == right.(u64));
        case .UInt:            return cast(bool) (left.(uint) == right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  == right.(u64));
        case .F32:             return cast(bool) (left.(f32)  == right.(f32));
        case .F64:             return cast(bool) (left.(f64)  == right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  == right.(f64));
        case .Char:            return cast(bool) (left.(rune) == right.(rune));
        case .Untyped_Char:    return cast(bool) (left.(rune) == right.(rune));
        case .Untyped_String:  return resolver_evaluate_operation_binary_comparison_string(operator, left, right);
        case .Typeid:          return cast(bool) (left.(Type_Id) == right.(Type_Id));
      }
    }
    case .Not_Equal: {
      #partial switch type.kind {
        case .B8:              return cast(bool) (left.(b8)   != right.(b8));
        case .B16:             return cast(bool) (left.(b16)  != right.(b16));
        case .B32:             return cast(bool) (left.(b32)  != right.(b32));
        case .B64:             return cast(bool) (left.(b64)  != right.(b64));
        case .Bool:            return cast(bool) (left.(bool) != right.(bool));
        case .Untyped_Boolean: return cast(bool) (left.(bool) != right.(bool));
        case .I8:              return cast(bool) (left.(i8)   != right.(i8));
        case .I16:             return cast(bool) (left.(i16)  != right.(i16));
        case .I32:             return cast(bool) (left.(i32)  != right.(i32));
        case .I64:             return cast(bool) (left.(i64)  != right.(i64));
        case .Int:             return cast(bool) (left.(int)  != right.(int));
        case .U8:              return cast(bool) (left.(u8)   != right.(u8));
        case .U16:             return cast(bool) (left.(u16)  != right.(u16));
        case .U32:             return cast(bool) (left.(u32)  != right.(u32));
        case .U64:             return cast(bool) (left.(u64)  != right.(u64));
        case .UInt:            return cast(bool) (left.(uint) != right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  != right.(u64));
        case .F32:             return cast(bool) (left.(f32)  != right.(f32));
        case .F64:             return cast(bool) (left.(f64)  != right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  != right.(f64));
        case .Char:            return cast(bool) (left.(rune) != right.(rune));
        case .Untyped_Char:    return cast(bool) (left.(rune) != right.(rune));
        case .Untyped_String:  return resolver_evaluate_operation_binary_comparison_string(operator, left, right);
        case .Typeid:          return cast(bool) (left.(Type_Id) != right.(Type_Id));
      }
    }
    case .Less_Than: {
      #partial switch type.kind {
        case .I8:              return cast(bool) (left.(i8)   < right.(i8));
        case .I16:             return cast(bool) (left.(i16)  < right.(i16));
        case .I32:             return cast(bool) (left.(i32)  < right.(i32));
        case .I64:             return cast(bool) (left.(i64)  < right.(i64));
        case .Int:             return cast(bool) (left.(int)  < right.(int));
        case .U8:              return cast(bool) (left.(u8)   < right.(u8));
        case .U16:             return cast(bool) (left.(u16)  < right.(u16));
        case .U32:             return cast(bool) (left.(u32)  < right.(u32));
        case .U64:             return cast(bool) (left.(u64)  < right.(u64));
        case .UInt:            return cast(bool) (left.(uint) < right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  < right.(u64));
        case .F32:             return cast(bool) (left.(f32)  < right.(f32));
        case .F64:             return cast(bool) (left.(f64)  < right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  < right.(f64));
      }
    }
    case .Less_Than_Equal: {
      #partial switch type.kind {
        case .I8:              return cast(bool) (left.(i8)   <= right.(i8));
        case .I16:             return cast(bool) (left.(i16)  <= right.(i16));
        case .I32:             return cast(bool) (left.(i32)  <= right.(i32));
        case .I64:             return cast(bool) (left.(i64)  <= right.(i64));
        case .Int:             return cast(bool) (left.(int)  <= right.(int));
        case .U8:              return cast(bool) (left.(u8)   <= right.(u8));
        case .U16:             return cast(bool) (left.(u16)  <= right.(u16));
        case .U32:             return cast(bool) (left.(u32)  <= right.(u32));
        case .U64:             return cast(bool) (left.(u64)  <= right.(u64));
        case .UInt:            return cast(bool) (left.(uint) <= right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  <= right.(u64));
        case .F32:             return cast(bool) (left.(f32)  <= right.(f32));
        case .F64:             return cast(bool) (left.(f64)  <= right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  <= right.(f64));
      }
    }
    case .Greater_Than: {
      #partial switch type.kind {
        case .I8:              return cast(bool) (left.(i8)   > right.(i8));
        case .I16:             return cast(bool) (left.(i16)  > right.(i16));
        case .I32:             return cast(bool) (left.(i32)  > right.(i32));
        case .I64:             return cast(bool) (left.(i64)  > right.(i64));
        case .Int:             return cast(bool) (left.(int)  > right.(int));
        case .U8:              return cast(bool) (left.(u8)   > right.(u8));
        case .U16:             return cast(bool) (left.(u16)  > right.(u16));
        case .U32:             return cast(bool) (left.(u32)  > right.(u32));
        case .U64:             return cast(bool) (left.(u64)  > right.(u64));
        case .UInt:            return cast(bool) (left.(uint) > right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  > right.(u64));
        case .F32:             return cast(bool) (left.(f32)  > right.(f32));
        case .F64:             return cast(bool) (left.(f64)  > right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  > right.(f64));
      }
    }
    case .Greater_Than_Equal: {
      #partial switch type.kind {
        case .I8:              return cast(bool) (left.(i8)   >= right.(i8));
        case .I16:             return cast(bool) (left.(i16)  >= right.(i16));
        case .I32:             return cast(bool) (left.(i32)  >= right.(i32));
        case .I64:             return cast(bool) (left.(i64)  >= right.(i64));
        case .Int:             return cast(bool) (left.(int)  >= right.(int));
        case .U8:              return cast(bool) (left.(u8)   >= right.(u8));
        case .U16:             return cast(bool) (left.(u16)  >= right.(u16));
        case .U32:             return cast(bool) (left.(u32)  >= right.(u32));
        case .U64:             return cast(bool) (left.(u64)  >= right.(u64));
        case .UInt:            return cast(bool) (left.(uint) >= right.(uint));
        case .Untyped_Integer: return cast(bool) (left.(u64)  >= right.(u64));
        case .F32:             return cast(bool) (left.(f32)  >= right.(f32));
        case .F64:             return cast(bool) (left.(f64)  >= right.(f64));
        case .Untyped_Float:   return cast(bool) (left.(f64)  >= right.(f64));
      }
    }

    case .And_And: {
      return left.(bool) && right.(bool);
    }
    case .Or_Or: {
      return left.(bool) || right.(bool);
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves a binary string comparison.
*
* @param operator The operator of the operation.
* @param left     The left constant value operand.
* @param right    The right constant value operand.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_binary_comparison_string :: proc(operator: Token_Kind, left: Value, right: Value) -> Value {
  #partial switch operator {
    case .Equal: {
      if left == nil && right == nil {
        return cast(bool) true;
      }
      if left == nil || right == nil {
        return cast(bool) false;
      }
      return cast(bool) (left.(string) == right.(string));
    }
    case .Not_Equal: {
      if left == nil && right == nil {
        return cast(bool) false;
      }
      if left == nil || right == nil {
        return cast(bool) true;
      }
      return cast(bool) (left.(string) != right.(string));
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves a binary shift operation.
*
* @param operator The operator of the operation.
* @param left     The left constant value operand.
* @param right    The right constant value operand.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_binary_shift :: proc(operator: Token_Kind, left: Operand, right: Operand) -> Value {
  if operator == .Left_Shift {
    #partial switch left.type.kind {
      case .I8:              return resolver_evaluate_operation_binary_shift_left(left, right, i8);
      case .I16:             return resolver_evaluate_operation_binary_shift_left(left, right, i16);
      case .I32:             return resolver_evaluate_operation_binary_shift_left(left, right, i32);
      case .I64:             return resolver_evaluate_operation_binary_shift_left(left, right, i64);
      case .Int:             return resolver_evaluate_operation_binary_shift_left(left, right, int);
      case .U8:              return resolver_evaluate_operation_binary_shift_left(left, right, u8);
      case .U16:             return resolver_evaluate_operation_binary_shift_left(left, right, u16);
      case .U32:             return resolver_evaluate_operation_binary_shift_left(left, right, u32);
      case .U64:             return resolver_evaluate_operation_binary_shift_left(left, right, u64);
      case .UInt:            return resolver_evaluate_operation_binary_shift_left(left, right, uint);
      case .Untyped_Integer: return resolver_evaluate_operation_binary_shift_left(left, right, u64);
    }
  } else if operator == .Right_Shift {
    #partial switch left.type.kind {
      case .I8:              return resolver_evaluate_operation_binary_shift_right(left, right, i8);
      case .I16:             return resolver_evaluate_operation_binary_shift_right(left, right, i16);
      case .I32:             return resolver_evaluate_operation_binary_shift_right(left, right, i32);
      case .I64:             return resolver_evaluate_operation_binary_shift_right(left, right, i64);
      case .Int:             return resolver_evaluate_operation_binary_shift_right(left, right, int);
      case .U8:              return resolver_evaluate_operation_binary_shift_right(left, right, u8);
      case .U16:             return resolver_evaluate_operation_binary_shift_right(left, right, u16);
      case .U32:             return resolver_evaluate_operation_binary_shift_right(left, right, u32);
      case .U64:             return resolver_evaluate_operation_binary_shift_right(left, right, u64);
      case .UInt:            return resolver_evaluate_operation_binary_shift_right(left, right, uint);
      case .Untyped_Integer: return resolver_evaluate_operation_binary_shift_right(left, right, u64);
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves a binary left shift operation.
*
* @param left  The left constant value operand.
* @param right The right constant value operand.
* @param T     The type of the left operand.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_binary_shift_left :: proc(left: Operand, right: Operand, $T: typeid) -> Value {
   #partial switch right.type.kind {
    case .U8:              return left.value.(T) << right.value.(u8);
    case .U16:             return left.value.(T) << right.value.(u16);
    case .U32:             return left.value.(T) << right.value.(u32);
    case .U64:             return left.value.(T) << right.value.(u64);
    case .UInt:            return left.value.(T) << right.value.(uint);
    case .Untyped_Integer: return left.value.(T) << right.value.(u64);
  }
  assert(false);
  return nil;
}

/**
* Resolves a binary right shift operation.
*
* @param left  The left constant value operand.
* @param right The right constant value operand.
* @param T     The type of the left operand.
* @return The evaluated value of the operation.
*/
resolver_evaluate_operation_binary_shift_right :: proc(left: Operand, right: Operand, $T: typeid) -> Value {
   #partial switch right.type.kind {
    case .U8:              return left.value.(T) >> right.value.(u8);
    case .U16:             return left.value.(T) >> right.value.(u16);
    case .U32:             return left.value.(T) >> right.value.(u32);
    case .U64:             return left.value.(T) >> right.value.(u64);
    case .UInt:            return left.value.(T) >> right.value.(uint);
    case .Untyped_Integer: return left.value.(T) >> right.value.(u64);
  }
  assert(false);
  return nil;
}
