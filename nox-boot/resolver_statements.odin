package nox

import "tracy"

/**
* The restriction of allowed statements in a routine.
*/
Statement_Restriction :: enum {
  Allow_All,       // Allow all statements.
  Allow_Only_Pure, // Allow only pure statements.
}

/**
* The restriction of allowed expressions.
*/
Expression_Restriction :: enum {
  Allow_All, // Allow all expressions.
  Restrict,  // Restrict expressions
}

/**
* Allow legal statements.
*/
Legal_Statement :: enum {
  Return,                                       // The return statement.
  Break,                                        // The break statement.
  Continue,                                     // The continue statement.
  Fallthrough,                                  // The fallthrough statement.
  Defer,                                        // The defer statement.
  Initialize,                                   // The initialize statement.
  
  Can_Refer_To_Enum_Item,                       // Can an enum item be referred to?
  Can_Call_Routine_With_Nox_Calling_Convention, // Can a routine with the 'nox' calling convention be called?
}

Legal_Statements :: bit_set[Legal_Statement];

/**
* Represents a statement context.
*/
Statement_Context :: struct {
  statement_restriction: Statement_Restriction,   // The statement restriction.
  expression_restriction: Expression_Restriction, // The expression restriction.
  legal_statements: Legal_Statements,             // The allowed legal statements.
}

/**
* The kind of a scope to create.
*/
Scope_Kind :: enum {
  New_Scope,  // Creates a new scope.
  Keep_Scope, // Keeps the current scope.
}

/**
* The default statement context.
*/
STATEMENT_CONTEXT :: Statement_Context{.Allow_All, .Restrict, {.Return, .Defer, .Initialize, .Can_Call_Routine_With_Nox_Calling_Convention}};

/**
* Resolves a statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  switch statement.kind {
    case .Return: resolver_resolve_statement_return(resolver, statement, statement_context, return_type); return true;
    case .Break: resolver_resolve_statement_break(resolver, statement, statement_context); return false;
    case .Continue: resolver_resolve_statement_continue(resolver, statement, statement_context); return false;
    case .Fallthrough: resolver_resolve_statement_fallthrough(resolver, statement, statement_context); return false;
    case .Defer: resolver_resolve_statement_defer(resolver, statement, statement_context, return_type); return false;
    case .Push_Context: return resolver_resolve_statement_push_context(resolver, statement, statement_context, return_type);

    case .Scope: return resolver_resolve_statement_block(resolver, (cast(^Statement_Scope) statement).block, statement_context, return_type, .New_Scope);
    case .If: return resolver_resolve_statement_if(resolver, statement, statement_context, return_type);
    case .Static_If: return resolver_resolve_statement_static_if(resolver, statement, statement_context, return_type);
    case .For: resolver_resolve_statement_for(resolver, statement, statement_context, return_type); return false;
    case .Foreach: resolver_resolve_statement_foreach(resolver, statement, statement_context, return_type); return false;
    case .Switch: return resolver_resolve_statement_switch(resolver, statement, statement_context, return_type);

    case .Assign: resolver_resolve_statement_assign(resolver, statement, statement_context); return false;
    case .Initialize: resolver_resolve_statement_initialize(resolver, statement, statement_context); return false;
    case .Expression: resolver_resolve_statement_expression(resolver, statement, statement_context); return false;

    case .None: fallthrough
    case: assert(false);
  }

  return false;
}

/**
* Resolves a return statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The return statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
*/
resolver_resolve_statement_return :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Return not_in statement_context.legal_statements {
    report_error_fatal(statement.position, "Illegal return statement");
  }

  return_expression := (cast(^Statement_Return) statement).expression;
  if return_expression != nil {
    if (type_is_void(return_type)) {
      report_error_fatal(statement.position, "Trying to return a value from routine which has no return type");
    }

    operand := resolver_resolve_expression_expected(resolver, return_expression, statement_context, return_type);
    if !operand_convert(resolver, return_expression.position, &operand, return_type) {
      report_error_fatal(statement.position, "Invalid type in return expression. Expected '%v' got '%v'", return_type.name, operand.type.name);
    }
    resolver_set_resolved_overwrite_type(resolver, return_expression, operand.type);
  } else if !type_is_void(return_type) {
    report_error_fatal(statement.position, "Empty return expression for routine with non-void return type");
  }
}

/**
* Resolves a break statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The break statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_break :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Break not_in statement_context.legal_statements {
    report_error_fatal(statement.position, "Illegal break statement");
  }
}

/**
* Resolves a continue statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The continue statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_continue :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Continue not_in statement_context.legal_statements {
    report_error_fatal(statement.position, "Illegal continue statement");
  }
}

/**
* Resolves a fallthrough statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The fallthrough statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_fallthrough :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Fallthrough not_in statement_context.legal_statements {
    report_error_fatal(statement.position, "Illegal fallthrough statement");
  }
}

/**
* Resolves a defer statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The defer statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
*/
resolver_resolve_statement_defer :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Defer not_in statement_context.legal_statements {
    report_error_fatal(statement.position, "Illegal defer statement");
  }

  statement_defer := cast(^Statement_Defer) statement;
  
  new_statement_context := statement_context;
  new_statement_context.legal_statements = {.Can_Call_Routine_With_Nox_Calling_Convention}; // We disallow every special statement inside a defer statement.

  resolver_resolve_statement(resolver, statement_defer.statement, new_statement_context, return_type)
} 

/**
* Resolves a push context statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The push context statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement_push_context :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_push_context := cast(^Statement_Push_Context) statement;

  operand := resolver_resolve_expression(resolver, statement_push_context.expression, statement_context);
  context_type := resolver.storage.cached_runtime_types.context_struct;
  if operand.type != context_type {
    report_error_fatal(statement_push_context.expression.position, "Need type 'Context' in push_context statement. Got '%v'", operand.type.name);
  }

  resolver_enter_local_scope(resolver);
  context_pointer_type := type_storage_get_or_make_type_pointer(resolver.storage, context_type);
  resolver_add_local_symbol(resolver, statement.position, SPECIAL_NAME_CONTEXT, context_pointer_type);
  new_statement_context := statement_context;
  new_statement_context.legal_statements += {.Can_Call_Routine_With_Nox_Calling_Convention};
  returns := resolver_resolve_statement_block(resolver, statement_push_context.block, new_statement_context, return_type, .Keep_Scope);
  resolver_leave_local_scope(resolver);

  return returns;
}

/**
* Resolves a block statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The block statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @param scope_kind        The kind of the scope to create.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement_block :: proc(
  resolver: ^Resolver,
  block: Statement_Block,
  statement_context: Statement_Context,
  return_type: ^Type,
  scope_kind: Scope_Kind,
) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if scope_kind == .New_Scope {
    resolver_enter_local_scope(resolver);
  }

  new_statement_context := statement_context;
  new_statement_context.legal_statements += {.Defer, .Initialize}; // Inside a block defer and initialize is always legal.

  returns := false;
  for statement, i in block.statements {
    returns = resolver_resolve_statement(resolver, statement, statement_context, return_type) || returns;

    // We try to detect dead code here, which occurs when there are:
    //   1. Statements after 'return'
    //   2. Statements after 'break'
    //   3. Statements after 'continue'
    //   4. Statements after 'fallthrough'
    // This does not however detect dead code after fully returning blocks like a returning if/else block.
    
    is_last_statement := i == len(block.statements) - 1;
    if (statement.kind == .Return && !is_last_statement) || (statement.kind == .Static_If && !is_last_statement && returns) {
      report_error_fatal(statement.position, "Statements after 'return' are never executed");
    } else if statement.kind == .Break && !is_last_statement {
      report_error_fatal(statement.position, "Statements after 'break' are never executed");
    } else if statement.kind == .Continue && !is_last_statement {
      report_error_fatal(statement.position, "Statements after 'continue' are never executed");
    } else if statement.kind == .Fallthrough && !is_last_statement {
      report_error_fatal(statement.position, "Statements after 'fallthrough' are never executed");
    }
  }

  if scope_kind == .New_Scope {
    resolver_leave_local_scope(resolver);
  }

  return returns;
}

/**
* Resolves an if statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The if statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement_if :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_if := cast(^Statement_If) statement;
  
  returns := false;
  resolver_resolve_expression_condition(resolver, statement_if.condition, statement_context);

  returns = resolver_resolve_statement_block(resolver, statement_if.then_block, statement_context, return_type, .New_Scope);

  for else_if in statement_if.else_ifs {
    resolver_resolve_expression_condition(resolver, else_if.condition, statement_context);
    returns = resolver_resolve_statement_block(resolver, else_if.block, statement_context, return_type, .New_Scope) && returns;
  }

  if len(statement_if.else_block.statements) > 0 {
    returns = resolver_resolve_statement_block(resolver, statement_if.else_block, statement_context, return_type, .New_Scope) && returns;
  } else {
    returns = false;
  }

  return returns;
}

/**
* Resolves a static if statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The static if statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement_static_if :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_if := cast(^Statement_If) statement;
  
  condition := resolver_resolve_directive_condition(resolver, statement_if.condition);
  resolver_set_resolved_constant(resolver, statement_if.condition, condition);
  if condition.value.(bool) {
    return resolver_resolve_statement_block(resolver, statement_if.then_block, statement_context, return_type, .Keep_Scope);
  } else {
    for else_if in statement_if.else_ifs {
      else_if_condition := resolver_resolve_directive_condition(resolver, else_if.condition);
      resolver_set_resolved_constant(resolver, else_if.condition, else_if_condition);
      if else_if_condition.value.(bool) {
        return resolver_resolve_statement_block(resolver, else_if.block, statement_context, return_type, .Keep_Scope);
      }
    }

    return resolver_resolve_statement_block(resolver, statement_if.else_block, statement_context, return_type, .Keep_Scope);
  }
}

/**
* Resolves a for statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The for statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
*/
resolver_resolve_statement_for :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_for := cast(^Statement_For) statement;

  resolver_enter_local_scope(resolver);
  {
    if statement_for.initializer != nil {
      new_statement_context := statement_context;
      new_statement_context.expression_restriction = .Allow_All;
      resolver_resolve_statement(resolver, statement_for.initializer, new_statement_context, return_type);
    }
    if statement_for.condition != nil {
      resolver_resolve_expression_condition(resolver, statement_for.condition, statement_context);
    }
    if statement_for.next != nil {
      new_statement_context := statement_context;
      new_statement_context.expression_restriction = .Allow_All;
      resolver_resolve_statement(resolver, statement_for.next, new_statement_context, return_type);
    }

    new_statement_context := statement_context;
    new_statement_context.legal_statements += {.Break, .Continue};
    // We keep the scope we have introduced with the for-statement.
    resolver_resolve_statement_block(resolver, statement_for.block, new_statement_context, return_type, .Keep_Scope);
  }
  resolver_leave_local_scope(resolver);
}

/**
* Resolves a foreach statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The foreach statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
*/
resolver_resolve_statement_foreach :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_foreach := cast(^Statement_Foreach) statement;
  
  resolver_enter_local_scope(resolver);
  {
    collection_operand := resolver_resolve_expression_rvalue(resolver, statement_foreach.collection, statement_context);

    collection_type := collection_operand.type;
    resolver_complete_type(resolver, statement.position, collection_type);
    if type_is_absolute_pointer(collection_type) || type_is_self_relative_pointer(collection_type) {
      collection_type = collection_type.base;
    }

    is_map := type_is_map(collection_type);
    if !type_is_array(collection_type) && !type_is_dynamic_array(collection_type) && !type_is_slice(collection_type) && !is_map {
      report_error_fatal(statement.position, "Foreach only works on arrays, dynamic arrays, slices and maps. Got '%v'", collection_type.name);
    }
    
    element_type := collection_type.base;
    if is_map {
      element_type = (cast(^Type_Map) collection_type).key;
    } else if type_is_soa(collection_type) {
      element_type = type_storage_get_or_make_type_soa_pointer(resolver.storage, collection_type);
    } else if type_is_aosoa(collection_type) {
      element_type = type_storage_get_or_make_type_aosoa_pointer(resolver.storage, collection_type);
    }

    resolver_complete_type(resolver, statement.position, element_type);
    if statement_foreach.element_name != SPECIAL_NAME_DISCARD {
      resolver_add_local_symbol(resolver, statement.position, statement_foreach.element_name, element_type, {.Local_Declared, .Local_Immutable});
    }
    if statement_foreach.index_name != "" && statement_foreach.index_name != SPECIAL_NAME_DISCARD {
      index_type := is_map ? (cast(^Type_Map) collection_type).value : resolver.storage.type_int;
      resolver_add_local_symbol(resolver, statement.position, statement_foreach.index_name, index_type, {.Local_Declared, .Local_Immutable});
    }

    new_statement_context := statement_context;
    new_statement_context.legal_statements += {.Break, .Continue};
    // We keep the scope we have introduced with the foreach-statement.
    resolver_resolve_statement_block(resolver, statement_foreach.block, new_statement_context, return_type, .Keep_Scope);
  }
  resolver_leave_local_scope(resolver);
}

/**
* Resolves a switch statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The switch statement to resolve.
* @param statement_context The statement context to use.
* @param return_type       The return type to expect.
* @return True if the statement returns otherwise false.
*/
resolver_resolve_statement_switch :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context, return_type: ^Type) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_switch := cast(^Statement_Switch) statement;

  operand := resolver_resolve_expression_rvalue(resolver, statement_switch.expression, statement_context);
  operand_remove_untyped(resolver.storage, statement_switch.expression.position, &operand);
  if !type_is_integer(operand.type) && !type_is_char(operand.type) && !type_is_enumeration(operand.type) {
    report_error_fatal(statement.position, "Switch expression must have an integer, char or enumeration type. Got '%v'", operand.type.name);
  }

  new_statement_context := statement_context;
  new_statement_context.legal_statements += {.Break, .Fallthrough};

  // NOTE: We currently do not detect if we return when 'fallthrough' statements are present.
  returns := true;
  has_default_case := false;

  switch_case_constants_map := make_map(map[Operand]Operand, len(statement_switch.cases), context.temp_allocator);
  for switch_case in statement_switch.cases {
    if len(switch_case.patterns) == 0 do has_default_case = true;
    for pattern in switch_case.patterns {
      case_operand := resolver_resolve_expression_constant_expected(resolver, pattern, statement_context, operand.type);

      if !operand_convert(resolver, pattern.position, &case_operand, operand.type) {
        report_error_fatal(pattern.position, "Invalid type in switch case expression. Expected: '%v' got '%v'", case_operand.type.name, operand.type.name);
      }

      _,found := switch_case_constants_map[case_operand];
      if found {
        report_error_fatal(pattern.position, "Duplicate case value '%v'", case_operand.value);
      }

      switch_case_constants_map[case_operand] = case_operand;
    }

    returns = resolver_resolve_statement_block(resolver, switch_case.block, new_statement_context, return_type, .New_Scope) && returns;
  }

  return returns && has_default_case;
}

/**
* Resolves a assignment statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The assignment statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_assign :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_assign := cast(^Statement_Assign) statement;

  if len(statement_assign.left_expressions) == 1 {
    resolver_resolve_statement_assign_simple(resolver, statement, statement_context);
  } else {
    resolver_resolve_statement_assign_tuple(resolver, statement, statement_context);
  }
}

/**
* Resolves a simple assignment statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The simple assignment statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_assign_simple :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_assign := cast(^Statement_Assign) statement;

  expression_left := statement_assign.left_expressions[0];
  expression_right := statement_assign.right_expression;

  left := resolver_resolve_expression(resolver, expression_left, statement_context);
  if .Is_LValue not_in left.flags {
    report_error_fatal(statement.position, "Cannot assign to non-lvalue");
  }
  if .Is_Immutable in left.flags {
    report_error_fatal(statement.position, "Cannot assign to immutable");
  }
  if .Is_Parameter in left.flags {
    report_error_fatal(statement.position, "Cannot assign to parameter");
  }

  result: Operand;
  right := resolver_resolve_expression_expected_rvalue(resolver, expression_right, statement_context, left.type);
  if statement_assign.operator == .Assign {
    result = right;
  } else {
    binary_operation := ASSIGN_TOKEN_TO_BINARY_TOKEN[statement_assign.operator];
    result = resolver_resolve_expression_binary_operation(resolver, expression_right.position, binary_operation, left, right, expression_left, expression_right);
  }

  if !operand_convert(resolver, expression_right.position, &result, left.type) {
    report_error_fatal(statement.position, "Invalid type in assignment. Expected '%v' got '%v'", left.type.name, result.type.name);
  }

  resolver_set_resolved_overwrite_type(resolver, expression_right, result.type);
}

/**
* Resolves a tuple assignment statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The tuple assignment statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_assign_tuple :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_assign := cast(^Statement_Assign) statement;

  if statement_assign.operator != .Assign {
    report_error_fatal(statement.position, "Multiple names can only be assigned with '='. Got '%v'", lexer_token_kind_name(statement_assign.operator));
  }

  right := resolver_resolve_expression_rvalue(resolver, statement_assign.right_expression, statement_context);
  right_type := right.type;
  if !type_is_tuple(right_type) {
    report_error_fatal(statement.position, "Multiple names can only be assigned by a tuple type. Got '%v'", right.type.name);
  }

  type_tuple := cast(^Type_Tuple) right_type;

  if len(type_tuple.elements) != len(statement_assign.left_expressions) {
    report_error_fatal(statement.position, "Invalid number of names in tuple assign statement. Expected %v got %v", len(type_tuple.elements), len(statement_assign.left_expressions));
  }

  for element, i in type_tuple.elements {
    element_type := element.type;
    left_expression := statement_assign.left_expressions[i];

    left := resolver_resolve_expression(resolver, left_expression, statement_context);
    if .Is_LValue not_in left.flags {
      report_error_fatal(statement.position, "Cannot assign to non-lvalue");
    }
    if .Is_Immutable in left.flags {
      report_error_fatal(statement.position, "Cannot assign to immutable");
    }
    if .Is_Parameter in left.flags {
      report_error_fatal(statement.position, "Cannot assign to parameter");
    }
    if left.type != element_type {
      report_error_fatal(statement.position, "Invalid type in %v. tuple assignment. Expected '%v' got '%v' from tuple", i + 1, left.type.name, element_type.name);
    }
  }
}

/**
* Resolves an initialize statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The initialize statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_initialize :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_initialize := cast(^Statement_Initialize) statement;

  type := resovler_resolve_initializer(resolver, statement.position, statement_context, statement_initialize.type, statement_initialize.expression);
  if len(statement_initialize.names) == 1 {
    name := statement_initialize.names[0];
    if resolver_resolve_statement_initialize_check_naming(resolver, statement.position, name) {
      resolver_add_local_symbol(resolver, statement.position, name, type, {.Local_Declared});
    }
  } else if type_is_tuple(type) {
    type_tuple := cast(^Type_Tuple) type;

    if len(type_tuple.elements) != len(statement_initialize.names) {
      report_error_fatal(statement.position, "Invalid number of names in tuple initialize statement. Expected %v got %v", len(type_tuple.elements), len(statement_initialize.names));
    }

    for element, i in type_tuple.elements {
      name := statement_initialize.names[i];
      if resolver_resolve_statement_initialize_check_naming(resolver, statement.position, name) {
        resolver_add_local_symbol(resolver, statement.position, name, element.type, {.Local_Declared});
      }
    }
  } else {
    report_error_fatal(statement.position, "Multiple names can only be initialized with a tuple type. Got '%v'", type.name);
  }
}

/**
* Checks the naming of an initialize statement.
*
* @param resolver The reference to the resolver.
* @param position The position of the initialized variable.
* @param name     The name of the initialized variable.
* @return True if the variable should be created otherwise false.
*/
resolver_resolve_statement_initialize_check_naming :: proc(resolver: ^Resolver, position: Source_Position, name: string) -> bool {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_routine := cast(^Type_Routine) resolver.current_routine.type;

  if type_routine_has_context(type_routine) && name == SPECIAL_NAME_CONTEXT {
    report_error_fatal(position, "Local variable named 'context' is not allowed in routine with calling convention 'Nox'");
    return false;
  }
  if .Routine_Method_Implemented in resolver.current_routine.flags && name == SPECIAL_NAME_SELF {
    report_error_fatal(position, "Local variable named 'self' is not allowed in method");
    return false;
  }
  if name == SPECIAL_NAME_DISCARD {
    return false;
  }

  return true;
}

/**
* Resolves an expression statement.
*
* @param resolver          The reference to the resolver.
* @param statement         The expression statement to resolve.
* @param statement_context The statement context to use.
*/
resolver_resolve_statement_expression :: proc(resolver: ^Resolver, statement: ^Statement, statement_context: Statement_Context) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expression := (cast(^Statement_Expression) statement).expression;

  resolver_resolve_expression(resolver, expression, statement_context);

  // We want to check for expressions which are unused.
  if statement_context.expression_restriction == .Restrict {
    is_unused_statement := expression.kind != .Call && expression.kind != .Modify;
    if is_unused_statement {
      report_error_fatal(expression.position, "Expression in statement is not being used");
    }
  }
}
