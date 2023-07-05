package nox

import "tracy"

/**
* Emit a statement.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param statement         The statement to emit.
* @param is_last_statement Is the statement the last in the current block?
* @return 1. True if the statement returns otherwise false; 2. True if the statement leaves otherwise false.
*/
generator_emit_statement :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement,
  is_last_statement: bool,
) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_debug_emit_location(generator, routine_context, statement.position);

  switch statement.kind {
    case .Return: generator_emit_statement_return(generator, routine_context, statement); return true, false;
    case .Break: generator_emit_statement_break(generator, routine_context); return false, true;
    case .Continue: generator_emit_statement_continue(generator, routine_context); return false, true;
    case .Fallthrough: generator_emit_statement_fallthrough(generator, routine_context); return false, true;
    case .Defer: generator_emit_statement_defer(routine_context, statement); return false, false;
    case .Push_Context: return generator_emit_statement_push_context(generator, routine_context, statement);
    
    case .Scope: return generator_emit_statement_block(generator, routine_context, (cast(^Statement_Scope) statement).block, .New_Scope);
    case .If: return generator_emit_statement_if(generator, routine_context, statement, is_last_statement);
    case .Static_If: return generator_emit_statement_static_if(generator, routine_context, statement);
    case .For: generator_emit_statement_for(generator, routine_context, statement); return false, false;
    case .Foreach: generator_emit_statement_foreach(generator, routine_context, statement); return false, false;
    case .Switch: return generator_emit_statement_switch(generator, routine_context, statement, is_last_statement);

    case .Assign: generator_emit_statement_assign(generator, routine_context, statement); return false, false;
    case .Initialize: generator_emit_statement_initialize(generator, routine_context, statement); return false, false;
    case .Expression: generator_emit_expression(generator, routine_context, (cast(^Statement_Expression) statement).expression, nil); return false, false;

    case .None: fallthrough;
    case: assert(false);
  }

  return false, false;
}

/**
* Emit a return statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_return :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  return_expression := (cast(^Statement_Return) statement).expression;

  if return_expression == nil {
    generator_emit_deferred_statements_all(generator, routine_context);
    LLVMBuildRetVoid(generator.builder);
    return;
  }

  return_type := routine_context.return_type;
  
  return_expression_type := return_type
  if type_is_union(return_expression_type) {
    return_expression_type = generator_get_resolved_overwrite_type(generator, return_expression);
  }
  needs_to_load_address := generator_type_requires_loading_address(return_type, return_expression_type);
  previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  llvm_return_value := generator_emit_expression(generator, routine_context, return_expression, return_expression_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  
  // Deferred statements are executed AFTER the return expression.
  // That way the return expression may for example refer to things which are then being deleted/freed in a deferred statement.
  generator_emit_deferred_statements_all(generator, routine_context);

  // Structs or unions which are being returned by value are handled as an out-parameter.
  if generator_type_is_passed_as_pointer(return_type) {
    // This assumes the return parameter will always be the first one.
    llvm_return_parameter := LLVMGetFirstParam(routine_context.llvm_symbol);
    routine_context.is_storing_byval_or_sret = true;
    generator_emit_store(generator, routine_context, return_type, llvm_return_parameter, llvm_return_value, return_expression);
    routine_context.is_storing_byval_or_sret = false;
    LLVMBuildRetVoid(generator.builder);
  } else {
    LLVMBuildRet(generator.builder, llvm_return_value);
  }
}

/**
* Emit a break statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_statement_break :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_emit_deferred_statements_until_control(generator, routine_context);

  assert(routine_context.last_break_block != nil);
  LLVMBuildBr(generator.builder, routine_context.last_break_block);
}

/**
* Emit a continue statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_statement_continue :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_emit_deferred_statements_until_control(generator, routine_context);
  
  assert(routine_context.last_continue_block != nil);
  LLVMBuildBr(generator.builder, routine_context.last_continue_block);
}

/**
* Emit a fallthrough statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
*/
generator_emit_statement_fallthrough :: proc(generator: ^Generator, routine_context: ^Routine_Context) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  generator_emit_deferred_statements_until_control(generator, routine_context);

  assert(routine_context.next_fallthrough_block != nil);
  LLVMBuildBr(generator.builder, routine_context.next_fallthrough_block);
}

/**
* Emit a defer statement.
* 
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_defer :: proc(routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_defer := cast(^Statement_Defer) statement;

  append(&routine_context.local_scope.deferred_statements, statement_defer.statement);
}

/**
* Emit a push context statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
* @return 1. True if the statement returns otherwise false; 2. True if the statement leaves otherwise false.
*/
generator_emit_statement_push_context :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_push_context := cast(^Statement_Push_Context) statement;
  
  context_type := generator.storage.cached_runtime_types.context_struct;

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_new_context_to_push := generator_emit_expression(generator, routine_context, statement_push_context.expression, context_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  generator_enter_local_scope(generator, routine_context, .Block, statement.position);
  
  context_pointer_type := type_storage_get_or_make_type_pointer(generator.storage, context_type);
  llvm_new_context := generator_emit_local(generator, routine_context, statement.position, context_pointer_type, SPECIAL_NAME_CONTEXT);
  generator_emit_store_raw(generator, llvm_new_context, llvm_new_context_to_push);

  returns, leaves = generator_emit_statement_block(generator, routine_context, statement_push_context.block, .Keep_Scope);

  generator_leave_local_scope(generator, routine_context, returns, leaves);

  return returns, leaves;
}

/**
* Emit a statement block.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param block           The statement block to emit.
* @param scope_kind      The kind of the scope.
* @return 1. True if the statement block returns otherwise false; 2. True if the statement block leaves otherwise false.
*/
generator_emit_statement_block :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  block: Statement_Block,
  scope_kind: Scope_Kind,
) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  if scope_kind == .New_Scope {
    generator_enter_local_scope(generator, routine_context, .Block, block.start_position);
  }

  returns = false;
  leaves = false;
  for statement, i in block.statements {
    is_last_statement := i == len(block.statements) - 1;
    returns, leaves = generator_emit_statement(generator, routine_context, statement, is_last_statement);
  }

  if scope_kind == .New_Scope {
    generator_leave_local_scope(generator, routine_context, returns, leaves);
  }

  return returns, leaves;
}

/**
* Emit an if statement.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param statement         The statement to emit.
* @param is_last_statement Is the statement the last in the current block?
* @return 1. True if the statement returns otherwise false; 2. True if the statement leaves otherwise false.
*/
generator_emit_statement_if :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement,
  is_last_statement: bool,
) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_if := cast(^Statement_If) statement;
  
  has_else := len(statement_if.else_block.statements) > 0;
  has_else_ifs := len(statement_if.else_ifs) > 0;
  needs_else_block := has_else || has_else_ifs;
  // There are two cases where we need an 'after' block.
  //   1. We are not the last statement in the block -> quite obvious
  //   2. We might be the last statement if we are returning void.
  needs_after_block := !is_last_statement || routine_context.needs_after_block;

  returns = false;

  // First append all the blocks we will need.

  // This block is always needed.
  then_block := generator_append_block(routine_context);

  else_block: LLVMBasicBlockRef;
  if needs_else_block {
    else_block = generator_append_block(routine_context);
  }

  previous_next_block := routine_context.last_next_block;
  after_block := routine_context.last_next_block;
  if needs_after_block {
    after_block = generator_append_block(routine_context);
    routine_context.last_next_block = after_block;
    if !needs_else_block {
      else_block = after_block;
    }
  } else if !needs_else_block {
    else_block = routine_context.last_next_block;
  }

  // Generate the conditional branch.
  condition_value := generator_emit_boolean_truncation(generator, routine_context, statement_if.condition, generator.storage.type_bool);
  LLVMBuildCondBr(generator.builder, condition_value, then_block, else_block);

  {
    previous_block := generator_enter_llvm_block(generator, routine_context, then_block);
    then_returns, then_leaves := generator_emit_statement_block(generator, routine_context, statement_if.then_block, .New_Scope);
    if !then_returns && !then_leaves {
      llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
      if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
        LLVMBuildBr(generator.builder, after_block);
      }
    }
    returns = then_returns;
    generator_leave_llvm_block(routine_context, previous_block);
  }

  if needs_else_block {
    previous_block := generator_enter_llvm_block(generator, routine_context, else_block);

    // Every 'else if' we have needs to be converted into a regular 'if/else' statement.
    else_if_else_block: LLVMBasicBlockRef = nil;
    for else_if, i in statement_if.else_ifs {
      is_last_else_if := i == len(statement_if.else_ifs) - 1;

      // Prepare the next else block (if we need one).
      if is_last_else_if {
        if has_else {
          else_if_else_block = generator_append_block(routine_context);
        } else {
          else_if_else_block = after_block;
        }
      } else {
        else_if_else_block = generator_append_block(routine_context);
      }

      else_if_returns, else_if_leaves := generator_emit_statement_if_else_if(generator, routine_context, else_if, else_if_else_block, after_block);
      returns = else_if_returns && returns;

      generator_enter_llvm_block(generator, routine_context, else_if_else_block);
    }

    if has_else {
      else_returns, else_leaves := generator_emit_statement_block(generator, routine_context, statement_if.else_block, .New_Scope);
      if !else_returns && !else_leaves {
        llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
        if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
          LLVMBuildBr(generator.builder, after_block);
        }
      }
      returns = else_returns && returns;
    }

    generator_leave_llvm_block(routine_context, previous_block)
  }

  if needs_after_block {
    // If we fully return that means that we don't need the after block anymore (as it will never be reached and is never referenced).
    // But this is only the case when we don't have an additional else block.
    // Because then we reference the after block in our branch condition and can't delete it.
    previous_block := generator_enter_llvm_block(generator, routine_context, after_block);
    if returns && is_last_statement {
      if (else_block == after_block) {
        if (previous_next_block == nil) {
          LLVMBuildRetVoid(generator.builder);
        } else {
          LLVMBuildBr(generator.builder, previous_next_block);
        }
      } else {
        LLVMDeleteBasicBlock(after_block);
      }
      generator_leave_llvm_block(routine_context, previous_block);
    }
  }

  routine_context.last_next_block = previous_next_block;

  return returns, false;
}

/**
* Emit an else if block of an if statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param else_if         The else if block to emit.
* @param else_block      The LLVM else block of the if statement.
* @param after_block     The LLVM after block of the if statement.
* @return 1. True if the else if block returns otherwise false; 2. True if the else if block leaves otherwise false.
*/
generator_emit_statement_if_else_if :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  else_if: Statement_If_Else,
  else_block: LLVMBasicBlockRef,
  after_block: LLVMBasicBlockRef,
) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  then_block := generator_append_block(routine_context);

  condition_value := generator_emit_boolean_truncation(generator, routine_context, else_if.condition, generator.storage.type_bool);
  LLVMBuildCondBr(generator.builder, condition_value, then_block, else_block);

  previous_block := generator_enter_llvm_block(generator, routine_context, then_block);
  else_if_returns, else_if_leaves := generator_emit_statement_block(generator, routine_context, else_if.block, .New_Scope);
  if !else_if_returns && !else_if_leaves {
    llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
    if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
      LLVMBuildBr(generator.builder, after_block);
    }
  }
  generator_leave_llvm_block(routine_context, previous_block);

  return else_if_returns, else_if_leaves;
}

/**
* Emit a static if statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
* @return 1. True if the statement returns otherwise false; 2. True if the statement leaves otherwise false.
*/
generator_emit_statement_static_if :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) ->  (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_if := cast(^Statement_If) statement;

  condition, _ := generator_get_resolved_constant(generator, statement_if.condition);
  if condition.value.(bool) {
    return generator_emit_statement_block(generator, routine_context, statement_if.then_block, .Keep_Scope);
  } else {
    for else_if in statement_if.else_ifs {
      else_if_condition, _ := generator_get_resolved_constant(generator, else_if.condition);
      if else_if_condition.value.(bool) {
        return generator_emit_statement_block(generator, routine_context, else_if.block, .Keep_Scope);
      }
    }

    return generator_emit_statement_block(generator, routine_context, statement_if.else_block, .Keep_Scope);
  }
}

/**
* Emit a for statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_for :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_for := cast(^Statement_For) statement;

  has_next := statement_for.next != nil;
  has_condition := statement_for.condition != nil;

  loop_block := generator_append_block(routine_context);
  previous_continue_block := routine_context.last_continue_block;
  routine_context.last_continue_block = loop_block;

  // A for loop will always have a block after it.
  after_block := generator_append_block(routine_context);
  previous_next_block := routine_context.last_next_block;
  routine_context.last_next_block = loop_block;
  previous_break_block := routine_context.last_break_block;
  routine_context.last_break_block = after_block;

  // Loops handle their own scope.
  generator_enter_local_scope(generator, routine_context, .Control_Block, statement.position);

  if statement_for.initializer != nil {
    generator_emit_statement(generator, routine_context, statement_for.initializer, false);
  }
  
  condition_block: LLVMBasicBlockRef;
  if has_condition {
    condition_block = generator_append_block(routine_context);
    routine_context.last_next_block = condition_block;
    routine_context.last_continue_block = condition_block;
    LLVMBuildBr(generator.builder, condition_block);

    previous_block := generator_enter_llvm_block(generator, routine_context, condition_block);

    LLVMPositionBuilderAtEnd(generator.builder, condition_block);
    condition_value := generator_emit_boolean_truncation(generator, routine_context, statement_for.condition, generator.storage.type_bool);
    LLVMBuildCondBr(generator.builder, condition_value, loop_block, after_block);

    generator_leave_llvm_block(routine_context, previous_block);
  } else {
    LLVMBuildBr(generator.builder, loop_block);
  }

  next_block: LLVMBasicBlockRef;
  if has_next {
    next_block = generator_append_block(routine_context);
    routine_context.last_next_block = next_block;
    routine_context.last_continue_block = next_block;
    previous_block := generator_enter_llvm_block(generator, routine_context, next_block);

    generator_emit_statement(generator, routine_context, statement_for.next, false)
    if has_condition {
      LLVMBuildBr(generator.builder, condition_block);
    } else {
      LLVMBuildBr(generator.builder, loop_block);
    }

    generator_leave_llvm_block(routine_context, previous_block);
  }

  {
    previous_block := generator_enter_llvm_block(generator, routine_context, loop_block);
    block_returns, block_leaves := generator_emit_statement_block(generator, routine_context, statement_for.block, .Keep_Scope);

    // We leave the scope before building the branches, so that defer statements can be properly generated.
    generator_leave_local_scope(generator, routine_context, block_returns, block_leaves);

    if !block_returns && !block_leaves {
      llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
      if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
        if has_next {
          LLVMBuildBr(generator.builder, next_block);
        } else if has_condition {
          LLVMBuildBr(generator.builder, condition_block);
        } else {
          LLVMBuildBr(generator.builder, loop_block);
        }
      }
    }
    generator_leave_llvm_block(routine_context, previous_block);
  }

  generator_enter_llvm_block(generator, routine_context, after_block);

  routine_context.last_next_block = previous_next_block;
  routine_context.last_break_block = previous_break_block;
  routine_context.last_continue_block = previous_continue_block;
}

/**
* Emit a foreach statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_foreach :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_foreach := cast(^Statement_Foreach) statement;

  llvm_loop_block := generator_append_block(routine_context);
  llvm_loop_next_block := generator_append_block(routine_context);
  
  previous_continue_block := routine_context.last_continue_block;
  routine_context.last_continue_block = llvm_loop_next_block;

  // A foreach loop will always have a block after it.
  llvm_after_block := generator_append_block(routine_context);
  previous_next_block := routine_context.last_next_block;
  routine_context.last_next_block = llvm_loop_next_block;
  previous_break_block := routine_context.last_break_block;
  routine_context.last_break_block = llvm_after_block;

  collection_type := generator_get_resolved_type(generator, statement_foreach.collection);
  
  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_collection := generator_emit_expression(generator, routine_context, statement_foreach.collection, collection_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  llvm_collection = generator_emit_possible_pointer_dereference(generator, routine_context, statement_foreach.collection, llvm_collection);
  if type_is_absolute_pointer(collection_type) || type_is_self_relative_pointer(collection_type) {
    collection_type = collection_type.base;
  }

  if type_is_map(collection_type) {
    generator_emit_statement_foreach_map(
      generator,
      routine_context,
      statement,
      collection_type,
      llvm_collection,
      llvm_loop_block,
      llvm_loop_next_block,
      llvm_after_block,
    );
  } else {
    generator_emit_statement_foreach_regular(
      generator,
      routine_context,
      statement,
      collection_type,
      llvm_collection,
      llvm_loop_block,
      llvm_loop_next_block,
      llvm_after_block,
    );
  }

  generator_enter_llvm_block(generator, routine_context, llvm_after_block);

  routine_context.last_next_block = previous_next_block;
  routine_context.last_break_block = previous_break_block;
  routine_context.last_continue_block = previous_continue_block;
}

/**
* Emit a regular foreach statement.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param statement            The statement to emit.
* @param collection_type      The type of the collection.
* @param llvm_collection      The LLVM type of the collection.
* @param llvm_loop_block      The LLVM loop block.
* @param llvm_loop_next_block The LLVM loop next block.
* @param llvm_after_block     The LLVM after block of the statement.
*/
generator_emit_statement_foreach_regular :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement,
  collection_type: ^Type,
  llvm_collection: LLVMValueRef,
  llvm_loop_block: LLVMBasicBlockRef,
  llvm_loop_next_block: LLVMBasicBlockRef,
  llvm_after_block: LLVMBasicBlockRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_foreach := cast(^Statement_Foreach) statement;
  
  llvm_collection_type := generator_type_to_llvm(generator, collection_type);
  is_iterating_layout_collection := type_is_soa_or_aosoa(collection_type);
  element_type := collection_type.base;
  if type_is_soa(collection_type) {
    element_type = type_storage_get_or_make_type_soa_pointer(generator.storage, collection_type);
  } else if type_is_aosoa(collection_type) {
    element_type = type_storage_get_or_make_type_aosoa_pointer(generator.storage, collection_type);
  }
  llvm_element_type := generator_type_to_llvm(generator, element_type);

  // Loops handle their own scope.
  generator_enter_local_scope(generator, routine_context, .Control_Block, statement.position);
  llvm_element := generator_emit_local(generator, routine_context, statement.position, element_type, statement_foreach.element_name);
  llvm_index: LLVMValueRef;
  if statement_foreach.index_name == "" {
    llvm_index = generator_emit_temporary(generator, routine_context, generator.storage.type_int, make_temp_name(routine_context));
  } else {
    llvm_index = generator_emit_local(generator, routine_context, statement.position, generator.storage.type_int, statement_foreach.index_name);
  }
  generator_emit_store_raw(generator, llvm_index, LLVMConstInt(LLVMInt64Type(), 0, false));

  // We get the length outside the loop. That way it is not subject to change, should the collection be modified.
  // The same goes for the data pointer. Adding elements to a dynamic array is undefined as it might reallocate and invalidate the data pointer.
  llvm_length: LLVMValueRef;
  llvm_data_pointer: LLVMValueRef;
  if type_is_array(collection_type) {
    llvm_length = LLVMConstInt(LLVMInt64Type(), cast(u64) (cast(^Type_Array) collection_type).number_of_elements, false);
    llvm_data_pointer = llvm_collection;
  } else {
    llvm_length_pointer: LLVMValueRef;
    if is_iterating_layout_collection {
      llvm_length_pointer = generator_emit_gep_layout_collection_length(generator.builder, routine_context, collection_type, llvm_collection_type, llvm_collection);
    } else {
      llvm_length_pointer = generator_emit_gep_value_length(generator.builder, routine_context, llvm_collection_type, llvm_collection);
      llvm_data_pointer_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_collection_type, llvm_collection);
      llvm_data_pointer = generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_data_pointer_pointer);
    }
    llvm_length = generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_length_pointer);
  }
  
  if is_iterating_layout_collection {
    llvm_data_pointer_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_collection_type, llvm_collection);
    llvm_layout_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_element_type, llvm_element);
    generator_emit_store_raw(generator, llvm_layout_pointer_data_pointer, llvm_data_pointer_pointer);
  }

  llvm_initial_condition := LLVMBuildICmp(generator.builder, .EQ, llvm_length, LLVMConstInt(LLVMInt64Type(), 0, false), make_value_name(routine_context));
  LLVMBuildCondBr(generator.builder, llvm_initial_condition, llvm_after_block, llvm_loop_block);

  {
    previous_block := generator_enter_llvm_block(generator, routine_context, llvm_loop_block);
    
    llvm_index_value := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_index);

    if is_iterating_layout_collection {
      llvm_layout_pointer_index_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_element_type, llvm_element);
      generator_emit_store_raw(generator, llvm_layout_pointer_index_pointer, llvm_index_value);
    } else {
      llvm_element_pointer := generator_emit_gep_pointer(generator.builder, routine_context, llvm_element_type, llvm_data_pointer, llvm_index_value);

      llvm_element_temporary: LLVMValueRef;
      if generator_type_requires_loading_address(element_type, element_type) {
        llvm_element_temporary = llvm_element_pointer;
      } else {
        llvm_element_temporary = generator_emit_load(generator, routine_context, element_type, llvm_element_pointer);
      }
      generator_emit_store(generator, routine_context, element_type, llvm_element, llvm_element_temporary, nil);
    }

    block_returns, block_leaves := generator_emit_statement_block(generator, routine_context, statement_foreach.block, .Keep_Scope);

    // We leave the scope before building the branches, so that defer statements can be properly generated.
    generator_leave_local_scope(generator, routine_context, block_returns, block_leaves);

    if !block_returns && !block_leaves {
      llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
      if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
        LLVMBuildBr(generator.builder, llvm_loop_next_block);
      }
    }
    generator_leave_llvm_block(routine_context, previous_block);

    previous_block = generator_enter_llvm_block(generator, routine_context, llvm_loop_next_block);
    llvm_index_incremented := LLVMBuildAdd(generator.builder, llvm_index_value, LLVMConstInt(LLVMInt64Type(), 1, false), make_value_name(routine_context));
    generator_emit_store_raw(generator, llvm_index, llvm_index_incremented);
    llvm_loop_condition := LLVMBuildICmp(generator.builder, .SLT, llvm_index_incremented, llvm_length, make_value_name(routine_context));
    LLVMBuildCondBr(generator.builder, llvm_loop_condition, llvm_loop_block, llvm_after_block);
    generator_leave_llvm_block(routine_context, previous_block);
  }
}

/**
* Emit a map foreach statement.
* 
* @param generator            The reference to the generator.
* @param routine_context      The context of the routine.
* @param statement            The statement to emit.
* @param collection_type      The type of the collection.
* @param llvm_collection      The LLVM type of the collection.
* @param llvm_loop_block      The LLVM loop block.
* @param llvm_loop_next_block The LLVM loop next block.
* @param llvm_after_block     The LLVM after block of the statement.
*/
generator_emit_statement_foreach_map :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement,
  collection_type: ^Type,
  llvm_collection: LLVMValueRef,
  llvm_loop_block: LLVMBasicBlockRef,
  llvm_loop_next_block: LLVMBasicBlockRef,
  llvm_after_block: LLVMBasicBlockRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_foreach := cast(^Statement_Foreach) statement;

  llvm_loop_capacity_check_block := llvm_loop_next_block; // The capacity check is the 'next' block for a foreach.
  llvm_get_next_entry_block := generator_append_block(routine_context);
  llvm_setup_entry_block := generator_append_block(routine_context);

  type_map := cast(^Type_Map) collection_type;
  key_type := type_map.key;
  value_type := type_map.value;
  llvm_collection_type := generator_type_to_llvm(generator, collection_type);

  // Loops handle their own scope.
  generator_enter_local_scope(generator, routine_context, .Control_Block, statement.position);
  llvm_key := generator_emit_local(generator, routine_context, statement.position, key_type, statement_foreach.element_name);
  llvm_value: LLVMValueRef;
  if statement_foreach.index_name != "" {
    llvm_value = generator_emit_local(generator, routine_context, statement.position, value_type, statement_foreach.index_name);
  }
  llvm_iterator_pointer := generator_emit_temporary_and_store_raw(
    generator,
    routine_context,
    generator.storage.type_int,
    LLVMConstInt(LLVMInt64Type(), 0, false),
    make_temp_name(routine_context),
  );

  llvm_data_pointer_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_collection_type, llvm_collection);
  llvm_capacity_pointer := generator_emit_gep_value_capacity(generator.builder, routine_context, llvm_collection_type, llvm_collection);

  LLVMBuildBr(generator.builder, llvm_loop_capacity_check_block);
  // This block checks if we reached the end of the loop (aka iterator < capacity).
  previous_block := generator_enter_llvm_block(generator, routine_context, llvm_loop_capacity_check_block);
  {
    llvm_iterator := generator_emit_load(generator, routine_context,  generator.storage.type_int, llvm_iterator_pointer);
    llvm_capacity := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_capacity_pointer);
    llvm_capacity_condition := LLVMBuildICmp(generator.builder, .SGE, llvm_iterator, llvm_capacity, make_value_name(routine_context));
    LLVMBuildCondBr(generator.builder, llvm_capacity_condition, llvm_after_block, llvm_get_next_entry_block);
  }
  generator_leave_llvm_block(routine_context, previous_block);

  // This corresponds to 'Map_Entry<K, V> { key: K, value: V, is_occupied: bool }'.
  llvm_key_type := generator_type_to_llvm(generator, key_type);
  llvm_value_type := generator_type_to_llvm(generator, value_type);
  llvm_map_entry_struct_elements: []LLVMTypeRef = {llvm_key_type, llvm_value_type, generator_type_to_llvm(generator, generator.storage.type_bool)};
  llvm_map_entry_type := LLVMStructType(raw_data(llvm_map_entry_struct_elements), cast(u32) len(llvm_map_entry_struct_elements), false);

  // This block gets the pointer to the next entry that is occupied.
  llvm_entry_pointer: LLVMValueRef;
  previous_block = generator_enter_llvm_block(generator, routine_context, llvm_get_next_entry_block);
  {
    llvm_iterator := generator_emit_load(generator, routine_context,  generator.storage.type_int, llvm_iterator_pointer);
    llvm_data_pointer := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_data_pointer_pointer);
    llvm_entry_pointer = generator_emit_gep_pointer(generator.builder, routine_context, llvm_map_entry_type, llvm_data_pointer, llvm_iterator);
    llvm_is_occupied_pointer := generator_emit_gep_value_capacity(generator.builder, routine_context, llvm_map_entry_type, llvm_entry_pointer);
    llvm_is_occupied := generator_emit_load(generator, routine_context, generator.storage.type_bool, llvm_is_occupied_pointer);
    llvm_iterator_incremented := LLVMBuildAdd(generator.builder, llvm_iterator, LLVMConstInt(LLVMInt64Type(), 1, false), make_value_name(routine_context));
    generator_emit_store_raw(generator, llvm_iterator_pointer, llvm_iterator_incremented);
    LLVMBuildCondBr(generator.builder, llvm_is_occupied, llvm_setup_entry_block, llvm_loop_capacity_check_block);
  }
  generator_leave_llvm_block(routine_context, previous_block);

  // This block setups the the entry (meaning the actual key and value variables).
  previous_block = generator_enter_llvm_block(generator, routine_context, llvm_setup_entry_block);
  {
    // Store value in key variable.
    llvm_key_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_map_entry_type, llvm_entry_pointer);
    llvm_key_temporary: LLVMValueRef;
    if generator_type_requires_loading_address(key_type, key_type) {
      llvm_key_temporary = llvm_key_pointer;
    } else {
      llvm_key_temporary = generator_emit_load(generator, routine_context, key_type, llvm_key_pointer);
    }
    generator_emit_store(generator, routine_context, key_type, llvm_key, llvm_key_temporary, nil);

    // Store value in value variable (if present).
    if llvm_value != nil {
      llvm_value_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_map_entry_type, llvm_entry_pointer);
      llvm_value_temporary: LLVMValueRef;
      if generator_type_requires_loading_address(value_type, value_type) {
        llvm_value_temporary = llvm_value_pointer;
      } else {
        llvm_value_temporary = generator_emit_load(generator, routine_context, value_type, llvm_value_pointer);
      }
      generator_emit_store(generator, routine_context, value_type, llvm_value, llvm_value_temporary, nil);
    }

    LLVMBuildBr(generator.builder, llvm_loop_block);
  }
  generator_leave_llvm_block(routine_context, previous_block);

  previous_block = generator_enter_llvm_block(generator, routine_context, llvm_loop_block);
  {
    block_returns, block_leaves := generator_emit_statement_block(generator, routine_context, statement_foreach.block, .Keep_Scope);
    // We leave the scope before building the branches, so that defer statements can be properly generated.
    generator_leave_local_scope(generator, routine_context, block_returns, block_leaves);
    if !block_returns && !block_leaves {
      llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
      if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
        LLVMBuildBr(generator.builder, llvm_loop_capacity_check_block);
      }
    }
  }
  generator_leave_llvm_block(routine_context, previous_block);
}

/**
* Emit a switch statement.
* 
* @param generator         The reference to the generator.
* @param routine_context   The context of the routine.
* @param statement         The statement to emit.
* @param is_last_statement Is the statement the last in the current block?
* @return 1. True if the statement returns otherwise false; 2. True if the statement leaves otherwise false.
*/
generator_emit_statement_switch :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement,
  is_last_statement: bool,
) -> (returns: bool, leaves: bool) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_switch := cast(^Statement_Switch) statement;

  // The same conditions apply here as with an if statement.
  needs_after_block := !is_last_statement || routine_context.needs_after_block;

  previous_next_block := routine_context.last_next_block;
  previous_break_block := routine_context.last_break_block;
  after_block := previous_next_block;
  if needs_after_block {
    after_block = generator_append_block(routine_context);
    routine_context.last_next_block = after_block;
    routine_context.last_break_block = after_block;
  }

  has_default_case := false;
  number_of_individual_cases := 0;
  for switch_case in statement_switch.cases {
    pattern_count := len(switch_case.patterns);
    if (pattern_count == 0) {
      has_default_case = true;
    }
    number_of_individual_cases += pattern_count;
  }

  default_block: LLVMBasicBlockRef;
  if has_default_case {
    default_block = generator_append_block(routine_context);
  } else {
    default_block = routine_context.last_next_block;
  }

  llvm_switch_value := generator_emit_expression(generator, routine_context, statement_switch.expression, nil);
  llvm_switch := LLVMBuildSwitch(generator.builder, llvm_switch_value, default_block, cast(u32) number_of_individual_cases);

  // Prepare all case blocks.
  case_blocks := make_dynamic_array_len_cap([dynamic]LLVMBasicBlockRef, 0, number_of_individual_cases, context.temp_allocator);
  for switch_case in statement_switch.cases {
    case_block: LLVMBasicBlockRef;
    if len(switch_case.patterns) == 0 {
      case_block = default_block;
    } else {
      case_block = generator_append_block(routine_context);
      for pattern in switch_case.patterns {
        llvm_case_value := generator_emit_expression(generator, routine_context, pattern, nil);
        LLVMAddCase(llvm_switch, llvm_case_value, case_block);
      }
    }

    append(&case_blocks, case_block);
  }

  returns = true;

  // Do generation for all case blocks.
  for switch_case, i in statement_switch.cases {
    case_block := case_blocks[i];
    
    // We need to properly set the next fallthrough block.
    next_fallthrough_block: LLVMBuilderRef;
    if i + 1 < len(statement_switch.cases) {
      routine_context.next_fallthrough_block = case_blocks[i + 1];
    } else {
      routine_context.next_fallthrough_block = after_block;
    }

    previous_block := generator_enter_llvm_block(generator, routine_context, case_block);
    {
      generator_enter_local_scope(generator, routine_context, .Control_Block, switch_case.block.start_position);
      block_returns, block_leaves := generator_emit_statement_block(generator, routine_context, switch_case.block, .Keep_Scope);
      generator_leave_local_scope(generator, routine_context, block_returns, block_leaves);
      if !block_returns && !block_leaves {
        // We have an automatic break from a switch case.
        llvm_last_instruction := LLVMGetLastInstruction(routine_context.current_block);
        if llvm_last_instruction == nil || LLVMIsATerminatorInst(llvm_last_instruction) == nil {
          LLVMBuildBr(generator.builder, after_block);
        }
      }
      returns = block_returns && returns;
      // We also set if we leave because our last case might have a break or fallthrough statment.
      leaves = block_leaves;
    }
    generator_leave_llvm_block(routine_context, previous_block);
  }

  if needs_after_block {
    previous_block := generator_enter_llvm_block(generator, routine_context, after_block);
    // If we fully return that means that we don't need the after block anymore (as it will never be reached and is never referenced).
    if returns && is_last_statement {
      if (default_block == after_block) {
        if (previous_next_block == nil) {
          LLVMBuildRetVoid(generator.builder);
        } else {
          LLVMBuildBr(generator.builder, previous_next_block);
        }
      } else {
        LLVMDeleteBasicBlock(after_block);
      }
      generator_leave_llvm_block(routine_context, previous_block);
    }
  }

  routine_context.last_next_block = previous_next_block;
  routine_context.last_break_block = previous_break_block;

  return returns, leaves;
}

/**
* Emit an assign statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_assign :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_assign := cast(^Statement_Assign) statement;

  if len(statement_assign.left_expressions) == 1 {
    // We explicitly check for assignment of single SoA/AoSoA elements as that needs to be handled as a special case (just like tuples).
    left_expression := statement_assign.left_expressions[0];
    if generator_member_is_soa_or_aosoa_access(generator, left_expression) {
      generator_emit_statement_assign_layout_member(generator, routine_context, statement_assign, left_expression);
    } else if left_expression.kind == .Unary && type_is_layout_pointer(generator_get_resolved_type(generator, (cast(^Expression_Unary) left_expression).expression)) {
      generator_emit_statement_assign_layout_pointer(generator, routine_context, statement_assign, left_expression);
    } else {
      generator_emit_statement_assign_simple(generator, routine_context, statement_assign, left_expression);
    }
  } else {
    tuple_type := generator_get_resolved_type(generator, statement_assign.right_expression);
    generator_emit_statement_assign_tuple(generator, routine_context, statement_assign, tuple_type);
  }
}

/**
* Emit an assign statement for simple expressions.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The assign statement to emit.
* @param left_expression The left expression of the assignment.
*/
generator_emit_statement_assign_simple :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement_Assign, left_expression: ^Expression) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  resolved_type := generator_get_resolved_type(generator, left_expression);

  llvm_value_to_assign_to := generator_emit_statement_assign_value_to_assign_to(generator, routine_context, left_expression, resolved_type);

  overwrite_type := generator_get_resolved_overwrite_type(generator, statement.right_expression);
  if overwrite_type == nil {
    overwrite_type = resolved_type;
  }
  needs_to_load_address := generator_type_requires_loading_address(resolved_type, overwrite_type);
  previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
  llvm_value_right := generator_emit_expression(generator, routine_context, statement.right_expression, overwrite_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  
  // Emit the binary operation for non-standard assignment operators.
  llvm_value_to_assign := llvm_value_right;
  if statement.operator != .Assign {
    operator := ASSIGN_TOKEN_TO_BINARY_TOKEN[statement.operator];

    previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
    llvm_value_left := generator_emit_expression(generator, routine_context, left_expression, nil);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    left_type := resolved_type;
    right_type := generator_get_resolved_type(generator, statement.right_expression);
    llvm_value_to_assign = generator_emit_expression_binary_arithmetic_operation(
      generator,
      routine_context,
      operator,
      left_type,
      llvm_value_left,
      right_type,
      llvm_value_right,
    );
  }

  generator_emit_store(generator, routine_context, resolved_type, llvm_value_to_assign_to, llvm_value_to_assign, statement.right_expression);
}

/**
* Emit an assign statement for tuple.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The assign statement to emit.
* @param tuple_type      The type of the tuple.
*/
generator_emit_statement_assign_tuple :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement_Assign, tuple_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_tuple_type := generator_type_to_llvm(generator, tuple_type);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_tuple_pointer := generator_emit_expression(generator, routine_context, statement.right_expression, tuple_type);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  type_tuple :=  cast(^Type_Tuple) tuple_type;
  assert(len(type_tuple.elements) == len(statement.left_expressions));

  for element, i in type_tuple.elements {
    element_type := element.type;

    left_expression := statement.left_expressions[i];
    resolved_type := generator_get_resolved_type(generator, left_expression);

    llvm_value_to_assign_to := generator_emit_statement_assign_value_to_assign_to(generator, routine_context, left_expression, resolved_type);

    llvm_element_type := generator_type_to_llvm(generator, element_type);
    llvm_element_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_tuple_type, llvm_tuple_pointer, i);
    llvm_element_value: LLVMValueRef;
    needs_to_load_address := generator_type_requires_loading_address(element_type, element_type);
    if needs_to_load_address {
      llvm_element_value = llvm_element_pointer;
    } else {
      llvm_element_value = generator_emit_load(generator, routine_context, element_type, llvm_element_pointer);
    }

    generator_emit_store(generator, routine_context, element_type, llvm_value_to_assign_to, llvm_element_value, nil);
  }
}

/**
* Emit an assign statement for a member in a layout collection.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The assign statement to emit.
* @param left_expression The left expression of the statement.
*/
generator_emit_statement_assign_layout_member :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement_Assign,
  left_expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  assert(left_expression.kind == .Index);
  left_expression_index := cast(^Expression_Index) left_expression;

  layout_collection_type := generator_get_resolved_type(generator, left_expression_index.expression);
  needs_to_dereference_pointer := type_is_absolute_pointer(layout_collection_type) || type_is_self_relative_pointer(layout_collection_type);
  if needs_to_dereference_pointer {
    layout_collection_type = layout_collection_type.base;
  }
  assert(type_is_soa_or_aosoa(layout_collection_type));
  assert(type_is_struct(layout_collection_type.base));

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Value);
  llvm_index := generator_emit_expression(generator, routine_context, left_expression_index.index, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  previous_loading_mode = generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_layout_collection := generator_emit_expression(generator, routine_context, left_expression_index.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);
  llvm_layout_collection = generator_emit_possible_pointer_dereference(generator, routine_context, left_expression_index.expression, llvm_layout_collection);

  generator_emit_statement_assign_layout_fields(generator, routine_context, statement.right_expression, layout_collection_type, llvm_layout_collection, llvm_index);
}

/**
* Emit an assign statement for a layout pointer.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The assign statement to emit.
* @param left_expression The left expression of the statement.
*/
generator_emit_statement_assign_layout_pointer :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  statement: ^Statement_Assign,
  left_expression: ^Expression,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  assert(left_expression.kind == .Unary);

  unary_expression := cast(^Expression_Unary) left_expression;
  layout_pointer_type := generator_get_resolved_type(generator, unary_expression.expression);
  llvm_layout_pointer_type := generator_type_to_llvm(generator, layout_pointer_type);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_layout_pointer := generator_emit_expression(generator, routine_context, unary_expression.expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  llvm_layout_pointer_data_pointer := generator_emit_gep_value_data(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
  llvm_layout_pointer_data := generator_emit_load(generator, routine_context, generator.storage.type_rawptr, llvm_layout_pointer_data_pointer);
  llvm_layout_pointer_index_pointer := generator_emit_gep_value_length(generator.builder, routine_context, llvm_layout_pointer_type, llvm_layout_pointer);
  llvm_layout_pointer_index := generator_emit_load(generator, routine_context, generator.storage.type_int, llvm_layout_pointer_index_pointer);

  generator_emit_statement_assign_layout_fields(
    generator,
    routine_context,
    statement.right_expression,
    layout_pointer_type.base,
    llvm_layout_pointer_data,
    llvm_layout_pointer_index,
  );
}

/**
* Emit an assign statement for the fields in a layout collection.
* 
* @param generator              The reference to the generator.
* @param routine_context        The context of the routine.
* @param right_expression       The right expression of the statement.
* @param layout_collection_type The type of the layout collection.
* @param llvm_layout_collection The LLVM layout collection.
* @param llvm_index             The LLVM index into the layout collection.
*/
generator_emit_statement_assign_layout_fields :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  right_expression: ^Expression,
  layout_collection_type: ^Type,
  llvm_layout_collection: LLVMValueRef,
  llvm_index: LLVMValueRef,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  element_type := layout_collection_type.base.base;
  type_struct := cast(^Type_Struct) element_type;
  
  llvm_collection_type := generator_type_to_llvm(generator, layout_collection_type);
  llvm_element_type := generator_type_to_llvm(generator, element_type);

  is_aosoa := type_is_aosoa(layout_collection_type);

  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_element_pointer := generator_emit_expression(generator, routine_context, right_expression, nil);
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  for field, i in type_struct.fields {
    llvm_element_field_pointer := generator_emit_gep_field(generator.builder, routine_context, llvm_element_type, llvm_element_pointer, i);
    llvm_field_value: LLVMValueRef;
    if generator_type_requires_loading_address(field.type, field.type) {
      llvm_field_value = llvm_element_field_pointer;
    } else {
      llvm_field_value = generator_emit_load(generator, routine_context, field.type, llvm_element_field_pointer);
    }

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
    
    generator_emit_store(generator, routine_context, field.type, llvm_layout_element_pointer, llvm_field_value, nil);
  }
}

/**
* Emit the value to assign to in an assign statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param left_expression The left expression of the statement.
* @param resolved_type   The resolved type of the assignment.
* @return The LLVM value to assign to.
*/
generator_emit_statement_assign_value_to_assign_to :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  left_expression: ^Expression,
  resolved_type: ^Type,
) -> LLVMValueRef {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  left_kind := left_expression.kind;

  // The value we load here has to be carefully generated to allow for storing into it.
  previous_loading_mode := generator_enter_loading_mode(routine_context, .Load_Address);
  llvm_value_to_assign_to: LLVMValueRef;
  if left_kind == .Name {
    // Parameters can never be assigned to and therefore 'generator_name_to_llvm' does not check them.
    llvm_value_to_assign_to, _ = generator_name_to_llvm(generator, routine_context, left_expression, (cast(^Expression_Name) left_expression).name);
  } else if left_kind == .Unary {
    expression_unary := cast(^Expression_Unary) left_expression;
    llvm_value_to_assign_to = generator_emit_expression(generator, routine_context, expression_unary.expression, nil);
    unary_expression_type := generator_get_resolved_type(generator, expression_unary.expression);
    if type_is_self_relative_pointer(unary_expression_type) {
      llvm_value_to_assign_to = generator_emit_self_relative_pointer_to_absolute(generator, routine_context, unary_expression_type, llvm_value_to_assign_to);
    } else if !routine_context.last_resolved_symbol_was_parameter {
      // We explicitly request the pointer type that we need to here in order to store into it.
      // Execept for parameters as they are already passed by value.
      pointer_type := type_storage_get_or_make_type_pointer(generator.storage, resolved_type);
      llvm_value_to_assign_to = generator_emit_load(generator, routine_context, pointer_type, llvm_value_to_assign_to);
    }
  } else {
    llvm_value_to_assign_to = generator_emit_expression(generator, routine_context, left_expression, nil);
  }
  generator_leave_loading_mode(routine_context, previous_loading_mode);

  return llvm_value_to_assign_to;
}

/**
* Emit an initialize statement.
* 
* @param generator       The reference to the generator.
* @param routine_context The context of the routine.
* @param statement       The statement to emit.
*/
generator_emit_statement_initialize :: proc(generator: ^Generator, routine_context: ^Routine_Context, statement: ^Statement) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  statement_initialize := cast(^Statement_Initialize) statement;

  is_deconstructing_tuple := len(statement_initialize.names) > 1;

  resolved_type: ^Type;
  if statement_initialize.type == nil {
    resolved_type = generator_get_resolved_type(generator, statement_initialize.expression);
  } else {
    resolved_type = generator_get_resolved_type(generator, statement_initialize.type);
  }
  
  llvm_initializer_value: LLVMValueRef;
  if statement_initialize.expression != nil {
    overwrite_type := generator_get_resolved_overwrite_type(generator, statement_initialize.expression);
    if overwrite_type == nil {
      overwrite_type = resolved_type;
    }

    needs_to_load_address := is_deconstructing_tuple || generator_type_requires_loading_address(resolved_type, overwrite_type);
    previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
    llvm_initializer_value = generator_emit_expression(generator, routine_context, statement_initialize.expression, overwrite_type);
    generator_leave_loading_mode(routine_context, previous_loading_mode);
  }

  // We emit the local after evaluating the initializer as it may refer to parameters with the same name as our local here.
  if is_deconstructing_tuple {
    assert(type_is_tuple(resolved_type));
    
    type_tuple := cast(^Type_Tuple) resolved_type;
    
    for element, i in type_tuple.elements {
      element_type := element.type;
      llvm_element_type := generator_type_to_llvm(generator, element_type);
      
      llvm_local := generator_emit_local(generator, routine_context, statement.position, element_type, statement_initialize.names[i]);
      llvm_element_pointer := generator_emit_gep_field(
        generator.builder,
        routine_context,
        generator_type_to_llvm(generator, resolved_type),
        llvm_initializer_value,
        i,
      );
      
      llvm_element_initializer: LLVMValueRef;
      needs_to_load_address := generator_type_requires_loading_address(element_type, element_type);
      if needs_to_load_address {
        llvm_element_initializer = llvm_element_pointer;
      } else {
        llvm_element_initializer = generator_emit_load(generator, routine_context, element_type, llvm_element_pointer);
      }
      
      generator_emit_store(generator, routine_context, element_type, llvm_local, llvm_element_initializer, nil);
    }
  } else {
    llvm_local := generator_emit_local(generator, routine_context, statement.position, resolved_type, statement_initialize.names[0]);

    // We can skip storing a 'default value' as that got already handled by emitting the local.
    if llvm_initializer_value != nil {
      generator_emit_store(generator, routine_context, resolved_type, llvm_local, llvm_initializer_value, statement_initialize.expression);
    }
  }
}
