package nox

import "tracy"

/**
* Holds all possible built-in routines.
*/
Builtin_Routine :: enum {
  Invalid,                      // Invalid built-in routine.
  
  Main,                         // The 'main' routine.

  Compare_String,               // The 'compare_string' routine.
  Compare_CString,              // The 'compare_cstring' routine.
  Hash_Data,                    // The 'hash_data' routine.

  Runtime_Init_Default_Context, // The 'runtime_init_default_context' routine.

  Count,                        // The number of built-in routines.
}

BUILTIN_ROUTINE_MAIN := "main";
BUILTIN_ROUTINE_COMPARE_STRING := "compare_string";
BUILTIN_ROUTINE_COMPARE_CSTRING := "compare_cstring";
BUILTIN_ROUTINE_HASH_DATA := "hash_data";
BUILTIN_ROUTINE_RUNTIME_INIT_DEFAULT_CONTEXT := "runtime_init_default_context";

/**
* Initializes all symbols.
*
* @param generator The reference to the generator.
*/
generator_init_symbols :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  for symbol in generator.resolver_output.reachable_sorted_symbols {
    generator_init_symbol(generator, symbol);
  }

  generator_init_interface_vtables(generator);
}

/**
* Initialize a symbol.
*
* @param generator The reference to the generator.
* @param symbol    The symbol to initialize.
*/
generator_init_symbol :: proc(generator: ^Generator, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  declaration := symbol.declaration;

  is_extern := false;
  if declaration != nil {
    is_extern = .Extern in declaration.flags;
  }

  llvm_symbol: LLVMValueRef = nil;
  switch symbol.kind {
    case .Constant, .Global: {
      needs_to_mangle_name := symbol.kind == .Constant ? true : !is_extern;
      
      llvm_name: cstring;
      if needs_to_mangle_name {
        llvm_name = make_mangled_name(symbol);
      } else {
        llvm_name = to_cstring(symbol.name); 
      }

      constant_type := generator_type_to_llvm(generator, symbol.type);
      llvm_symbol = LLVMAddGlobal(generator.module, constant_type, llvm_name);
      if symbol.kind == .Constant {
        LLVMSetGlobalConstant(llvm_symbol, true);
      }
    }

    case .Routine: {
      type := symbol.type;
      type_routine := cast(^Type_Routine) type;

      // There are a few instances in which we can skip generating the routine.
      //   1. The routine is an intrinsc.
      //   2. The routine is an uninstantiated generic.
      //   3. The routine got disabled through the corresponding attribute.
      if .Is_Intrinsic in type_routine.routine_flags || .Uninstantiated_Generic in symbol.flags || .Routine_Disabled in symbol.flags {
        return;
      }

      needs_to_mangle_name := !is_extern;

      llvm_name: cstring;
      if needs_to_mangle_name {
        llvm_name = make_mangled_name(symbol);
      } else {
        llvm_name = to_cstring(symbol.name);
      }

      llvm_type := generator_type_to_llvm_without_routine_promotion(generator, type);
      llvm_symbol = LLVMAddFunction(generator.module, llvm_name, llvm_type);
      calling_convention := type_routine.calling_convention;
      llvm_calling_convention := generator_calling_convention_to_llvm(calling_convention);
      LLVMSetFunctionCallConv(llvm_symbol, llvm_calling_convention);

      // NOTE: We would like to have the 'nounwind' attribute on every function as we do not have exceptions.
      // However it seems that the attribute causes weird issues with the call stack in the debug info (it makes it pretty much unreadable which is very annoying).
      // Currently there is no obvious way of fixing this other than removing the attribute.
      // As the attribute does not really have an impact on performance, it should not really matter.
      // LLVM is smart enough to figure out itself that our routines do not use exceptions.
      //LLVMAddAttributeAtIndex(llvm_symbol, cast(u32) LLVMAttributeFunctionIndex, generator_create_llvm_enum_attribute("nounwind"));

      generator_init_symbol_routine_attributes(generator, llvm_symbol, type);

      if .Routine_Builtin in symbol.flags {
        builtin_routine := Builtin_Routine.Invalid;
        switch symbol.name {
          case BUILTIN_ROUTINE_MAIN: builtin_routine = .Main;
          case BUILTIN_ROUTINE_COMPARE_STRING: builtin_routine = .Compare_String;
          case BUILTIN_ROUTINE_COMPARE_CSTRING: builtin_routine = .Compare_CString;
          case BUILTIN_ROUTINE_HASH_DATA: builtin_routine = .Hash_Data;
          case BUILTIN_ROUTINE_RUNTIME_INIT_DEFAULT_CONTEXT: builtin_routine = .Runtime_Init_Default_Context;
        }
        assert(builtin_routine != .Invalid);

        // We don't want a mangled name for the builtin 'main'.
        if (builtin_routine == .Main) {
          LLVMSetValueName2(llvm_symbol, "main", 4);
          symbol.access_modifier = .Public; // HACK: This is a weird way to force the symbol to properly linked.
        }

        generator.llvm_builtin_routines[builtin_routine] = {symbol, {llvm_symbol, llvm_type}};
      }
    }

    case .Type, .Package: return; // Don't need to do anything here.

    case .Local, .Parameter, .None: fallthrough;
    case: assert(false);
  }

  if is_extern {
    LLVMSetLinkage(llvm_symbol, .External);
  } else {
    switch symbol.access_modifier {
      case .Private: LLVMSetLinkage(llvm_symbol, .Private);
      case .Internal: LLVMSetLinkage(llvm_symbol, .Internal);
      case .Public, .Member_Unspecified, .None: fallthrough;
      case: LLVMSetLinkage(llvm_symbol, .External);
    }
  }

  generator.llvm_symbols[symbol] = llvm_symbol;
}

/**
* Initialize the attributes for a routine symbol.
*
* @param generator   The reference to the generator.
* @param llvm_symbol The LLVM symbol.
* @param type        The type of the routine.
*/
generator_init_symbol_routine_attributes :: proc(generator: ^Generator, llvm_symbol: LLVMValueRef, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  type_routine := cast(^Type_Routine) type;

  llvm_noalias_attribute := generator_create_llvm_enum_attribute("noalias");
  llvm_noundef_attribute := generator_create_llvm_enum_attribute("noundef");

  return_type := type_routine.return_type;

  parameter_offset: u32 = 1; // Index '0' would be the return value (aka things not being passed as pointers).
  if generator_type_is_passed_as_pointer(return_type) {
    llvm_attribute_index: u32 = parameter_offset;

    llvm_return_type := generator_type_to_llvm(generator, return_type);
    llvm_sret_attribute := generator_create_llvm_enum_attribute_type("sret", llvm_return_type);

    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_sret_attribute);
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noalias_attribute);
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noundef_attribute);

    parameter_offset += 1;
  }
  
  if type_routine_has_context(type_routine) {
    llvm_attribute_index := parameter_offset;

    llvm_nonnull_attribute := generator_create_llvm_enum_attribute("nonnull");
    llvm_nocapture_attribute := generator_create_llvm_enum_attribute("nocapture");

    // The 'context' pointer can always be marked with special attributes.
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noalias_attribute);
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noundef_attribute);
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_nonnull_attribute);
    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_nocapture_attribute);

    parameter_offset += 1;
  }
  
  if .Is_Method in type_routine.routine_flags {
    parameter_offset += 1;
  }

  for parameter, i in type_routine.parameters {
    llvm_attribute_index: u32 = parameter_offset + cast(u32) i;

    LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noundef_attribute);

    parameter_type := parameter.type;
    if generator_type_is_passed_as_pointer(parameter_type) {
      LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_noalias_attribute);
      
      // HACK: We would really like to add the 'byval' attribute to mark the parameters correctly.
      // But for some reason when doing so, LLVM will generate incorrect code and I can't figure out why...
      //llvm_parameter_type := generator_type_to_llvm(generator, parameter_type);
      //llvm_byval_attribute := generator_create_llvm_enum_attribute_type("byval", llvm_parameter_type);
      //LLVMAddAttributeAtIndex(llvm_symbol, llvm_attribute_index, llvm_byval_attribute);
    }
  }
}

/**
* Initializes the vtables for all reachable interfaces.
*
* @param generator The reference to the generator.
*/
generator_init_interface_vtables :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  for key, reachable_implementation in generator.resolver_output.reachable_interface_implementations {
    interface_type := key.interface_type;
    implementation_type := key.implementation_type;

    llvm_vtable_type := generator_type_to_llvm(generator, interface_type);
    assert(len(reachable_implementation) == 1); 
    resolved_implementation := reachable_implementation[0]

    llvm_vtable_elements := make_dynamic_array_len_cap([dynamic]LLVMValueRef, 0, len(interface_type.methods), context.temp_allocator);
    for method in interface_type.methods {
      method_symbol := resolved_implementation.symbols[method.name];
      append(&llvm_vtable_elements, generator_symbol_to_llvm(generator, method_symbol));
    }
    llvm_vtable_value := LLVMConstNamedStruct(llvm_vtable_type, raw_data(llvm_vtable_elements), cast(u32) len(llvm_vtable_elements));

    llvm_name := to_cstring_f(
      "vtable-%v-%v",
      make_mangled_name_from_type(interface_type, interface_type.symbol),
      make_mangled_name_from_type(implementation_type, implementation_type.symbol),
    );
    llvm_vtable := LLVMAddGlobal(generator.module, llvm_vtable_type, llvm_name);
    LLVMSetInitializer(llvm_vtable, llvm_vtable_value);

    vtable_key := VTable_Key{interface_type, implementation_type};
    generator.llvm_vtables[vtable_key] = llvm_vtable;
  }
}

/**
* Emit all symbols.
*
* @param generator The reference to the generator.
*/
generator_emit_symbols :: proc(generator: ^Generator) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  for symbol in generator.resolver_output.reachable_sorted_symbols {
    generator_emit_symbol(generator, symbol);
  }
}

/**
* Emit a symbol.
*
* @param generator The reference to the generator.
* @param symbol    The symbol to emit.
*/
generator_emit_symbol :: proc(generator: ^Generator, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);
   
  // Here seems a good spot to clear our temporary allocator
  // which we use to allocate LLVM names among other things.
  free_all(context.temp_allocator);

  switch symbol.kind {
    case .Constant: generator_emit_symbol_constant(generator, symbol);
    case .Global: generator_emit_symbol_global(generator, symbol);
    case .Routine: generator_emit_symbol_routine(generator, symbol);

    case .Type, .Package: break; // Don't need to do anything here.

    case .Local, .Parameter: fallthrough;
    case .None: assert(false);
  }
}

/**
* Emit a constant symbol.
*
* @param generator The reference to the generator.
* @param symbol    The constant symbol to emit.
*/
generator_emit_symbol_constant :: proc(generator: ^Generator, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_constant := generator_symbol_to_llvm(generator, symbol);
  llvm_value := generator_constant_to_llvm(generator, symbol.value, symbol.type);
  LLVMSetInitializer(llvm_constant, llvm_value);
}

/**
* Emit a global symbol.
*
* @param generator The reference to the generator.
* @param symbol    The global symbol to emit.
*/
generator_emit_symbol_global :: proc(generator: ^Generator, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  declaration := symbol.declaration;
  if .Extern in declaration.flags {
    return;
  }
  
  declaration_global := cast(^Declaration_Global) declaration;

  llvm_global := generator_symbol_to_llvm(generator, symbol);
  type := symbol.type;
  llvm_value := generator_default_initializer_value(generator, type);
  LLVMSetInitializer(llvm_global, llvm_value);
  LLVMSetThreadLocal(llvm_global, .Global_Thread_Local in symbol.flags);

  if declaration_global.expression != nil {
    routine_context := &generator.initializer_routine_context;

    generator_enter_local_scope(generator, routine_context, .Block, declaration.position);
    previous_block := generator_enter_llvm_block(generator, routine_context, generator.initializer_block);
    
    context_type := generator.storage.cached_runtime_types.context_struct;
    llvm_context_type := generator_type_to_llvm(generator, context_type);
    generator_add_local_symbol(SPECIAL_NAME_CONTEXT, generator.initializer_context, llvm_context_type, context_type, false, routine_context.local_scope);

    expression_type := generator_get_resolved_type(generator, declaration_global.expression);
    needs_to_load_address := generator_type_requires_loading_address(type, expression_type);
    previous_loading_mode := generator_enter_loading_mode(routine_context, needs_to_load_address ? .Load_Address : .Load_Value);
    llvm_initializer_value := generator_emit_expression(generator, routine_context, declaration_global.expression, symbol.type);
    generator_leave_loading_mode(routine_context, previous_loading_mode);

    generator_emit_store(generator, routine_context, symbol.type, llvm_global, llvm_initializer_value, declaration_global.expression);

    generator_leave_local_scope(generator, routine_context, false, false);
    generator_leave_llvm_block(routine_context, previous_block);
  }
}

/**
* Emit a routine symbol.
*
* @param generator The reference to the generator.
* @param symbol    The routine symbol to emit.
*/
generator_emit_symbol_routine :: proc(generator: ^Generator, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  routine_data := cast(^Type_Routine) symbol.type;

  // There are a few instances where we can skip generating a body for a routine.
  //   - The routine is an intrinsic
  //   - The routine is an uninstantiated generic
  //   - The routine got disabled through the corresponding attribute
  //   - The routine is marked extern
  declaration := symbol.declaration;
  if .Is_Intrinsic in routine_data.routine_flags || .Uninstantiated_Generic in symbol.flags || .Routine_Disabled in symbol.flags || .Extern in declaration.flags {
    return;
  }

  declaration_routine := cast(^Declaration_Routine) symbol.declaration;

  llvm_symbol := generator_symbol_to_llvm(generator, symbol);
  type_routine := cast(^Type_Routine) symbol.type;
  return_type := type_routine.return_type;

  routine_context: Routine_Context;
  routine_context.symbol = symbol;
  routine_context.llvm_symbol = llvm_symbol;
  routine_context.return_type = return_type;
  routine_context.needs_after_block = type_is_void(return_type) || generator_type_is_passed_as_pointer(return_type);

  generator_debug_init_routine(generator, &routine_context);

  generator_enter_local_scope(generator, &routine_context, .Block, declaration.position);
  
  // We first want to define a declaration block in which all local variables will be allocated.
  declaration_block := generator_append_block(&routine_context);
  routine_context.declaration_block = declaration_block;
  generator_init_routine_parameters(generator, &routine_context, symbol, declaration_routine, return_type, type_routine);

  // We append a second block for our actual statements and emit them.
  statement_block := generator_append_block(&routine_context);
  previous_block := generator_enter_llvm_block(generator, &routine_context, statement_block);

  returns, leaves := generator_emit_statement_block(generator, &routine_context, declaration_routine.block, .New_Scope);
  if routine_context.needs_after_block && !returns {
    generator_debug_emit_location(generator, &routine_context, declaration_routine.block.end_position);
    LLVMBuildRetVoid(generator.builder);
  }
  generator_leave_local_scope(generator, &routine_context, returns, leaves);
  generator_leave_llvm_block(&routine_context, previous_block);

  // Now that we are done with all statements, we need to finish our declaration block, by jumping from it into the statement block.
  {
    previous_block := generator_enter_llvm_block(generator, &routine_context, declaration_block);
    generator_debug_emit_no_location(generator);
    LLVMBuildBr(generator.builder, statement_block);
    generator_leave_llvm_block(&routine_context, previous_block);
  }
}

/**
* Initialize the parameters for a routine symbol.
*
* @param generator           The reference to the generator.
* @param routine_context     The context of the routine.
* @param symbol              The routine symbol.
* @param declaration_routine The declaration of the routine.
* @param return_type         The return type of the routine.
* @param type_routine        The type of the routine.
*/
generator_init_routine_parameters :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  symbol: ^Symbol,
  declaration_routine: ^Declaration_Routine,
  return_type: ^Type,
  type_routine: ^Type_Routine,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  parameter_offset := 0;

  if generator_type_is_passed_as_pointer(return_type) {
    llvm_parameter_symbol := LLVMGetParam(routine_context.llvm_symbol, cast(u32) parameter_offset);
    LLVMSetValueName2(llvm_parameter_symbol, "return", 6);
    parameter_offset += 1;
  }

  if type_routine_has_context(type_routine) {
    parameter_type := type_storage_get_or_make_type_pointer(generator.storage, generator.storage.cached_runtime_types.context_struct);
    generator_init_routine_parameter(generator, routine_context, symbol.declaration.position, SPECIAL_NAME_CONTEXT, parameter_type, cast(u32) parameter_offset);
    parameter_offset += 1;
  }

  if .Routine_Method_Implemented in symbol.flags {
    implementation_type := symbol.value.(^Type); // This gets us the type the method belongs to.
    parameter_type := type_storage_get_or_make_type_pointer(generator.storage, implementation_type);
    generator_init_routine_parameter(generator, routine_context, symbol.declaration.position, SPECIAL_NAME_SELF, parameter_type, cast(u32) parameter_offset);
    parameter_offset += 1;
  }

  // We need to add all routine parameters to the local symbol table.
  for parameter, i in declaration_routine.parameters {
    index := parameter_offset + i;
    parameter_type := generator_get_resolved_type(generator, parameter.type);
    generator_init_routine_parameter(generator, routine_context, parameter.position, parameter.name, parameter_type, cast(u32) index);
  }
}

/**
* Initialize a parameter for a routine symbol.
*
* @param generator           The reference to the generator.
* @param routine_context     The context of the routine.
* @param position            The source position of the parameter.
* @param parameter_name      The name of the parameter.
* @param parameter_type      The type of the parameter.
* @param parameter_offset    The offset of the parameter.
*/
generator_init_routine_parameter :: proc(
  generator: ^Generator,
  routine_context: ^Routine_Context,
  position: Source_Position,
  parameter_name: string,
  parameter_type: ^Type,
  parameter_offset: u32,
) {
  tracy.ZoneC(ZONE_COLOR_GENERATOR);

  llvm_parameter_type := generator_type_to_llvm(generator, parameter_type);
  llvm_parameter_symbol := LLVMGetParam(routine_context.llvm_symbol, cast(u32) parameter_offset);
  LLVMSetValueName2(llvm_parameter_symbol, to_cstring(parameter_name), len(parameter_name));
  
  generator_add_local_symbol(parameter_name, llvm_parameter_symbol, llvm_parameter_type, parameter_type, true, routine_context.local_scope);

  generator_debug_emit_parameter(generator, routine_context, position, parameter_type, parameter_name, llvm_parameter_symbol, parameter_offset + 1);
}
