package nox

import "core:fmt"
import "core:mem"
import "core:strings"
import "tracy"

SPECIAL_NAME_SELF    :: "self";
SPECIAL_NAME_CONTEXT :: "context";
SPECIAL_NAME_DISCARD :: "_";

/**
* Represents a local block scope.
*/
Local_Scope :: struct {
  parent_block: ^Local_Scope,       // The parent block scope.
  symbols: map[string]Local_Symbol, // The symbols in the local block.
}

/**
* Represents a single field in the special 'Context' struct.
*/
Context_Field :: struct {
  name: string, // The name of the context field.
  type: ^Type,  // The type of the context field.
}

/**
* Represents a resolved member to be consumed by the generator.
*/
Resolved_Member :: struct {
  index: int,            // The resolved index of the accessed member.
  indices: [dynamic]int, // The resolved indices when accessing multiple fields (will be empty if just a single index is needed).
  type: ^Type,           // The type of the resolved member.
}

/**
* Reprsents a resolved compound field.
*/
Resolved_Compound_Field :: struct {
  index: int,  // The index of the compound field.
  type: ^Type, // The type of the compound field.
}

/**
* Represents the output of the resolver to be consumed by the generator.
*/
Resolver_Output :: struct {
  reachable_sorted_symbols: [dynamic]^Symbol,                                                            // Holds reachable symbols in a sorted way.
  reachable_types: [dynamic]^Type,                                                                       // Holds reachable types.
  reachable_interface_implementations: map[Resolved_Implementation_Key][dynamic]Resolved_Implementation, // Holds reachable interface implementations.

  resolved_symbols: map[^Expression]^Symbol,                                                             // Holds resolved symbols.
  // The key has to be a 'rawptr' as both '^Expression' and '^Type_Sepcification' are valid.
  resolved_types: map[rawptr]^Type,                                                                      // Holds resolved types.
  resolved_overwrite_types: map[^Expression]^Type,                                                       // Holds resolved overwritten types.
  resolved_constants: map[^Expression]Operand,                                                           // Holds resolved constants.
  resolved_members: map[^Expression]Resolved_Member,                                                     // Holds resolved members.
  resolved_compound_fields: map[^Expression]Resolved_Compound_Field,                                     // Holds resolved compound fields.
  resolved_implementations: map[Resolved_Implementation_Key][dynamic]Resolved_Implementation,            // Holds resolved implementations (interfaces and others).
}

/**
* Holds necessary data for the resolver.
*/
Resolver :: struct {
  storage: ^Type_Storage,                                                           // A reference to the type storage.
  
  // This is bookkeeping while resolving.
  symbol_pool: mem.Dynamic_Pool,                                                    // The backing pool for the symbol allocator.
  symbol_allocator: mem.Allocator,                                                  // The allocator used for symbols
  always_reachable_symbols: [dynamic]^Symbol,                                       // Symbols that are always reachable (get resolved before 'main').
  reachable_symbols: [dynamic]^Symbol,                                              // Symbols that are reachable (will get resolved and sorted).
  unreachable_symbols: map[^Symbol]struct{},                                        // Symbols that are unreachable (will get resolved but not sorted).
  sorted_symbols: [dynamic]^Symbol,                                                 // Contains the symbols ordered by their dependency.
  instantiated_generic_routines: map[^Symbol][dynamic]Instantiated_Generic_Routine, // Map of instantiated generic routines where the key is the uninstantiated symbol.
  unresolved_implementations: #soa [dynamic]Unresolved_Implementation,              // Unresolved implementations (get resolved before 'main').
  types_implementing_interface: map[^Type][dynamic]^Type,                           // Map of interfaces with the corresponding types that implement them.
  context_fields: #soa [dynamic]Context_Field,                                      // List of fields that got added through the '#expand_context' directive.
  name_pool: mem.Dynamic_Pool,                                                      // The backing pool for the symbol allocator.
  name_allocator: mem.Allocator,                                                    // The allocator used for symbol names.

  // The following is 'local' storage which gets reset after resolving the body of a routine.
  current_routine: ^Symbol,                                                         // The routine whose body is currently being resolved.
  local_scope: ^Local_Scope,                                                        // This stores the current local symbol block (the 'deepest' one in the stack).
  
  output: Resolver_Output,                                                          // The output of the resolver.
}

/**
* Makes a new resolver.
*
* @return The new resolver.
*/
resolver_make :: proc() -> ^Resolver {
  resolver := new(Resolver);
  resolver_init(resolver);
  return resolver;
}

/**
* Initializes a resolver.
*
* @param resolver The resolver to initialize.
*/
resolver_init :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.storage = (cast(^Compiler) context.user_ptr).storage;

  mem.dynamic_pool_init(&resolver.symbol_pool);
  resolver.symbol_allocator = mem.dynamic_pool_allocator(&resolver.symbol_pool);
  mem.dynamic_pool_init(&resolver.name_pool);
  resolver.name_allocator = mem.dynamic_pool_allocator(&resolver.name_pool);
}

/**
* Destroys the resouces assocociated with a resolver.
*
* @param resolver The resolver to destroy.
*/
resolver_destroy :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  mem.dynamic_pool_destroy(&resolver.symbol_pool);
  delete(resolver.always_reachable_symbols);
  delete(resolver.reachable_symbols);
  delete(resolver.unreachable_symbols);
  delete(resolver.sorted_symbols);
  for _, instantiated_generic_routines in resolver.instantiated_generic_routines {
    for instantiated_generic_symbol in instantiated_generic_routines {
      delete(instantiated_generic_symbol.generic_types);
    }
    delete(instantiated_generic_routines);
  }
  delete(resolver.instantiated_generic_routines);
  delete_soa(resolver.unresolved_implementations);
  for _, types_implementing_interface in resolver.types_implementing_interface {
    delete(types_implementing_interface);
  }
  delete(resolver.types_implementing_interface);
  delete_soa(resolver.context_fields);
  mem.dynamic_pool_destroy(&resolver.name_pool);

  delete(resolver.output.reachable_sorted_symbols);
  delete(resolver.output.reachable_types);
  delete(resolver.output.resolved_symbols);
  delete(resolver.output.resolved_types);
  delete(resolver.output.resolved_overwrite_types);
  delete(resolver.output.resolved_constants);
  for _, resolved_member in resolver.output.resolved_members {
    delete(resolved_member.indices);
  }
  delete(resolver.output.resolved_members);
  delete(resolver.output.resolved_compound_fields);
  for _, resolved_implementations in resolver.output.resolved_implementations {
    for resolved_implementation in resolved_implementations {
      delete(resolved_implementation.symbols);
    }
    delete(resolved_implementations);
  }
  delete(resolver.output.resolved_implementations);
  delete(resolver.output.reachable_interface_implementations);

  free(resolver);
}

/**
* Imports all built-in types.
*
* @param resolver The reference to the resolver.
* @param file     The package file the built-in types will live in.
*/
resolver_import_builtin_types :: proc(resolver: ^Resolver, file: ^Package_File) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_add_builtin_type(resolver, file, resolver.storage.type_b8);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_b16);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_b32);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_b64);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_bool);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_i8);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_i16);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_i32);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_i64);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_int);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_u8);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_u16);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_u32);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_u64);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_uint);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_f32);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_f64);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_char);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_string);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_cstring);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_rawptr);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_any);
  resolver_add_builtin_type(resolver, file, resolver.storage.type_typeid);

  resolver_add_builtin_type(resolver, file, resolver.storage.type_u8, "byte");

  resolver_add_builtin_constant(resolver, file, "null", resolver.storage.type_untyped_null, nil);

  context_symbol := resolver_add_builtin_type(resolver, file, nil, "Context");
  context_type := type_storage_make_type_incomplete(resolver.storage, .Struct, context_symbol);
  context_symbol.type = context_type;
  resolver.storage.cached_runtime_types.context_struct = context_type;
}

/**
* Adds a built-in type.
*
* @param resolver The reference to the resolver.
* @param file     The package file the built-in type will live in.
* @param name     The name of the built-in type.
* @return The symbol of the built-in type.
*/
resolver_add_builtin_type :: proc(resolver: ^Resolver, file: ^Package_File, type: ^Type, name := "") -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  name := name;
  if name == "" {
    name = type.name;
  }
  symbol := symbol_make(resolver, .Type, .Public, file, name, nil);
  symbol.state = .Resolved;
  symbol.flags += {.Reachable};
  symbol.type = type;
  resolver_add_global_symbol(resolver, file, name, symbol);

  if type != nil {
    type.symbol = symbol;
  }

  return symbol;
}

/**
* Adds a built-in constant.
*
* @param resolver The reference to the resolver.
* @param file     The package file the built-in contant will live in.
* @param name     The name of the built-in contant.
* @param type     The type of the built-in contant.
* @param value    The value of the built-in contant.
*/
resolver_add_builtin_constant :: proc(resolver: ^Resolver, file: ^Package_File, name: string, type: ^Type, value: Value) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  symbol := symbol_make(resolver, .Constant, .Public, file, name, nil);
  symbol.state = .Resolved;
  symbol.flags += {.Reachable};
  symbol.type = type;
  symbol.value = value;
  resolver_add_global_symbol(resolver, file, name, symbol);
}

/**
* Cache the special types of the 'runtime' package.
*
* @param resolver        The reference to the resolver.
* @param runtime_package The 'runtime' package.
*/
resolver_cache_runtime_package_types :: proc(resolver: ^Resolver, runtime_package: ^Package) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  resolver_cache_runtime_package_type(resolver, runtime_package, "Operating_System_Kind", &resolver.storage.cached_runtime_types.operating_system_kind);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Source_Location", &resolver.storage.cached_runtime_types.source_location);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Allocator", &resolver.storage.cached_runtime_types.allocator);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info", &resolver.storage.cached_runtime_types.type_info);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Variant", &resolver.storage.cached_runtime_types.type_info_variant);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Integer", &resolver.storage.cached_runtime_types.type_info_integer);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Enumeration", &resolver.storage.cached_runtime_types.type_info_enumeration);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_String", &resolver.storage.cached_runtime_types.type_info_string);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Pointer", &resolver.storage.cached_runtime_types.type_info_pointer);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Relative_Pointer", &resolver.storage.cached_runtime_types.type_info_relative_pointer);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Layout_Pointer", &resolver.storage.cached_runtime_types.type_info_layout_pointer);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Dynamic_Pointer", &resolver.storage.cached_runtime_types.type_info_dynamic_pointer);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Array", &resolver.storage.cached_runtime_types.type_info_array);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Dynamic_Array", &resolver.storage.cached_runtime_types.type_info_dynamic_array);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Slice", &resolver.storage.cached_runtime_types.type_info_slice);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Map", &resolver.storage.cached_runtime_types.type_info_map);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Tuple", &resolver.storage.cached_runtime_types.type_info_tuple);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Struct", &resolver.storage.cached_runtime_types.type_info_struct);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Union", &resolver.storage.cached_runtime_types.type_info_union);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Routine", &resolver.storage.cached_runtime_types.type_info_routine);
  resolver_cache_runtime_package_type(resolver, runtime_package, "Type_Info_Interface", &resolver.storage.cached_runtime_types.type_info_interface);
}

/**
* Cache a special 'runtime' package type.
*
* @param resolver        The reference to the resolver.
* @param runtime_package The 'runtime' package.
* @param name            The name of the special 'runtime' package type.
* @param type_storage    Pointer to the type that will hold the special 'runtime' package type.
*/
resolver_cache_runtime_package_type :: proc(resolver: ^Resolver, runtime_package: ^Package, name: string, type_storage: ^^Type) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  symbol := package_get_symbol_defined_anywhere_in_package(runtime_package, name, .All);
  assert(symbol != nil);
  resolver_resolve_symbol(resolver, symbol);
  resolver_complete_type(resolver, symbol.declaration.position, symbol.type);
  type_storage^ = symbol.type;
}

/**
* Setup the compiler constants of the 'runtime' package.
*
* @param resolver        The reference to the resolver.
* @param runtime_package The 'runtime' package.
* @param file            The package file the constants will live in.
*/
resolver_setup_compiler_constants :: proc(resolver: ^Resolver, file: ^Package_File) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  nox_operating_system_kind_value: Value;
  when ODIN_OS == .Windows {
    nox_operating_system_kind_value = cast(int) 0;
  } else when ODIN_OS == .Linux {
    nox_operating_system_kind_value = cast(int) 1;
  } else {
    assert(false);
  }
  resolver_add_builtin_constant(resolver, file, "NOX_OS", resolver.storage.cached_runtime_types.operating_system_kind, nox_operating_system_kind_value);

  compiler := cast(^Compiler) context.user_ptr;

  nox_debug_constant: Value = cast(bool) compiler.arguments.build_debug;
  resolver_add_builtin_constant(resolver, file, "NOX_DEBUG", resolver.storage.type_bool, nox_debug_constant);
  nox_rttr_disabled_constant: Value = cast(bool) compiler.arguments.rttr_disabled;
  resolver_add_builtin_constant(resolver, file, "NOX_RTTR_DISABLED", resolver.storage.type_bool, nox_rttr_disabled_constant);
  nox_assert_disabled_constant: Value = cast(bool) compiler.arguments.assert_disabled;
  resolver_add_builtin_constant(resolver, file, "NOX_ASSERT_DISABLED", resolver.storage.type_bool, nox_assert_disabled_constant);
}

/**
* Resolves all directives in a package.
*
* @param resolver The reference to the resolver.
* @param pack     The package whose directives to resolve.
*/
resolver_resolve_package_directives :: proc(resolver: ^Resolver, pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for _, file in pack.files {
    for declaration in file.declarations {
      if declaration.kind == .Directive {
        resolver_resolve_directive(resolver, file, declaration);
      }
    }
  }
}

/**
* Resolves a directive in a package file.
*
* @param resolver    The reference to the resolver.
* @param file        The package file the directive belongs to.
* @param declaration The directive declaration.
*/
resolver_resolve_directive :: proc(resolver: ^Resolver, file: ^Package_File, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration_directive := cast(^Declaration_Directive) declaration;

  switch declaration_directive.directive_kind {
    case .Assert: resolver_resolve_directive_assert(resolver, declaration.position, cast(^Declaration_Directive_Assert) declaration_directive);
    case .If: resolver_resolve_directive_if(resolver, file, cast(^Declaration_Directive_If) declaration_directive);
    case .Expand_Context: resolver_resolve_directive_expand_context(resolver, declaration.position, cast(^Declaration_Directive_Expand_Context) declaration_directive);
  }
}

/**
* Resolves an assertion directive.
*
* @param resolver                     The reference to the resolver.
* @param position                     The position of the directive.
* @param declaration_directive_assert The assertion directive declaration.
*/
resolver_resolve_directive_assert :: proc(resolver: ^Resolver, position: Source_Position, declaration_directive_assert: ^Declaration_Directive_Assert) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assertion := resolver_resolve_directive_condition(resolver, declaration_directive_assert.expression);
  if !assertion.value.(bool) {
    report_error_fatal(position, "Static assertion failed");
  }
}

/**
* Resolves an if directive.
*
* @param resolver                     The reference to the resolver.
* @param position                     The position of the directive.
* @param declaration_directive_if The if directive declaration.
*/
resolver_resolve_directive_if :: proc(resolver: ^Resolver, file: ^Package_File, declaration_directive_if: ^Declaration_Directive_If) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  condition := resolver_resolve_directive_condition(resolver, declaration_directive_if.condition);
  if condition.value.(bool) {
    resolver_resolve_directive_if_block(resolver, file, declaration_directive_if.then_declarations);
  } else {
    for else_if in declaration_directive_if.else_ifs {
      else_if_condition := resolver_resolve_directive_condition(resolver, else_if.condition);
      if else_if_condition.value.(bool) {
        resolver_resolve_directive_if_block(resolver, file, else_if.declarations);
        // We stop when we found a condition we resolved. 
        return;
      }
    }

    resolver_resolve_directive_if_block(resolver, file, declaration_directive_if.else_declarations);
  }
}

/**
* Resolves an if directive block.
*
* @param resolver     The reference to the resolver.
* @param file         The file the directive belongs to.
* @param declarations The declarations of the if directive block.
*/
resolver_resolve_directive_if_block :: proc(resolver: ^Resolver, file: ^Package_File, declarations: [dynamic]^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for declaration in declarations {
    if declaration.kind == .Directive {
      // We do not append directives to 'file.directive_declarations' as we can simply recursively resolve them here directly.
      resolver_resolve_directive(resolver, file, declaration);
    } else if declaration.kind == .Import {
      package_process_import(file, declaration);
    } else {
      resolver_add_package_declaration(resolver, file, declaration); 
    }
  }
}

/**
* Resolves an if directive block.
*
* @param resolver   The reference to the resolver.
* @param expression The condition expression of the if directive.
* @return The resolved operand.
*/
resolver_resolve_directive_condition :: proc(resolver: ^Resolver, expression: ^Expression) -> Operand {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  operand := resolver_resolve_expression_constant(resolver, expression, STATEMENT_CONTEXT);
  if .Is_Constant not_in operand.flags {
    report_error_fatal(expression.position, "Condition in static directive must be constant");
  }
  if !type_is_boolean(operand.type) {
    report_error_fatal(expression.position, "Condition in static directive must have a boolean type. Got '%v'", operand.type.name);
  }
  return operand;
}

/**
* Resolves an expand context directive.
*
* @param resolver                             The reference to the resolver.
* @param position                             The position of the directive.
* @param declaration_directive_expand_context The expand context directive declaration.
*/
resolver_resolve_directive_expand_context :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  declaration_directive_expand_context: ^Declaration_Directive_Expand_Context,
) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  field_type := resolver_resolve_type_specification(resolver, declaration_directive_expand_context.field_type);
  append_soa(&resolver.context_fields, Context_Field{declaration_directive_expand_context.field_name, field_type});

  // For now we do a simple check for duplicate field entries and bail out.
  for i := 0; i < len(resolver.context_fields); i += 1 {
    for j := i + 1; j < len(resolver.context_fields); j += 1 {
      field_a := resolver.context_fields[i];
      field_b := resolver.context_fields[j];
      if field_a.name == field_b.name {
        report_error_fatal(position, "Duplicate field name '%v' in context", field_a.name);
      }
    }
  }
}

/**
* Adds all declarations of package.
*
* @param resolver The reference to the resolver.
* @param pack     The package whose declarations to add.
*/
resolver_add_package_declarations :: proc(resolver: ^Resolver, pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for _, file in pack.files {
    for declaration in file.declarations {
      resolver_add_package_declaration(resolver, file, declaration);
    }
  }
}

/**
* Adds a single declarations of package file.
*
* @param resolver    The reference to the resolver.
* @param file        The package file the declaration belongs to.
* @param declaration The declaration to add.
*/
resolver_add_package_declaration :: proc(resolver: ^Resolver, file: ^Package_File, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if declaration.kind == .Import || declaration.kind == .Directive {
    return;
  } else if declaration.kind == .Implementation {
    resolver_add_global_implementation(resolver, file, declaration);
  } else {
    symbol := symbol_make_declaration(resolver, file, declaration);
    resolver_add_global_symbol(resolver, file, symbol.name, symbol);
    resolver_check_declaration_attributes(resolver, file, declaration, symbol);
  }
}

/**
* Checks the attributes of a declaration.
*
* @param resolver    The reference to the resolver.
* @param file        The package file the declaration belongs to.
* @param declaration The declaration to check.
* @param symbol      The symbol of the declaration.
*/
resolver_check_declaration_attributes :: proc(resolver: ^Resolver, file: ^Package_File, declaration: ^Declaration, symbol: ^Symbol) {
  for attribute in declaration.attributes.attributes {
    switch attribute.kind {
      case .Flags: {
        if declaration.kind != .Enumeration {
          report_error_fatal(attribute.position, "The attribute '%v' is only valid for enum declarations", ATTRIBUTE_NAMES[attribute.kind]);
        }
        if len(attribute.arguments) > 0 {
          report_error_fatal(attribute.position, "The attribute '%v' does not have any arguments", ATTRIBUTE_NAMES[attribute.kind]);
        }
        symbol.flags += {.Enum_Flags};
      }
      case .Thread_Local: {
        if declaration.kind != .Global {
          report_error_fatal(attribute.position, "The attribute '%v' is only valid for global variable declarations", ATTRIBUTE_NAMES[attribute.kind]);
        }
        if len(attribute.arguments) > 0 {
          report_error_fatal(attribute.position, "The attribute '%v' does not have any arguments", ATTRIBUTE_NAMES[attribute.kind]);
        }
        symbol.flags += {.Global_Thread_Local};
      }
      case .Disabled, .Intrinsic, .Builtin: {
        if declaration.kind != .Procedure && declaration.kind != .Function {
          report_error_fatal(attribute.position, "The attribute '%v' is only valid for procedures or functions", ATTRIBUTE_NAMES[attribute.kind]);
        }
        if attribute.kind == .Intrinsic || attribute.kind == .Builtin {
          if len(attribute.arguments) > 0 {
            report_error_fatal(attribute.position, "The attribute '%v' does not have any arguments", ATTRIBUTE_NAMES[attribute.kind]);
          }
          compiler := cast(^Compiler) context.user_ptr;
          if (file.pack != compiler.runtime_package) {
            report_error_fatal(attribute.position, "The attribute '%v' is not allowed outside the 'runtime' package", ATTRIBUTE_NAMES[attribute.kind]);
          }
          
          if attribute.kind == .Builtin {
            // We consider builtin routines to be always reachable.
            append(&resolver.always_reachable_symbols, symbol);
            symbol.flags += {.Routine_Builtin};
          } else {
            assert(attribute.kind == .Intrinsic);
            symbol.flags += {.Routine_Intrinsic};
          }
        }
      }
      case .Invalid: fallthrough;
      case: assert(false);
    }
  }
}

/**
* Start the resolving phase.
*
* @param resolver The reference to the resolver.
*/
resolver_resolve :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_resolve_context(resolver);

  // We resolve always reachable symbols first.
  for always_reachable_symbol, _ in resolver.always_reachable_symbols {
    resolver_resolve_symbol(resolver, always_reachable_symbol);
  }

  // We have to resolve implementations manually here as they are not directly a global symbol.
  resolver_resolve_implementations(resolver);

  // We iterate manually here as 'reachable_symbols' gets populated inside the loop.
  for i := 0; i < len(resolver.reachable_symbols); i += 1 {
    reachable_symbol := resolver.reachable_symbols[i];
    resolver_finalize_symbol(resolver, reachable_symbol);
  }

  // All reachable symbols are now found and ordered so copy them over to the resolver output to be consumed by the Generator.
  resize(&resolver.output.reachable_sorted_symbols, len(resolver.sorted_symbols));
  copy(resolver.output.reachable_sorted_symbols[:], resolver.sorted_symbols[:]);

  // We also manually copy over all reachable implementations that correspond to interfaces.
  // That way the Generator can easily consume them and generate the appropriate vtables.
  for key, resolved_implementation in resolver.output.resolved_implementations {
    if (key.interface_type == nil || .Reachable not_in key.interface_type.symbol.flags) do continue;
    resolver.output.reachable_interface_implementations[key] = resolved_implementation;
  }

  // Every type that is now stored in the storage after resolving every reachable symbol is now a reachable type.
  reserve(&resolver.output.reachable_types, len(resolver.storage.types));
  for type in resolver.storage.types {
    append(&resolver.output.reachable_types, type);
  }
  
  // At the end we resolve and finalize every unreachable symbol.
  // We make a copy of the unreachable symbols here as elements would otherwise get removed while iterating.
  // We allocate the array globally for now as the temporary allocate is being used when finalizing a symbol.
  unreachable_symbols_copy := make_dynamic_array_len_cap([dynamic]^Symbol, 0, len(resolver.unreachable_symbols));
  defer delete(unreachable_symbols_copy);
  for unreachable_symbol, _ in resolver.unreachable_symbols {
    append(&unreachable_symbols_copy, unreachable_symbol);
  }
  for unreachable_symbol in unreachable_symbols_copy {
    resolver_resolve_symbol(resolver, unreachable_symbol);
    resolver_finalize_symbol(resolver, unreachable_symbol);
  }
}

/**
* Resolves the special 'Context' struct.
*
* @param resolver The reference to the resolver.
*/
resolver_resolve_context :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  compiler := cast(^Compiler) context.user_ptr;
  context_symbol := package_get_symbol_defined_anywhere_in_package(compiler.runtime_package, "Context", .All);
  context_type := context_symbol.type;

  fields: #soa [dynamic]Type_Struct_Field;
  for field in resolver.context_fields {
    resolver_complete_type(resolver, SOURCE_POSITION_BUILTIN, field.type);
    append_soa(&fields, Type_Struct_Field{.Public, false, field.name, field.type, 0});
  }
  type_complete_struct(context_type, fields, LAYOUT_INFO_NONE);
  context_type.state = .Completed;
}

/**
* Trys to get the attribute of declaration.
*
* @param declaration The declaration whose attribute to get.
* @param kind        The kind of the attribute to get.
* @return 1. The attribute if found; 2. True if the attribute could be found otherwise false.
*/
resolver_try_get_attribute :: proc(declaration: ^Declaration, kind: Attribute_Kind) -> (Attribute, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for attribute in declaration.attributes.attributes {
    if attribute.kind == kind {
      return attribute, true;
    }
  }

  return {}, false;
}

/**
* Completes a type.
*
* @param resolver The reference to the resolver.
* @param position The position of the type declaration.
* @param type     The type to complete.
*/
resolver_complete_type :: proc(resolver: ^Resolver, position: Source_Position, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if .Uninstantiated_Generic in type.flags {
    return;
  } else if type.state == .Completed {
    return;
  } else if type.state == .Completing {
    report_error_fatal(position, "Illegal cyclic dependency of type '%s'", type.name);
    return;
  }

  assert(type.state == .Incomplete);
  type.state = .Completing;
  
  symbol := type.symbol;
  declaration := symbol.declaration;

  previous_package := package_enter(symbol.package_file.pack);

  if declaration.kind == .Struct {
    resolver_complete_type_struct(resolver, type, declaration);
  } else if declaration.kind == .Union {
    resolver_complete_type_union(resolver, type, declaration);
  } else {
    assert(false);
  }

  type.state = .Completed;

  append(&resolver.sorted_symbols, type.symbol);

  package_leave(previous_package);
}

/**
* Completes a struct type.
*
* @param resolver    The reference to the resolver.
* @param type        The struct type to complete.
* @param declaration The declaration of the type.
*/
resolver_complete_type_struct :: proc(resolver: ^Resolver, type: ^Type, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(type.state == .Completing);

  declaration_struct := cast(^Declaration_Struct) declaration;

  generic_context: Generic_Context;
  generic_context.type_names = declaration_struct.generic_type_names;
  generic_context.generic_types = (cast(^Type_Struct) type).generic_types;

  struct_name := declaration.name;

  field_names := make_dynamic_array([dynamic]string, context.temp_allocator);
  fields: #soa [dynamic]Type_Struct_Field;
  for field in declaration_struct.fields {
    field_type := resolver_resolve_type_specification(resolver, field.type, generic_context);
    resolver_complete_type(resolver, field.position, field_type);
    if .Not_Allowed_As_Field in field_type.flags {
      report_error_fatal(field.position, "Type '%v' is not allowed as a struct field", field_type.name);
    }
    if field.is_composite && !type_is_struct(field_type) {
      report_error_fatal(field.position, "Only fields with a struct type can be composited. Got '%v'", field_type.name);
    }

    access_modifier := field.access_modifier;
    if access_modifier == .Member_Unspecified {
      access_modifier = declaration.access_modifier;
    }
    if declaration.access_modifier < access_modifier {
      report_error_fatal(field.position, "Field has a higher access modifier than the struct it is defined in");
    }
    if field_type.symbol != nil {
      if field_type.symbol.access_modifier < access_modifier {
        report_error_fatal(field.type.position, "Type '%v' has a higher access modifier than '%v'", field_type.name, declaration.name);
      }
    }

    name := field.name;
    
    // First check for field duplicates including composited structs (before appending).
    for field_name in field_names {
      if name == field_name {
        report_error_fatal(field.position, "Struct '%v' already defines a field with the name '%v'", struct_name, name);
      }
    }
    
    resolver_check_for_method_name_clash(field.position, type.methods, name, struct_name);
    for interface in type.interfaces {
      resolver_check_for_method_name_clash(field.position, interface.methods, name, struct_name);
    }

    resolver_add_struct_field_names_recursive(&field_names, name, field.is_composite, field_type);
    append_soa(&fields, Type_Struct_Field{access_modifier, field.is_composite, field.name, field_type, 0});
  }

  type_complete_struct(type, fields, LAYOUT_INFO_NONE);
}

/**
* Recursively add the fields of a struct.
*
* @param names        The names .
* @param field_name   The name of the field.
* @param is_composite Is the field marked with the 'composite' keyword.
* @param field_type   The type of the field.
*/
resolver_add_struct_field_names_recursive :: proc(names: ^[dynamic]string, field_name: string, is_composite: bool, field_type: ^Type) {
  append(names, field_name);
  if (is_composite) {
    type_struct := cast(^Type_Struct) field_type;
    for field in type_struct.fields {
      resolver_add_struct_field_names_recursive(names, field.name, field.is_composite, field.type);
    }
  }
}

/**
* Check for name clashes with already existing methods.
*
* @param position    The position of the method.
* @param methods     The methods to check in.
* @param method_name The name of the method to check.
* @param type_name   The name of the method type.
*/
resolver_check_for_method_name_clash :: proc(position: Source_Position, methods: #soa [dynamic]Type_Method, method_name: string, type_name: string) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for method in methods {
    if method.name == method_name {
      report_error_fatal(position, "Type '%v' already implements a method named '%v'", type_name, method_name);
    }
  }
}

/**
* Completes a union type.
*
* @param resolver    The reference to the resolver.
* @param type        The union type to complete.
* @param declaration The declaration of the type.
*/
resolver_complete_type_union :: proc(resolver: ^Resolver, type: ^Type, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration_union := cast(^Declaration_Union) declaration;

  types: [dynamic]^Type;
  for type_spec in declaration_union.types {
    union_element_type := resolver_resolve_type_specification(resolver, type_spec);
    resolver_complete_type(resolver, type_spec.position, union_element_type);
    if .Not_Allowed_As_Union in union_element_type.flags {
      report_error_fatal(type_spec.position,  "Type '%v' is not allowed as a union variant", union_element_type.name);
    }
    append(&types, union_element_type);
  }

  for i := 0; i < len(types) - 1; i += 1 {
    type_a := types[i];
    type_b := types[i + 1];
    type_spec_b := declaration_union.types[i + 1];

    if type_a == type_b {
      report_error_fatal(type_spec_b.position, "Duplicate type '%v' in union is not allowed", type_a.name);
    }
  }

  type_complete_union(type, types);
}

/**
* Marks a type for RTTR generation.
*
* @param resolver The reference to the resolver.
* @param position The position of the type.
* @param type     The type to mark.
*/
resolver_mark_type_for_rttr_generation :: proc(resolver: ^Resolver, position: Source_Position, type: ^Type) {
  if .Generate_RTTR in type.flags {
    return;
  }

  resolver_complete_type(resolver, position, type);
  type.flags += {.Generate_RTTR};

  if type.base != nil {
    resolver_mark_type_for_rttr_generation(resolver, position, type.base);
  }

  #partial switch type.kind {
    case .Self_Relative_Pointer, .Offset_Relative_Pointer: {
      type_relative_pointer := cast(^Type_Relative_Pointer) type;
      resolver_mark_type_for_rttr_generation(resolver, position, type_relative_pointer.relative_base);
    }

    case .Map: {
      type_map := cast(^Type_Map) type;
      resolver_mark_type_for_rttr_generation(resolver, position, type_map.key);
      resolver_mark_type_for_rttr_generation(resolver, position, type_map.value);
    }
    case .Tuple: {
      type_tuple := cast(^Type_Tuple) type;
      for element in type_tuple.elements {
        resolver_mark_type_for_rttr_generation(resolver, position, element.type);
      }
    }

    case .Struct: {
      type_struct := cast(^Type_Struct) type;
      for field in type_struct.fields {
        resolver_mark_type_for_rttr_generation(resolver, position, field.type);
      }
    }
    case .Union: {
      type_union := cast(^Type_Union) type;
      for variant in type_union.variants {
        resolver_mark_type_for_rttr_generation(resolver, position, variant);
      }
    }

    case .Procedure, .Function: {
      type_routine := cast(^Type_Routine) type;
      for parameter in type_routine.parameters {
        resolver_mark_type_for_rttr_generation(resolver, position, parameter.type);
      }
      resolver_mark_type_for_rttr_generation(resolver, position, type_routine.return_type);
    }
  }
}

/**
* Resolves a name to a symbol.
*
* @param resolver The reference to the resolver.
* @param position The position of the name to resolve.
* @param pack     The package to resolve the name in.
* @param name     The name to resolve.
* @return The resolved symbol.
*/
resolver_resolve_name :: proc(resolver: ^Resolver, position: Source_Position, pack: ^Package, name: string) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  local_symbol := resolver_get_local_symbol(resolver, name);  
  if local_symbol != nil {
    local_symbol.flags += {.Local_Used};
    return local_symbol;
  }

  symbol := package_get_symbol(pack, position, name, .All);
  resolver_resolve_symbol(resolver, symbol);

  return symbol;
}

/**
* Resolves an initializer.
*
* @param resolver          The reference to the resolver.
* @param position          The position of the initializer.
* @param statement_context The statement context to use.
* @param typespec          The type specification of the initializer to resolve.
* @param expression        The expression of the initializer to resolve.
* @return The resolved type.
*/
resovler_resolve_initializer :: proc(
  resolver: ^Resolver,
  position: Source_Position,
  statement_context: Statement_Context,
  typespec: ^Type_Specification,
  expression: ^Expression,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type: ^Type;
  if typespec != nil {
    declared_type := resolver_resolve_type_specification(resolver, typespec);
    type = declared_type;

    if expression != nil {
      inferred_type, success := resolver_resolve_initializer_typed(resolver, statement_context, declared_type, expression);
      if !success {
        report_error_fatal(position, "Invalid type in initializer. Expected '%v' got '%v'", declared_type.name, inferred_type.name);
      }
      if type_is_array_and_incomplete(type) {
        resolver_set_resolved_type(resolver, typespec, inferred_type);
        type = inferred_type;
      }
    }
  } else {
    assert(expression != nil);

    resolved_expression := resolver_resolve_expression(resolver, expression, statement_context);
    operand_remove_untyped(resolver.storage, expression.position, &resolved_expression);
    type = resolved_expression.type;

    if type_is_untyped_null(type) {
      report_error_fatal(position, "Cannot deduce type from 'untyped null'");
    }

    resolver_set_resolved_type(resolver, expression, type);
    resolver_set_resolved_overwrite_type(resolver, expression, type);
  }

  // Incomplete array types need explicit handling here as they are allowed as variables but only with an corresponding initializer expression.
  if .Not_Allowed_As_Variable in type.flags {
    if type_is_void(type) && expression != nil && expression.kind == .Call {
      report_error_fatal(position, "Routine call does not return a value and cannot be used as a value");
    } else {
      report_error_fatal(position, "Type '%v' is not allowed as a local or global variable", type.name);
    }
  } else if type_is_array_and_incomplete(type) && expression == nil {
    report_error_fatal(position, "Incomplete array of type '%v' is missing its initializer expression", type.name);
  }

  resolver_complete_type(resolver, position, type);

  return type;
}

/**
* Resolves a initializer with an expected type.
*
* @param resolver          The reference to the resolver.
* @param statement_context The statement context to use.
* @param type              The expected type of the initializer.
* @param expression        The expression of the initializer to resolve.
* @return 1. The resolved type; 2. True if the initializer could be converted to the expected type otherwise false.
*/
resolver_resolve_initializer_typed :: proc(resolver: ^Resolver, statement_context: Statement_Context, type: ^Type, expression: ^Expression) -> (^Type, bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  expected_type := type_unqualify(type);

  operand := resolver_resolve_expression_expected(resolver, expression, statement_context, expected_type);

  if type_is_array_and_incomplete(type) {
    if type.base == operand.type.base {
      complete_array := type_storage_get_or_make_type_array(
        resolver.storage,
        LAYOUT_INFO_NONE,
        operand.type.base,
        (cast(^Type_Array) operand.type).number_of_elements,
        false,
      );
      resolver_set_resolved_type(resolver, expression, complete_array);
      resolver_set_resolved_overwrite_type(resolver, expression, complete_array);
      return complete_array, true;
    }
  }

  if !operand_convert(resolver, expression.position, &operand, expected_type) {
    return operand.type, false;
  }

  resolver_set_resolved_overwrite_type(resolver, expression, operand.type);

  return operand.type, true;
}

/**
* Gets a local symbol by name.
*
* @param resolver The reference to the resolver.
* @param name     The name of the local symbol.
* @return The local symbol.
*/
resolver_get_local_symbol :: proc(resolver: ^Resolver, name: string) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  return resolver_get_local_symbol_in_block_recursive(resolver.local_scope, name);
}

/**
* Gets a local symbol by name recursively by looking into parent scopes.
*
* @param local_scope The current local scope to look in.
* @param name        The name of the local symbol.
* @return The local symbol.
*/
resolver_get_local_symbol_in_block_recursive :: proc(local_scope: ^Local_Scope, name: string) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if local_scope == nil {
    return nil;
  }

  local_symbol, found := local_scope.symbols[name];
  if found {
    local_symbol := &local_scope.symbols[name];
    return &local_symbol.symbol;
  }

  return resolver_get_local_symbol_in_block_recursive(local_scope.parent_block, name);
}

/**
* Set a resolved symbol.
*
* @param resolver The reference to the resolver.
* @param key      The AST key of the resolved symbol.
* @param symbol   The resolved symbol.
*/
resolver_set_resolved_symbol :: proc(resolver: ^Resolver, key: ^Expression, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // Locals and parameters are not persistent and are therefore never 'globally' resolved.
  if symbol.kind != .Local && symbol.kind != .Parameter {
    resolver.output.resolved_symbols[key] = symbol;
  }
}

/**
* Set a resolved type.
*
* @param resolver The reference to the resolver.
* @param key      The AST key of the resolved type.
* @param type     The resolved type.
*/
resolver_set_resolved_type :: proc(resolver: ^Resolver, key: rawptr, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.output.resolved_types[key] = type;
}

/**
* Set a resolved overwritten type.
*
* @param resolver The reference to the resolver.
* @param key      The AST key of the resolved overwritten type.
* @param type     The resolved overwritten type.
*/
resolver_set_resolved_overwrite_type :: proc(resolver: ^Resolver, key: ^Expression, type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.output.resolved_overwrite_types[key] = type;
}

/**
* Set a resolved constant.
*
* @param resolver The reference to the resolver.
* @param key      The AST key of the resolved constant.
* @param constant The resolved constant.
*/
resolver_set_resolved_constant :: proc(resolver: ^Resolver, key: ^Expression, constant: Operand) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.output.resolved_constants[key] = constant;
}

/**
* Set a resolved member.
*
* @param resolver The reference to the resolver.
* @param key      The AST key of the resolved member.
* @param member   The resolved member.
*/
resolver_set_resolved_member :: proc(resolver: ^Resolver, key: ^Expression, member: Resolved_Member) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.output.resolved_members[key] = member;
}

/**
* Set a resolved compound field.
*
* @param resolver       The reference to the resolver.
* @param key            The AST key of the resolved compound field.
* @param compound_field The resolved compound field.
*/
resolver_set_resolved_compound_field :: proc(resolver: ^Resolver, key: ^Expression, compound_field: Resolved_Compound_Field) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver.output.resolved_compound_fields[key] = compound_field;
}

/**
* Enters a new local scope.
*
* @param resolver The reference to the resolver.
*/
resolver_enter_local_scope :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  scope := resolver.local_scope;
  
  // Currently we allocate local scopes with the regular heap allocator.
  // We would like to use a 'Memory_Pool' but for some reason it does not properly handle alignment.
  // That causes issues for the symbol map which needs a specific alignment when requesting memory.
  local_scope := new(Local_Scope);
  local_scope.parent_block = scope;
  resolver.local_scope = local_scope;
}

/**
* Leaves the current local scope.
*
* @param resolver The reference to the resolver.
*/
resolver_leave_local_scope :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  scope := resolver.local_scope;

  // Before leaving we check for locals that got declared but not used.
  for name, local_symbol in scope.symbols {
    if .Local_Declared in local_symbol.symbol.flags && .Local_Used not_in local_symbol.symbol.flags {
      symbol_description_name := local_symbol.symbol.kind == .Parameter ? "parameter" : "local variable";
      report_warning(local_symbol.position, "The %v '%v' got declared but not used", symbol_description_name, name);
    }
  }

  if scope != nil {
    resolver.local_scope = scope.parent_block;
    delete(scope.symbols);
    free(scope);
  } else {
    resolver.local_scope = nil;
  }
}

/**
* Makes a formatted symbol.
*
* @param resolver The reference to the resolver.
* @param format   The format of the name.
* @param args     The arguments of the format.
* @return The formatted symbol name.
*/
resolver_make_formatted_symbol_name :: proc(resolver: ^Resolver, format: string, args: ..any) -> string {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  builder := strings.builder_make(resolver.name_allocator);
  fmt.sbprintf(&builder, format, ..args);
  symbol_name := strings.to_string(builder);
  return symbol_name;
}
