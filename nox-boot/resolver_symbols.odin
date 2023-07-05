package nox

import "tracy"

/**
* Represents a constant value.
*/
Value :: union {
  b8,
  b16,
  b32,
  b64,
  bool,
  i8,
  i16,
  i32,
  i64,
  int,
  u8,
  u16,
  u32,
  u64,
  uint,
  f32,
  f64,
  rune,
  string,

  // HACK: Those are strictly not an actual valid value but it is quite convenient to save on space in 'Symbol'.
  ^Package, // Necessary for 'Package' symbols.
  ^Type,    // Used for accessing the implemented type in symbols that are flagged as 'Routine_Method_Implemented'.
}

/**
* Creates the default value for a type.
* 
* @param type The type whose default value to get.
* @return The default value.
*/
value_default :: proc(type: ^Type) -> Value {
  type := type_unqualify(type);
  if type_is_enumeration(type) {
    type = type.base;
  }

  #partial switch type.kind {
    case .B8:     return cast(b8)     false;
    case .B16:    return cast(b16)    false;
    case .B32:    return cast(b32)    false;
    case .B64:    return cast(b64)    false;
    case .Bool:   return cast(bool)   false;
    case .I8:     return cast(i8)     0;
    case .I16:    return cast(i16)    0;
    case .I32:    return cast(i32)    0;
    case .I64:    return cast(i64)    0;
    case .Int:    return cast(int)    0;
    case .U8:     return cast(u8)     0;
    case .U16:    return cast(u16)    0;
    case .U32:    return cast(u32)    0;
    case .U64:    return cast(u64)    0;
    case .UInt:   return cast(uint)   0;
    case .F32:    return cast(f32)    0;
    case .F64:    return cast(f64)    0;
    case .Char:   return cast(rune)   0;
    case .String: return cast(string) "";
  }

  return nil;
}

/**
* The kind of a symbol.
*/
Symbol_Kind :: enum {
  None,     // No/Invalid symbol.

  // Regular global declarations.
  Constant,  // A constant symbol ('const').
  Global,    // A global symbol ('global').
  Type,      // A type symbol ('struct', 'union', 'enum').
  Routine,   // A routine symbol ('proc', 'func').

  // Special symbols with limited scope.
  Local,     // A local symbol inside a routine.
  Parameter, // A parameter symbol of a routine.

  // Special symbol referring to a different package.
  Package,   // A package symbol (defined by an import).
}

/**
* The state of a symbol.
*/
Symbol_State :: enum {
  Unresolved,                 // The symbol is still unresolved.
  Resolving,                  // The symbol is currently being resolved.
  Resolved,                   // The symbol got fully resolved.
  Resolved_But_Not_Reachable, // The symbol got resolved but is not yet determined reachable.
}

/**
* The flags for a symbol
*/
Symbol_Flag :: enum {
  Reachable,                  // The symbol is reachable inside the user program.
  
  Uninstantiated_Generic,     // The symbol is an uninstantiated generic (type or routine).
  
  Enum_Flags,                 // The enumeration is marked with the attribute 'flags'.

  Global_Thread_Local,        // The global is marked with the attribute 'threadlocal'.

  Routine_Function,           // The routine is a function.
  Routine_Function_Pure,      // The function routine is marked with the attribute 'pure'.
  Routine_Builtin,            // The routine is marked with the attribute 'builtin'.
  Routine_Intrinsic,          // The routine is marked with the attribute 'intrinsic'.
  Routine_Disabled,           // The routine is marked with the attribute 'disabled' (Added only when the symbol got resolved).
  Routine_Instantiated,       // The routine has instantiated generic type parameters (Added only when the symbol got resolved).
  Routine_Method_Interface,   // The routine is part of an interface declaration (Added only when the symbol got resolved).
  Routine_Method_Implemented, // The routine is part of an implementation (Added only when the symbol got resolved).

  Local_Declared,             // The local got declared through a dedicated initialize statement.
  Local_Used,                 // The local got used after being declared.
  Local_Immutable,            // The local symbol is immutable.
}

Symbol_Flags :: bit_set[Symbol_Flag];

/**
* Represents a symbol referred to by name.
*/
Symbol :: struct {
  kind: Symbol_Kind,                // The kind of the symbol.
  state: Symbol_State,              // The state of the symbol.
  flags: Symbol_Flags,              // The flags of the symbol.
  access_modifier: Access_Modifier, // The access modifier of the symbol

  name: string,                     // The name of the symbol
  package_file: ^Package_File,      // The package file the symbol belongs to.

  declaration: ^Declaration,        // The AST declaration that defines the symbol (can be null for built-in types).

  type: ^Type,                      // The type of the symbol (gets filled in after resolving).
  value: Value,                     // The value of constant symbols (gets filled in after resolving) but can also holds special values for certain symbols.
}

/**
* Represents a local symbol
*/
Local_Symbol :: struct {
  symbol: Symbol,            // The symbol of the local variable.
  position: Source_Position, // The position the local variable is declared.
}

/**
* Represents intrinsic routines the compiler handles directly.
*/
Intrinsic_Routine :: enum {
  Invalid,                   // No/Ivalid routine.

  Entry_Point,               // The 'entry_point' routine.

  Data,                      // The 'data' routine.
  Length,                    // The 'length' routine.
  Capacity,                  // The 'capacity' routine.

  Hash_Function_Of_Type,     // The 'hash_function_of_type' routine.
  Compare_Function_Of_Type,  // The 'compare_function_of_type' routine.

  Trap,                      // The 'trap' routine.

  Type_Is_Scalar,            // The 'hash_function_of_type' routine.
  Type_Is_Enum,              // The 'type_is_enum' routine.
  Type_Is_Struct,            // The 'type_is_struct' routine.
  Type_Is_AoSoA,             // The 'type_is_aosoa' routine.
  Type_Is_Trivial_Copyable,  // The 'type_is_trivial_copyable' routine.
  Type_Enum_Item_Count,      // The 'type_enum_item_count' routine.
  Type_Struct_Field_Count,   // The 'type_struct_field_count' routine.
  Type_AoSoA_Chunk_Size,     // The 'type_aosoa_chunk_size' routine.
  Type_AoSoA_Items_In_Chunk, // The 'type_aosoa_items_in_chunk' routine.
}

/**
* The mappings of routine names to their intrinsic routine type.
*/
INTRINSIC_ROUTINES := map[string]Intrinsic_Routine {
  "entry_point" = .Entry_Point,

  "data" = .Data,
  "length" = .Length,
  "capacity" = .Capacity,

  "hash_function_of_type" = .Hash_Function_Of_Type,
  "compare_function_of_type" = .Compare_Function_Of_Type,

  "trap" = .Trap,

  "type_is_scalar" = .Type_Is_Scalar,
  "type_is_enum" = .Type_Is_Enum,
  "type_is_struct" = .Type_Is_Struct,
  "type_is_aosoa" = .Type_Is_AoSoA,
  "type_is_trivial_copyable" = .Type_Is_Trivial_Copyable,
  "type_enum_item_count" = .Type_Enum_Item_Count,
  "type_struct_field_count" = .Type_Struct_Field_Count,
  "type_aosoa_chunk_size" = .Type_AoSoA_Chunk_Size,
  "type_aosoa_items_in_chunk" = .Type_AoSoA_Items_In_Chunk,
}

/**
* Makes a new symbol.
*
* @param resolver        The reference to the resolver.
* @param kind            The kind of the symbol.
* @param access_modifier The access modifier of the symbol.
* @param package_file    The package file the symbol belongs to.
* @param name            The name of the symbol.
* @param declaration     The declaration that defines the symbol.
* @return The new symbol.
*/
symbol_make :: proc(
  resolver: ^Resolver,
  kind: Symbol_Kind,
  access_modifier: Access_Modifier,
  package_file: ^Package_File,
  name: string,
  declaration: ^Declaration,
) -> ^Symbol {
  symbol := new(Symbol, resolver.symbol_allocator);
  symbol.kind = kind;
  symbol.state = .Unresolved;
  symbol.access_modifier = access_modifier;
  symbol.name = name;
  symbol.package_file = package_file;
  symbol.declaration = declaration;
  return symbol;
}

/**
* Makes a symbol from a declaration.
*
* @param resolver     The reference to the resolver.
* @param package_file The package file the symbol belongs to.
* @param declaration  The declaration that defines the symbol.
* @return The new symbol.
*/
symbol_make_declaration :: proc(resolver: ^Resolver, package_file: ^Package_File, declaration: ^Declaration) -> ^Symbol {
  flags := symbol_get_flags_from_declaration(declaration);
  
  kind := Symbol_Kind.None;
  switch declaration.kind {
    case .Constant: kind = .Constant;
    case .Global: kind = .Global;
    case .Enumeration, .Struct, .Union, .Interface, .Type_Alias, .Type_Define: kind = .Type;
    case .Procedure, .Function: kind = .Routine;

    case .Import, .Implementation, .Directive: fallthrough;
    case .None: assert(false);
  }

  symbol := symbol_make(resolver, kind, declaration.access_modifier, package_file, declaration.name, declaration);
  symbol.flags = flags;

  return symbol;
}

/**
* Get symbol flags from a declaration.
* 
* @param declaration The declaration to get the flags for.
* @return The symbol flags.
*/
symbol_get_flags_from_declaration :: proc(declaration: ^Declaration) -> Symbol_Flags {
  flags := Symbol_Flags{};

  if .Generic in declaration.flags {
    flags += {.Uninstantiated_Generic};
  }
  if declaration.kind == .Function {
    flags += {.Routine_Function};
  }
  if .Routine_Is_Pure_Function in declaration.flags {
    flags += {.Routine_Function_Pure};
  }
  
  return flags;
}

/**
* Imports symbols from a package .
*
* @param resolver         The reference to the resolver.
* @param file             The package file to import the symbols into.
* @param position         The position of the import.
* @param imported_package The package whose symbols to import.
*/
resolver_import_package_symbols :: proc(resolver: ^Resolver, file: ^Package_File, position: Source_Position, imported_package: ^Package) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for _, imported_file in imported_package.files {
    for _, symbol in imported_file.symbols {
      if symbol.access_modifier == .Public {
        resolver_add_imported_symbol(resolver, position, file, symbol.name, symbol);
      }
    }
  }
}

/**
* Adds a global symbol.
*
* @param resolver The reference to the resolver.
* @param file     The package file the symbol belongs to.
* @param name     The name of the symbol.
* @param symbol   The symbol to add.
*/
resolver_add_global_symbol :: proc(resolver: ^Resolver, file: ^Package_File, name: string, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  compiler := cast(^Compiler) context.user_ptr;
  current_package := compiler.current_package;
  old_symbol := package_get_symbol_without_checks(current_package, file, name, .All);

  if old_symbol != nil {
    is_duplicate := false;
    if symbol.access_modifier == .Private || old_symbol.access_modifier == .Private {
      if old_symbol.package_file == file {
        is_duplicate = true;
      }
    } else {
      is_duplicate = symbol.package_file.pack == current_package;
    }

    if is_duplicate {
      position := symbol.declaration == nil ? SOURCE_POSITION_BUILTIN : symbol.declaration.position;
      old_declaration := old_symbol.declaration;
      if old_declaration == nil {
        report_error_fatal(position, "Duplicate definition of symbol '%v'", name);
      } else {
        old_position := old_declaration.position;
        report_error_fatal(
          position,
          "Duplicate definition of symbol '%v'\nPrevious definition at %v(%v:%v)",
          name,
          old_position.file,
          old_position.line,
          old_position.column,
        );
      }
    }
  }

  file.symbols[symbol.name] = symbol;
  resolver.unreachable_symbols[symbol] = {};
}

/**
* Adds a global package symbol.
*
* @param resolver         The reference to the resolver.
* @param imported_package The package that got imported.
* @param file             The package file the symbol belongs to.
* @param name             The name of the symbol.
* @param declaration      The import declaration.
*/
resolver_add_package_symbol :: proc(resolver: ^Resolver, imported_package: ^Package, file: ^Package_File, name: string, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  symbol := symbol_make(resolver, .Package, .Private, file, name, declaration);
  symbol.value = imported_package;
  symbol.state = .Resolved;
  symbol.flags += {.Reachable};

  // Check that we might have already imported a package with the same name in the same file.
  old_symbol := file.imported_symbols[name];
  if old_symbol != nil && old_symbol.kind == .Package {
    package_with_the_same_name := old_symbol.value.(^Package);
    report_error_fatal(
      declaration.position,
      "A package with the name '%v' was already imported through '%v'. Try using a package alias",
      name,
      package_with_the_same_name.import_path,
    );
  }

  resolver_add_imported_symbol(resolver, declaration.position, file, name, symbol);
}

/**
* Adds an imported symbol.
*
* @param resolver The reference to the resolver.
* @param position The position of the import.
* @param file     The package file the symbol belongs to.
* @param name     The name of the symbol.
* @param symbol   The imported symbol to add.
*/
resolver_add_imported_symbol :: proc(resolver: ^Resolver, position: Source_Position, file: ^Package_File, name: string, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  compiler := cast(^Compiler) context.user_ptr;
  current_package := compiler.current_package;
  old_symbol := package_get_symbol_without_checks(current_package, file, name, .All);

  if old_symbol != nil {
    report_error_fatal(position, "Conflicting import of symbol '%v' into '%v' from '%v'", name, current_package.name, symbol.package_file.pack.name);
  }

  file.imported_symbols[symbol.name] = symbol;
  resolver.unreachable_symbols[symbol] = {};
}

/**
* Adds a parameter symbol.
*
* @param resolver The reference to the resolver.
* @param position The position of the symbol.
* @param name     The name of the symbol.
* @param type     The type of the symbol.
* @param flags    The flags of the symbol.
*/
resolver_add_parameter_symbol :: proc(resolver: ^Resolver, position: Source_Position, name: string, type: ^Type, flags: Symbol_Flags = {}) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  compiler := cast(^Compiler) context.user_ptr;
  package_file := package_get_package_file_by_name(compiler.current_package, position.file);
  flags := flags;
  flags += {.Reachable};
  symbol := Symbol{.Parameter, .Resolved, flags, .Private, name, package_file, nil, type, nil};
  resolver.local_scope.symbols[name] = {symbol, position};
}

/**
* Adds a local symbol.
*
* @param resolver The reference to the resolver.
* @param position The position of the symbol.
* @param name     The name of the symbol.
* @param type     The type of the symbol.
* @param flags    The flags of the symbol.
* @param value    The value of the symbol.
*/
resolver_add_local_symbol :: proc(resolver: ^Resolver, position: Source_Position, name: string, type: ^Type, flags: Symbol_Flags = {}, value: Value = nil) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // Verify that the symbol was not already defined in the current block.
  local_symbol, found := resolver.local_scope.symbols[name];
  if found {
    report_error_fatal(position, "Duplicate definition of local '%v'", local_symbol.symbol.name);
  }

  compiler := cast(^Compiler) context.user_ptr;
  package_file := package_get_package_file_by_name(compiler.current_package, position.file);
  flags := flags;
  flags += {.Reachable};
  symbol := Symbol{.Local, .Resolved, flags, .Private, name, package_file, nil, type, value};
  resolver.local_scope.symbols[name] = {symbol, position};
}

/**
* Resolves a symbol.
* 
* @param resolver The reference to the resolver.
* @param symbol   The symbol to resolve.
*/
resolver_resolve_symbol :: proc(resolver: ^Resolver, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if symbol.state == .Resolved {
    // Everything that is resolved (including builtin types) should also be reachable.
    assert(.Reachable in symbol.flags);
    return;
  } else if symbol.state == .Resolving {
    report_error_fatal(symbol.declaration.position, "Cyclic dependency of symbol: '%v'", symbol.name);
    return;
  }

  assert(.Reachable not_in symbol.flags);
  symbol.flags += {.Reachable};
  if symbol.kind != .Local && symbol.kind != .Parameter {
    append(&resolver.reachable_symbols, symbol);
    delete_key(&resolver.unreachable_symbols, symbol);
  }

  declaration := symbol.declaration;

  // We add the symbol to the list of sorted symbols AFTER it got properly resolved depending on its kind.
  // This also needs to happen in case of 'Resolved_But_Not_Reachable'.
  defer if declaration != nil && declaration.kind != .Struct && declaration.kind != .Union {
    append(&resolver.sorted_symbols, symbol);
  }

  // Here we know we already resolved the symbol but did not add it to the reachable list.
  // This currently only happens for methods.
  if symbol.state == .Resolved_But_Not_Reachable {
    symbol.state = .Resolved;
    return;
  }

  assert(symbol.state == .Unresolved);
  symbol.state = .Resolving;

  previous_package := package_enter(symbol.package_file.pack);

  switch symbol.kind {
    case .Constant: symbol.type = resolver_resolve_symbol_constant(resolver, declaration, symbol);
    case .Global: symbol.type = resolver_resolve_symbol_global(resolver, declaration, symbol);
    case .Type: symbol.type = resolver_resolve_symbol_type(resolver, declaration, symbol);
    case .Routine: symbol.type = resolver_resolve_symbol_routine(resolver, declaration, symbol);

    case .Package: // We don't need to do anything.

    case .Local, .Parameter: fallthrough;
    case .None: assert(false);
  }
  
  package_leave(previous_package);

  symbol.state = .Resolved;
}

/**
* Resolves a constant symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The constant symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_constant :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(declaration.kind == .Constant);

  declaration_constant := cast(^Declaration_Constant) declaration;

  result := resolver_resolve_expression_constant(resolver, declaration_constant.expression, STATEMENT_CONTEXT);
  constant_type := type_unqualify(result.type);

  if !type_is_scalar(constant_type) && !type_is_string_like(constant_type) {
    report_error_fatal(declaration.position, "Constants must have a scalar or string type");
  }
  if declaration_constant.type != nil {
    type := resolver_resolve_type_specification(resolver, declaration_constant.type);
    if !operand_convert(resolver, declaration_constant.expression.position, &result, type) {
      report_error_fatal(declaration.position, "Invalid types in constant declaration. Expected '%v' got '%v'", type.name, result.type.name);
    }
  } else {
    operand_remove_untyped(resolver.storage, declaration.position, &result);
  }

  symbol.value = result.value;
  return type_storage_get_or_make_type_constant(resolver.storage, result.type);
}

/**
* Resolves a global symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The global symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_global :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(declaration.kind == .Global);

  declaration_global := cast(^Declaration_Global) declaration;

  if .Extern in declaration.flags && declaration_global.expression != nil {
    report_error_fatal(declaration.position, "Extern global cannot have an initializer expression");
  }

  return resovler_resolve_initializer(resolver, declaration.position, STATEMENT_CONTEXT, declaration_global.type, declaration_global.expression);
}

/**
* Resolves a type symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The type symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_type :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  #partial switch declaration.kind {
    case .Enumeration: return resolver_resolve_symbol_type_enumeration(resolver, declaration, symbol);

    case .Struct, .Union: {
      if .Uninstantiated_Generic in symbol.flags {
        generic_type_names := (cast(^Declaration_Struct) declaration).generic_type_names;
        resolver_check_for_duplicate_generic_type_names(declaration.position, generic_type_names);
      }
      return type_storage_make_type_incomplete(resolver.storage, declaration.kind == .Struct ? .Struct : .Union, symbol);
    }
    
    case .Interface: return resolver_resolve_symbol_type_interface(resolver, declaration, symbol);

    case .Type_Alias: {
      declaration_type_alias := cast(^Declaration_Type_Alias) declaration;
      generic_context: Generic_Context;
      generic_context.type_names = declaration_type_alias.generic_type_names;
      generic_context.generic_types = make_dynamic_array_len_cap([dynamic]^Type, 0, len(declaration_type_alias.generic_type_names), context.temp_allocator);
      for type_name in declaration_type_alias.generic_type_names {
        append(&generic_context.generic_types, type_storage_get_or_make_type_generic(resolver.storage, type_name));
      }
      aliased_type := resolver_resolve_type_specification(resolver, declaration_type_alias.type, generic_context);
      if type_is_array_and_incomplete(aliased_type) {
        report_error_fatal(declaration.position, "Type '%v' is not allowed in type alias", aliased_type.name);
      }
      return aliased_type;
    }
    case .Type_Define: {
      declaration_type_define := cast(^Declaration_Type_Define) declaration;
      base := resolver_resolve_type_specification(resolver, declaration_type_define.type);
      // Currently we disallow enumerations in type defines as that would require a bit more work to support.
      // If we would allow them right now, there would be no easy way to refer to the enum constants.
      if type_is_array_and_incomplete(base) || type_is_enumeration(base) {
        report_error_fatal(declaration.position, "Type '%v' is not allowed in type define", base.name);
      }
      return type_storage_make_type_distinct(resolver.storage, declaration.name, base);
    }
  }

  assert(false);
  return nil;
}

/**
* Resolves a type enumeration symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The type enumeration symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_type_enumeration :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration_enumeration := cast(^Declaration_Enumeration) declaration;

  base_type := resolver.storage.type_int;
  if declaration_enumeration.type != nil {
    base_type = resolver_resolve_type_specification(resolver, declaration_enumeration.type);
  }

  if !type_is_integer(base_type) {
    report_error_fatal(declaration.position, "Base type of an enumeration must be an integer type. Got '%v'", base_type.name);
  }

  is_flags := .Enum_Flags in symbol.flags;

  item_value := value_default(base_type);
  // Flags start at '1' and not '0', so we increment it accordingly.
  if is_flags {
    resolver_resolve_symbol_type_enumeration_increment_regular(&item_value, base_type);
  }

  // We are entering a local scope, so that we can refer to our own enum names.
  // NOTE: Currently we do NOT resolve those out-of-order meaning they have to be forward declared.
  resolver_enter_local_scope(resolver);

  enumeration_items: #soa [dynamic]Type_Enumeration_Item;
  for item in declaration_enumeration.items {
    if item.initializer != nil {
      if is_flags {
        report_error_fatal(item.initializer.position, "Enum item initializer not allowed for enum flags");
      }
      constant_operand := resolver_resolve_expression_constant(resolver, item.initializer, STATEMENT_CONTEXT);
      if !operand_convert(resolver, item.initializer.position, &constant_operand, base_type) {
        report_error_fatal(item.initializer.position, "Invalid type in enum item initializer. Expected '%v' got '%v'", base_type.name, constant_operand.type.name);
      }
      item_value = constant_operand.value;
    }

    // We add our item as a local constant by making the type constant and explicitly setting the value.
    constant_local_type := type_storage_get_or_make_type_constant(resolver.storage, base_type);
    resolver_add_local_symbol(resolver, item.position, item.name, constant_local_type, {}, item_value);

    append_soa(&enumeration_items, Type_Enumeration_Item{item.name, item_value});

    // Automatically increment the value for the next item.
    if is_flags {
      resolver_resolve_symbol_type_enumeration_increment_flags(&item_value, base_type);
    } else {
      resolver_resolve_symbol_type_enumeration_increment_regular(&item_value, base_type);
    }
  }

  resolver_leave_local_scope(resolver);

  return type_storage_make_type_enumeration(resolver.storage, symbol, base_type, enumeration_items, is_flags);
}

/**
* Regularly increment the value of an enum.
*
* @param The value to increment.
* @param The base type of the enum.
*/
resolver_resolve_symbol_type_enumeration_increment_regular :: proc(value: ^Value, base_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  #partial switch base_type.kind {
    case .I8:   value^ = value.(i8)   + 1;
    case .I16:  value^ = value.(i16)  + 1;
    case .I32:  value^ = value.(i32)  + 1;
    case .I64:  value^ = value.(i64)  + 1;
    case .Int:  value^ = value.(int)  + 1;
    case .U8:   value^ = value.(u8)   + 1;
    case .U16:  value^ = value.(u16)  + 1;
    case .U32:  value^ = value.(u32)  + 1;
    case .U64:  value^ = value.(u64)  + 1;
    case .UInt: value^ = value.(uint) + 1;
  }
}

/**
* Increment the value of an enum flag.
*
* @param The value to increment.
* @param The base type of the enum flag.
*/
resolver_resolve_symbol_type_enumeration_increment_flags :: proc(value: ^Value, base_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  #partial switch base_type.kind {
    case .I8:   value^ = value.(i8)   << 1;
    case .I16:  value^ = value.(i16)  << 1;
    case .I32:  value^ = value.(i32)  << 1;
    case .I64:  value^ = value.(i64)  << 1;
    case .Int:  value^ = value.(int)  << 1;
    case .U8:   value^ = value.(u8)   << 1;
    case .U16:  value^ = value.(u16)  << 1;
    case .U32:  value^ = value.(u32)  << 1;
    case .U64:  value^ = value.(u64)  << 1;
    case .UInt: value^ = value.(uint) << 1;
  }
}

/**
* Resolves a type interface symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The type interface symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_type_interface :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration_interface := cast(^Declaration_Interface) declaration;

  methods: #soa [dynamic]Type_Method; 
  for declaration_method in declaration_interface.methods {
    name := declaration_method.name;
    for method in methods {
      if name == method.name {
        report_error_fatal(declaration_method.position, "Duplicate interface method named '%v'", declaration_method.name);
      }
    }

    method_symbol := symbol_make_declaration(resolver, symbol.package_file, declaration_method);
    method_symbol.state = .Resolved;
    method_symbol.flags += {.Reachable, .Routine_Method_Interface};
    if (.Uninstantiated_Generic in method_symbol.flags) {
      report_error_fatal(declaration_method.position, "Interface method '%v' can not be generic", declaration_method.name);
    }

    method_type := resolver_resolve_symbol_routine(resolver, declaration_method, method_symbol);
    append_soa(&methods, Type_Method{declaration_method.access_modifier, symbol.package_file.pack, declaration_method.position.file, name, method_type});

    if method_symbol.access_modifier > declaration.access_modifier {
      report_error_fatal(declaration_method.position, "Method '%v' has a higher access modifier than '%v'", name, declaration.name);
    }
  }

  return type_storage_make_type_interface(resolver.storage, symbol, methods);
}

/**
* Resolves a routine symbol.
* 
* @param resolver    The reference to the resolver.
* @param declaration The declaration that defines the symbol.
* @param symbol      The routine symbol to resolve.
* @return The resolved type of the symbol.
*/
resolver_resolve_symbol_routine :: proc(resolver: ^Resolver, declaration: ^Declaration, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(declaration.kind == .Procedure || declaration.kind == .Function);

  declaration_routine := cast(^Declaration_Routine) declaration;

  calling_convention := declaration_routine.calling_convention;
  resolver_resolve_symbol_routine_attributes(resolver, symbol, declaration);
  is_implemented_method := .Routine_Method_Implemented in symbol.flags;
  is_method := is_implemented_method || .Routine_Method_Interface in symbol.flags;
  is_pure := .Routine_Function_Pure in symbol.flags;
  is_intrinsic := .Routine_Intrinsic in symbol.flags;

  type_kind := declaration.kind == .Procedure ? Type_Kind.Procedure : Type_Kind.Function;
  has_params := .Routine_Has_Params in declaration.flags;
  has_c_varargs := .Routine_Has_C_Varargs in declaration.flags;
  
  is_generic := .Uninstantiated_Generic in symbol.flags;
  is_instantiated := .Routine_Instantiated in symbol.flags;
  generic_context: Generic_Context;
  generic_context.generic_types = make_dynamic_array_len_cap([dynamic]^Type, 0, len(declaration_routine.generic_type_names), context.temp_allocator);
  if is_generic {
    resolver_check_for_duplicate_generic_type_names(declaration.position, declaration_routine.generic_type_names);
    generic_context.type_names = declaration_routine.generic_type_names;
    for type_name in declaration_routine.generic_type_names {
      append(&generic_context.generic_types, type_storage_get_or_make_type_generic(resolver.storage, type_name));
    }
  }

  parameters := make_soa_dynamic_array_len_cap(#soa [dynamic]Type_Routine_Parameter, 0, len(declaration_routine.parameters));
  parameter_names := make_dynamic_array_len_cap([dynamic]string, 0, len(declaration_routine.parameters), context.temp_allocator);
  for parameter in declaration_routine.parameters {
    parameter_type := resolver_resolve_type_specification(resolver, parameter.type, generic_context);
    if .Not_Allowed_As_Parameter_Or_Return_Type in parameter_type.flags {
      report_error_fatal(parameter.position, "Type '%v' is not allowed as parameter type", parameter_type.name);
    }
    if is_pure {
      if .Not_Allowed_As_Parameter_In_Pure_Function in parameter_type.flags {
        report_error_fatal(parameter.position, "Parameter '%v' of type '%v' is not allowed in a pure function", parameter.name, parameter_type.name);
      }
    }

    if !is_instantiated && parameter_type.symbol != nil {
      if parameter_type.symbol.access_modifier < declaration.access_modifier {
        report_error_fatal(
          parameter.type.position,
          "Parameter '%v' exposes type '%v' with a higher access modifier than '%v'",
          parameter.name,
          parameter_type.name,
          declaration.name,
        );
      }
    }

    is_default := parameter.initializer != nil;
    append_soa(&parameters, Type_Routine_Parameter{parameter_type, is_default});

    if is_implemented_method && parameter.name == SPECIAL_NAME_SELF {
      report_error_fatal(parameter.position, "Implemented method cannot have a paramter named 'self'");
    }
    if (calling_convention == .Nox && !is_pure) && parameter.name == SPECIAL_NAME_CONTEXT {
      report_error_fatal(parameter.position, "Routine with 'Nox' calling convention cannot have a paramter named 'context'");
    }

    for name in parameter_names {
      if name == parameter.name {
        report_error_fatal(parameter.position, "Duplicate paramter name '%v' in routine", name);
      }
    }

    if (parameter.name != SPECIAL_NAME_DISCARD) {
      append(&parameter_names, parameter.name);
    }
  }

  return_type := resolver.storage.type_void;
  if declaration_routine.return_type != nil {
    return_type = resolver_resolve_type_specification(resolver, declaration_routine.return_type, generic_context);
    if .Not_Allowed_As_Parameter_Or_Return_Type in return_type.flags {
      report_error_fatal(declaration.position, "Type '%v' is not allowed as return type", return_type.name);
    }
    if !is_instantiated && return_type.symbol != nil {
      if return_type.symbol.access_modifier < declaration.access_modifier {
        report_error_fatal(declaration.position, "Return type '%v' has a higher access modifier than '%v'", return_type.name, declaration.name);
      }
    }
  }
  if declaration.kind == .Function && type_is_void(return_type) {
    report_error_fatal(declaration.position, "Function '%v' has no return value", declaration.name);
  }

  if has_params {
    params_index := len(parameters) - 1;
    params_type := parameters[params_index].type;
    if !type_is_slice(params_type) {
      report_error_fatal(declaration_routine.parameters[params_index].type.position, "Parameter marked 'params' must be a slice type. Got '%v'", params_type.name);
    }
  }

  needs_block := !is_intrinsic && .Extern not_in declaration.flags && .Routine_Method_Interface not_in symbol.flags;
  if needs_block && .Routine_Has_Block not_in declaration.flags {
    report_error_fatal(declaration.position, "Routine '%v' is missing its body", declaration.name);
  } else if !needs_block && .Routine_Has_Block in declaration.flags {
    if .Extern in declaration.flags {
      report_error_fatal(declaration.position, "Routine '%v' can't have a body because its marked as 'extern'", declaration.name);  
    } else {
      report_error_fatal(declaration.position, "Interface method '%v' can't have a body", declaration.name);  
    }
  }

  routine_flags := type_make_routine_flags(has_params, has_c_varargs, is_intrinsic, is_method, is_pure);
  if is_generic {
    // For generic routines we will always create a new routine type.
    // That way we can associate the uninstantiated symbol with them.
    generic_routine_type := type_storage_make_type_routine(resolver.storage, type_kind, calling_convention, parameters, return_type, routine_flags);
    generic_routine_type.symbol = symbol; 
    return generic_routine_type;
  } else {
    resolver_enter_routine_scope(resolver, symbol, declaration, calling_convention, is_pure);
    statement_context := resolver_get_routine_statement_context(symbol, calling_convention);
    resolver_resolve_symbol_routine_default_parameters(resolver, declaration_routine, parameters, statement_context);
    resolver_leave_routine_scope(resolver);

    return type_storage_get_or_make_type_routine(resolver.storage, type_kind, calling_convention, parameters, return_type, routine_flags);
  }
}

/**
* Resolves a routines attributes.
* 
* @param resolver    The reference to the resolver.
* @param symbol      The routine symbol to resolve.
* @param declaration The declaration that defines the symbol.
*/
resolver_resolve_symbol_routine_attributes :: proc(resolver: ^Resolver, symbol: ^Symbol, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  disabled_attribute, has_disabled_attribute := resolver_try_get_attribute(declaration, .Disabled);
  if has_disabled_attribute {
    attribute_name := ATTRIBUTE_NAMES[Attribute_Kind.Disabled];

    if len(disabled_attribute.arguments) != 1 {
      report_error_fatal(
        disabled_attribute.position,
        "Invalid number of arguments for attribute '%v'. Expected 1 got %v",
        attribute_name,
        len(disabled_attribute.arguments),
      );
    }
    condition_expression := disabled_attribute.arguments[0].expression;
    disabled_operand := resolver_resolve_expression_constant(resolver, condition_expression, STATEMENT_CONTEXT);
    if !operand_convert(resolver, condition_expression.position, &disabled_operand, resolver.storage.type_bool) {
      report_error_fatal(condition_expression.position, "Expected boolean type in argument for attribute '%v'. Got %v", attribute_name, disabled_operand.type.name);
    }

    if (cast(^Declaration_Routine) declaration).return_type != nil {
      report_error_fatal(declaration.position, "The attribute '%v' is not allowed on routines that return a value", attribute_name);
    }

    is_disabled := disabled_operand.value.(bool);
    if is_disabled {
      symbol.flags += {.Routine_Disabled};
    }
  }
}

/**
* Finalize a symbol.
* 
* @param resolver The reference to the resolver.
* @param symbol   The symbol to finalize.
*/
resolver_finalize_symbol :: proc(resolver: ^Resolver, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  if symbol.kind == .Type {
    position := symbol.declaration == nil ? SOURCE_POSITION_BUILTIN : symbol.declaration.position;
    resolver_complete_type(resolver, position, symbol.type);
  } else if symbol.kind == .Routine {
    should_finalize_body := .Extern not_in symbol.declaration.flags && .Uninstantiated_Generic not_in symbol.flags && .Routine_Method_Interface not_in symbol.flags;
    if should_finalize_body {
      resolver_finalize_routine_body(resolver, symbol);
      free_all(context.temp_allocator);
    }
  }
}

/**
* Finalize the body of a routine symbol.
* 
* @param resolver The reference to the resolver.
* @param symbol   The routine symbol whose body to finalize.
*/
resolver_finalize_routine_body :: proc(resolver: ^Resolver, symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(symbol.kind == .Routine && symbol.state == .Resolved);
  
  resolver.current_routine = symbol;
  
  previous_package := package_enter(symbol.package_file.pack);

  declaration := symbol.declaration;
  declaration_routine := cast(^Declaration_Routine) declaration;
  type_routine := cast(^Type_Routine) symbol.type;

  assert(len(declaration_routine.parameters) == len(type_routine.parameters));

  for parameter, i in declaration_routine.parameters {
    parameter_type := type_routine.parameters[i].type;
    resolver_complete_type(resolver, parameter.position, parameter_type);
  }

  statement_context := resolver_get_routine_statement_context(symbol, type_routine.calling_convention);
  resolver_enter_routine_scope(resolver, symbol, declaration, type_routine.calling_convention, .Is_Pure_Function in type_routine.routine_flags);
  
  // We only need to resolve default parameters for generic routines.
  if .Routine_Instantiated in symbol.flags {
    resolver_resolve_symbol_routine_default_parameters(resolver, declaration_routine, type_routine.parameters, statement_context);
  }

  // We add the parameter symbols AFTER resolving the default parameters as we can't access them in a parameter initializer.
  for parameter, i in declaration_routine.parameters {
    parameter_type := type_routine.parameters[i].type;
    if (parameter.name != SPECIAL_NAME_DISCARD) {
      resolver_add_parameter_symbol(resolver, parameter.position, parameter.name, parameter_type, {.Local_Declared});
    }
  }

  return_type := type_routine.return_type;
  resolver_complete_type(resolver, declaration_routine.return_type == nil ? declaration.position : declaration_routine.return_type.position, return_type);

  returns := resolver_resolve_statement_block(resolver, declaration_routine.block, statement_context, return_type, .New_Scope);
  resolver_leave_routine_scope(resolver);

  // We need to check that our procedure properly returns on all code paths.
  if !type_is_void(return_type) && !returns {
    report_error_fatal(symbol.declaration.position, "Not all control paths return a value");
  }

  package_leave(previous_package);
}

/**
* Enters the scope of a routine.
* 
* @param resolver           The reference to the resolver.
* @param symbol             The routine symbol.
* @param declaration        The declaration that defines the routine.
* @param calling_convention The calling convention of the routine.
* @param is_pure            Is the routine a pure function?
*/
resolver_enter_routine_scope :: proc(resolver: ^Resolver, symbol: ^Symbol, declaration: ^Declaration, calling_convention: Calling_Convention, is_pure: bool) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_enter_local_scope(resolver);
  // We may have an implicit context pointer.
  if calling_convention == .Nox && !is_pure {
    context_type := type_storage_get_or_make_type_pointer(resolver.storage, resolver.storage.cached_runtime_types.context_struct);
    resolver_add_parameter_symbol(resolver, declaration.position, SPECIAL_NAME_CONTEXT, context_type, {.Local_Immutable});
  }
  // Methods get a special pointer parameter 'self' that gets implicitly passed in.
  if .Routine_Method_Implemented in symbol.flags {
    self_type := type_storage_get_or_make_type_pointer(resolver.storage, symbol.value.(^Type));
    resolver_add_parameter_symbol(resolver, declaration.position, SPECIAL_NAME_SELF, self_type);
  }
}

/**
* Leavs the scope of a routine.
* 
* @param resolver The reference to the resolver.
*/
resolver_leave_routine_scope :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  resolver_leave_local_scope(resolver);
}

/**
* Gets the statement context for a routine.
* 
* @param symbol             The symbol of the routine.
* @param calling_convention The calling convention of the routine.
* @return The statement context.
*/
resolver_get_routine_statement_context :: proc(symbol: ^Symbol, calling_convention: Calling_Convention) -> Statement_Context {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  statement_context := STATEMENT_CONTEXT;
  if .Routine_Function in symbol.flags || .Routine_Function_Pure in symbol.flags {
    statement_context.statement_restriction = .Allow_Only_Pure;
  }
  if calling_convention != .Nox {
    statement_context.legal_statements -= {.Can_Call_Routine_With_Nox_Calling_Convention};
  }
  return statement_context;
}

/**
* Resolves the default parameters of a routine.
* 
* @param resolver            The reference to the resolver.
* @param declaration_routine The routine declaration.
* @param parameters          The parameters of the routine.
* @param statement_context   The statement context to use.
*/
resolver_resolve_symbol_routine_default_parameters :: proc(
  resolver: ^Resolver,
  declaration_routine: ^Declaration_Routine,
  parameters: #soa[dynamic]Type_Routine_Parameter,
  statement_context: Statement_Context,
) {
  for parameter, i in declaration_routine.parameters {
    parameter_type := parameters[i].type;
    if parameter.initializer != nil {
      initializer := resolver_resolve_expression_expected(resolver, parameter.initializer, statement_context, parameter_type);
      if !operand_convert(resolver, parameter.position, &initializer, parameter_type) {
        report_error_fatal(parameter.position, "Invalid type in default parameter initializer. Expected '%v' got '%v'", parameter_type.name, initializer.type.name);
      }

      if .Is_Constant in initializer.flags {
        resolver_set_resolved_constant(resolver, parameter.initializer, initializer);
      }
    }
  }
}
