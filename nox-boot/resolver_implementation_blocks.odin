package nox

import "core:strings"
import "tracy"

/**
* Represents an unresolved implementation block.
*/
Unresolved_Implementation :: struct {
  package_file: ^Package_File, // The package file the implementation block is declared in.
  declaration: ^Declaration,   // The declaration of the implementation block.
}

/**
* Represent a resolved implementation block key.
*/
Resolved_Implementation_Key :: struct {
  interface_type: ^Type,      // The interface that gets implemented (can be empty for simple implementation blocks).
  implementation_type: ^Type, // The type of the implementation.
}

/**
* Represents a resolved implementation block.
*/
Resolved_Implementation :: struct {
  symbols: map[string]^Symbol, // The symbols of the resolved implementation block.
}

/**
* Represents a method to implement for interface implementation blocks.
*/
Method_To_Implement :: struct {
  access_modifier: Access_Modifier, // The access modifier of the method.
  type: ^Type,                      // The type of the method.
  is_implemented: bool,             // Whether or not the method is implemented.
}

/**
* Adds a global implementation block.
*
* @param resolver     The reference to the resolver.
* @param package_file The package file the implementation block belongs to.
* @param declaration  The declaration of the implementation block.
*/
resolver_add_global_implementation :: proc(resolver: ^Resolver, package_file: ^Package_File, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  assert(declaration.kind == .Implementation);

  append_soa(&resolver.unresolved_implementations, Unresolved_Implementation{package_file, declaration});
}

/**
* Resolves all implementation blocks.
*
* @param resolver The reference to the resolver.
*/
resolver_resolve_implementations :: proc(resolver: ^Resolver) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  for implementation in resolver.unresolved_implementations {
    previous_package := package_enter(implementation.package_file.pack);
    resolver_resolve_implementation(resolver, implementation);
    package_leave(previous_package);
  }
}

/**
* Resolves an implementation block.
*
* @param resolver       The reference to the resolver.
* @param implementation The implementation block to resolve.
*/
resolver_resolve_implementation :: proc(resolver: ^Resolver, implementation: Unresolved_Implementation) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration := implementation.declaration;
  declaration_implementation := cast(^Declaration_Implementation) declaration;

  implementation_type := resolver_resolve_type_specification(resolver, declaration_implementation.implementation_type);
  if type_is_interface(implementation_type) {
    report_error_fatal(declaration_implementation.implementation_type.position, "Implementation block cannot implement the interface '%v'", implementation_type.name);
  }
  // The resolving marks the type as reachable but it actually is not as we are just setting up the implementations.
  if (implementation_type.symbol != nil) {
    implementation_type.symbol.state = .Resolved_But_Not_Reachable;
    implementation_type.symbol.flags -= {.Reachable};
  }

  if declaration_implementation.interface_type == nil {
    resolver_resolve_implementation_regular(resolver, implementation, implementation_type);
  } else {
    resolver_resolve_implementation_interface(resolver, implementation, implementation_type);
  }
}

/**
* Resolves a regular implementation block.
*
* @param resolver            The reference to the resolver.
* @param implementation      The implementation block to resolve.
* @param implementation_type The type that the implementation block refers to.
*/
resolver_resolve_implementation_regular :: proc(resolver: ^Resolver, implementation: Unresolved_Implementation, implementation_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration := implementation.declaration;
  declaration_implementation := cast(^Declaration_Implementation) declaration;

  symbols: map[string]^Symbol;

  key := Resolved_Implementation_Key{nil, implementation_type};
  implementations, _ := resolver.output.resolved_implementations[key];

  package_file := implementation.package_file;
  for declaration_method in declaration_implementation.methods {
    name := declaration_method.name;

    resolver_resolve_implementation_check_for_method_name_clash(declaration_method.position, implementation_type, name);

    symbol_name := resolver_make_formatted_symbol_name(resolver, "%v.%v", implementation_type.name, name);
    method_symbol := resolver_resolve_implementation_method_symbol(resolver, declaration_method, implementation_type, package_file, symbol_name);

    if implementation_type.symbol != nil {
      if method_symbol.access_modifier > implementation_type.symbol.access_modifier  {
        report_error_fatal(declaration_method.position, "Method '%v' has a higher access modifier than '%v'", declaration_method.name, implementation_type.name);
      }
    }

    symbols[name] = method_symbol;
    append_soa(
      &implementation_type.methods,
      Type_Method{declaration_method.access_modifier, package_file.pack, declaration_method.position.file, name, method_symbol.type},
    );
  }

  append(&implementations, Resolved_Implementation{symbols});
  resolver.output.resolved_implementations[key] = implementations;
}

/**
* Resolves an interface implementation block.
*
* @param resolver            The reference to the resolver.
* @param implementation      The implementation block to resolve.
* @param implementation_type The type that the implementation block refers to.
*/
resolver_resolve_implementation_interface :: proc(resolver: ^Resolver, implementation: Unresolved_Implementation, implementation_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  declaration := implementation.declaration;
  declaration_implementation := cast(^Declaration_Implementation) declaration;

  interface_type := resolver_resolve_type_specification(resolver, declaration_implementation.interface_type);
  if !type_is_interface(interface_type) {
    report_error_fatal(declaration_implementation.interface_type.position, "Implementation block needs an interface type to implement. Got '%v'", interface_type.name);
  }
  // The resolving marks the type as reachable but it actually is not as we are just setting up the implementations.
  if (interface_type.symbol != nil) {
    interface_type.symbol.state = .Resolved_But_Not_Reachable;
    interface_type.symbol.flags -= {.Reachable};
  }

  for implemented_interface in implementation_type.interfaces {
    if implemented_interface == interface_type {
      report_error_fatal(declaration.position, "Interface '%v' is already implemented for type '%v'", interface_type.name, implementation_type.name);
    }
  }

  methods_to_implement := make_map(map[string]Method_To_Implement, len(interface_type.methods), context.temp_allocator);
  for interface_method in interface_type.methods {
    methods_to_implement[interface_method.name] = Method_To_Implement{interface_method.access_modifier, interface_method.type, false};
  }
  
  package_file := implementation.package_file;
  symbols: map[string]^Symbol;
  for declaration_method in declaration_implementation.methods {
    name := declaration_method.name;

    method_to_implement, found_matching_interface_method := methods_to_implement[name];
    if !found_matching_interface_method {
      report_error_fatal(declaration_method.position, "Interface '%v' does not define a method with the name '%v'", interface_type.name, name);
    }

    resolver_resolve_implementation_check_for_method_name_clash(declaration_method.position, implementation_type, name);

    symbol_name := resolver_make_formatted_symbol_name(resolver, "%v.%v.%v", interface_type.name, implementation_type.name, name);
    method_symbol := resolver_resolve_implementation_method_symbol(resolver, declaration_method, implementation_type, package_file, symbol_name);
    if method_symbol.type != method_to_implement.type {
      report_error_fatal(
        declaration_method.position,
        "Signature of method '%v' does not match with interface definition. Expected '%v' got '%v'",
        name,
        method_to_implement.type.name,
        method_symbol.type.name,
      );
    }
    
    if method_symbol.access_modifier != method_to_implement.access_modifier {
      report_error_fatal(
        declaration_method.position,
        "Mismatch in access modifier for implemented interface method '%v'. Expected '%v' got '%v'",
        name,
        method_to_implement.access_modifier,
        method_symbol.access_modifier,
      );
    }

    method_to_implement.is_implemented = true;
    methods_to_implement[name] = method_to_implement;
    
    symbols[name] = method_symbol;
  }

  has_missing_methods := false;
  missing_methods_builder := strings.builder_make(context.temp_allocator);
  for method_name, method_to_implement in methods_to_implement {
    if !method_to_implement.is_implemented {
      if has_missing_methods {
        strings.write_string(&missing_methods_builder, "\n");
      }
      has_missing_methods = true;

      strings.write_string(&missing_methods_builder, "    ");
      type_write_routine_name(&missing_methods_builder, method_to_implement.type, method_name);
    }
  }
  if has_missing_methods {
    missing_methods := strings.to_string(missing_methods_builder);
    report_error_fatal(declaration.position, "Missing interface methods in implementation:\n%v", missing_methods);
  }

  key := Resolved_Implementation_Key{interface_type, implementation_type};
  implementations, _ := resolver.output.resolved_implementations[key];
  append(&implementations, Resolved_Implementation{symbols});
  resolver.output.resolved_implementations[key] = implementations;

  types_implementing_interface := resolver.types_implementing_interface[interface_type];
  append(&types_implementing_interface, implementation_type);
  resolver.types_implementing_interface[interface_type] = types_implementing_interface;

  append(&implementation_type.interfaces, interface_type);
}

/**
* Resolves an interface implementation block.
*
* @param resolver            The reference to the resolver.
* @param declaration_method  The declaration of the method.
* @param implementation_type The type that the method belongs to.
* @param package_file        The package file the method symbol belongs to.
* @param symbol_name         The name of the method symbol.
* @return The resolved method symbol.
*/
resolver_resolve_implementation_method_symbol :: proc(
  resolver: ^Resolver,
  declaration_method: ^Declaration,
  implementation_type: ^Type,
  package_file: ^Package_File,
  symbol_name: string,
) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  // Methods are special in that they are sort of resolved manually and not part of the general recursive sorting alogrithm.

  method_symbol := symbol_make_declaration(resolver, package_file, declaration_method);
  method_symbol.flags += {.Routine_Method_Implemented};
  method_symbol.name = symbol_name; // We overwrite the name manually.
  method_symbol.value = implementation_type; // We associate the implemented type with the symbol.
  // We resolve the type of the routine here directly.
  method_symbol.type = resolver_resolve_symbol_routine(resolver, declaration_method, method_symbol);
  method_symbol.state = .Resolved_But_Not_Reachable; // This is the special state for methods that denotes that we resolved the type but are not yet reachable.

  // We have to manually add the method symobl to the unreachable symbols list.
  resolver.unreachable_symbols[method_symbol] = {};

  return method_symbol;
}

/**
* Resolves an interface implementation block.
*
* @param position            The position of the method.
* @param implementation_type The type whose methods to check.
* @param name                The name of the method.
*/
resolver_resolve_implementation_check_for_method_name_clash :: proc(position: Source_Position, implementation_type: ^Type, name: string) {
  tracy.ZoneC(ZONE_COLOR_RESOLVER);

  type_name := implementation_type.name;
  resolver_check_for_method_name_clash(position, implementation_type.methods, name, type_name);
  for interface in implementation_type.interfaces {
    resolver_check_for_method_name_clash(position, interface.methods, name, type_name);
  }
}
