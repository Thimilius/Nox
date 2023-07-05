package nox

import "core:runtime"
import "tracy"

/**
* Holds data needed for parsing.
*/
Parser :: struct {
  lexer: ^Lexer,                              // Reference to the lexer.
  allocator: runtime.Allocator,               // The allocator to use when allocating AST elements.

  declarations: [dynamic]^Declaration,        // Holds all parsed declarations.
  import_declarations: [dynamic]^Declaration, // Holds import declarations at the top scope (Not those nested inside a static if for example).
}

/**
* Makes a new parser.
*
* @param lexer     The lexer to use..
* @param allocator The allocator to use when allocating AST elements.
* @return The new parser.
*/
parser_make :: proc(lexer: ^Lexer, allocator: runtime.Allocator) -> Parser {
  p: Parser;
  p.lexer = lexer;
  p.allocator = allocator;
  p.declarations.allocator = allocator;
  p.import_declarations.allocator = allocator;
  return p;
}

/**
* Parses all top scope declarations.
*
* @param parser The reference to the parser.
*/
parser_parse_declarations :: proc(parser: ^Parser) {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  context.allocator = parser.allocator;

  lexer_get_next_token(parser.lexer);
  for !lexer_is_token(parser.lexer, .Eof) {
    declaration := parser_parse_declaration(parser);

    if declaration.kind == .Import {
      append(&parser.import_declarations, declaration);
    }
    append(&parser.declarations, declaration);
  }
}

/**
* Parses a single declaration.
*
* @param parser The reference to the parser.
* @return The parsed declaration.
*/
parser_parse_declaration :: proc(parser: ^Parser) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  attributes := parser_parse_attributes(parser);

  declaration := parser_parse_declaration_optional(parser);
  if declaration == nil {
    report_error_fatal(parser, "Expected declaration keyword. Got '%v'", lexer_token_info(parser.lexer));
  }

  if len(attributes.attributes) > 0 {
    if declaration.kind == .Import {
      report_error_fatal(parser, "Import declaration can't have any attributes");
    } else if declaration.kind == .Implementation {
      report_error_fatal(parser, "Implementation block can't have any attributes");
    }
  }
  declaration.attributes = attributes;
  
  return declaration;
}

/**
* Parses a set of attributes.
*
* @param parser The reference to the parser.
* @return The parsed attribute set.
*/
parser_parse_attributes :: proc(parser: ^Parser) -> Attributes {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  attributes: Attributes;
  for lexer_match_token(parser.lexer, .At) {
    append_soa(&attributes.attributes, parser_parse_attribute(parser));
  }
  return attributes;
}

/**
* Parses a single attribute.
*
* @param parser The reference to the parser.
* @return The parsed attribute.
*/
parser_parse_attribute :: proc(parser: ^Parser) -> Attribute {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  kind := parser_parse_attribute_kind(parser);

  arguments: #soa [dynamic]Attribute_Argument;
  if lexer_match_token(parser.lexer, .Left_Parentheses) {
    for !lexer_is_token(parser.lexer, .Right_Parentheses) {
      append_soa(&arguments, parser_parse_attribute_argument(parser));
      if !lexer_match_token(parser.lexer, .Comma) {
        break;
      }
    }
    lexer_expect_token(parser.lexer, .Right_Parentheses);
  }

  return {kind, position, arguments};
}

/**
* Parses a single attribute argument.
*
* @param parser The reference to the parser.
* @return The parsed attribute argument.
*/
parser_parse_attribute_argument :: proc(parser: ^Parser) -> Attribute_Argument {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;

  expression := parser_parse_expression(parser);

  name: string;
  if lexer_match_token(parser.lexer, .Assign) {
    if expression.kind != .Name {
      report_error_fatal(parser, "Left operand of '=' in attribute must be a name");
    }

    name = (cast(^Expression_Name) expression).name;
    // No destroying of 'expression' necessary here because we have a dedicated AST allocator.
    expression = parser_parse_expression(parser);
  }

  return {position, name, expression};
}

/**
* Parses an optional declaration.
*
* @param parser The reference to the parser.
* @return The parsed declaration or null if none could be parsed.
*/
parser_parse_declaration_optional :: proc(parser: ^Parser) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  if lexer_match_token(parser.lexer, .Keyword_Public) {
    return parser_parse_declaration_base(parser, .Public);
  } else if lexer_match_token(parser.lexer, .Keyword_Internal) {
    return parser_parse_declaration_base(parser, .Internal);
  } else if lexer_match_token(parser.lexer, .Keyword_Private) {
    return parser_parse_declaration_base(parser, .Private);
  } else {
    position := parser.lexer.token.position;

    if lexer_match_token(parser.lexer, .Keyword_Import) {
      return parser_parse_declaration_import(parser, position);
    } else if lexer_match_token(parser.lexer, .Keyword_Implement) {
      return parser_parse_declaration_implementation(parser, position);
    } else if lexer_match_token(parser.lexer, .Pound) {
      return parser_parse_declaration_directive(parser, position);
    } else {
      return parser_parse_declaration_base(parser, .Declaration_Default);
    }
  }
}

/**
* Parses a base declaration.
*
* @param parser          The reference to the parser.
* @param access_modifier The access modifier of the declaration.
* @return The parsed base declaration.
*/
parser_parse_declaration_base :: proc(parser: ^Parser, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;

  if lexer_match_token(parser.lexer, .Keyword_Const) {
    return parser_parse_declaration_constant(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Global) {
    return parser_parse_declaration_global(parser, position, access_modifier, {});
  }  else if lexer_match_token(parser.lexer, .Keyword_Enum) {
    return parser_parse_declaration_enumeration(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Struct) {
    return parser_parse_declaration_struct(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Union) {
    return parser_parse_declaration_union(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Proc) {
    return parser_parse_declaration_routine(parser, position, access_modifier, .Procedure, {});
  } else if lexer_match_token(parser.lexer, .Keyword_Func) {
    return parser_parse_declaration_routine(parser, position, access_modifier, .Function, {});
  } else if lexer_match_token(parser.lexer, .Keyword_Pure) {
    lexer_expect_token(parser.lexer, .Keyword_Func);
    return parser_parse_declaration_routine(parser, position, access_modifier, .Function, {.Routine_Is_Pure_Function});
  } else if lexer_match_token(parser.lexer, .Keyword_Extern) {
    return parser_parse_declaration_extern(parser, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Interface) {
    return parser_parse_declaration_interface(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Type_Alias) {
    return parser_parse_declaration_type_alias(parser, position, access_modifier);
  } else if lexer_match_token(parser.lexer, .Keyword_Type_Define) {
    return parser_parse_declaration_type_define(parser, position, access_modifier);
  } else {
    return nil;
  }
}

/**
* Parses an import declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed import declaration.
*/
parser_parse_declaration_import :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  alias: string;
  import_in_scope := false;
  flags: Declaration_Flags;
  if lexer_is_token(parser.lexer, .Name) {
    alias = parser_parse_name(parser);
  } else if lexer_is_token(parser.lexer, .Multiply) {
    import_in_scope = true;
    lexer_get_next_token(parser.lexer);
  } else if lexer_match_token(parser.lexer, .Keyword_Extern) {
    flags += {.Extern};
  }

  name, is_string := parser.lexer.token.value.(string);
  lexer_expect_token(parser.lexer, .String);

  return ast_declaration_make_import(position, name, flags, alias, import_in_scope);
}

/**
* Parses a constant declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed constant declaration.
*/
parser_parse_declaration_constant :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);
  type: ^Type_Specification = nil;
  if lexer_match_token(parser.lexer, .Colon) {
    type = parser_parse_type(parser);
    lexer_expect_token(parser.lexer, .Assign);
  } else {
    lexer_expect_token(parser.lexer, .Colon_Assign);
  }
  expression := parser_parse_expression(parser);
  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_declaration_make_constant(position, access_modifier, name, type, expression);
}

/**
* Parses a global declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @param flags           The flags of the global declaration.
* @return The parsed global declaration.
*/
parser_parse_declaration_global :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier, flags: Declaration_Flags) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);
  if lexer_match_token(parser.lexer, .Colon_Assign) {
    expression := parser_parse_expression(parser);
    lexer_expect_token(parser.lexer, .Semicolon);
    return ast_declaration_make_global(position, access_modifier, name, flags, nil, expression);
  } else if lexer_match_token(parser.lexer, .Colon) {
    type := parser_parse_type(parser);
    expression: ^Expression = nil;
    if lexer_match_token(parser.lexer, .Assign) {
      expression = parser_parse_expression(parser);
    }
    lexer_expect_token(parser.lexer, .Semicolon);
    return ast_declaration_make_global(position, access_modifier, name, flags, type, expression);
  } else {
    report_error_fatal(parser, "Expected ':' or ':=' after global. Got '%v'", lexer_token_info(parser.lexer));
    return nil;
  }
}

/**
* Parses an enumeration declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed enumeration declaration.
*/
parser_parse_declaration_enumeration :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);

  type: ^Type_Specification = nil;
  if lexer_match_token(parser.lexer, .Colon) {
    type = parser_parse_type(parser);
  }

  lexer_expect_token(parser.lexer, .Left_Brace);
  items: #soa [dynamic]Declaration_Enumeration_Item;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append_soa(&items, parser_parse_declaration_enumeration_item(parser));
    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  return ast_declaration_make_enumeration(position, access_modifier, name, type, items);
}

/**
* Parses an enumeration declaration item.
*
* @param parser The reference to the parser.
* @return The parsed enumeration declaration item.
*/
parser_parse_declaration_enumeration_item :: proc(parser: ^Parser) -> Declaration_Enumeration_Item {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  name := parser_parse_name(parser);
  initializer: ^Expression = nil;
  if lexer_match_token(parser.lexer, .Assign) {
    initializer = parser_parse_expression(parser);
  }
  return {position, name, initializer}
}

/**
* Parses a struct declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed struct declaration.
*/
parser_parse_declaration_struct :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);
  
  name := parser_parse_name(parser);
  generic_type_names, has_generic_type_names := parser_parse_declaration_generic_type_names(parser, position);
  flags: Declaration_Flags = has_generic_type_names ? {.Generic} : {};

  lexer_expect_token(parser.lexer, .Left_Brace);
  fields: #soa [dynamic]Declaration_Struct_Field;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append_soa(&fields, parser_parse_declaration_struct_item(parser));
    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  return ast_declaration_make_struct(position, access_modifier, name, flags, fields, generic_type_names);
}

/**
* Parses a struct declaration field.
*
* @param parser The reference to the parser.
* @return The parsed struct declaration field.
*/
parser_parse_declaration_struct_item :: proc(parser: ^Parser) -> Declaration_Struct_Field {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  access_modifier := parser_parse_access_modifier(parser, .Member_Unspecified);
  is_composite := lexer_match_token(parser.lexer, .Keyword_Composite);
  name := parser_parse_name(parser);
  lexer_expect_token(parser.lexer, .Colon);
  type := parser_parse_type(parser);

  return {position, access_modifier, is_composite, name, type};
}

/**
* Parses a union declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed union declaration.
*/
parser_parse_declaration_union :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);

  lexer_expect_token(parser.lexer, .Left_Brace);
  types: [dynamic]^Type_Specification;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append(&types, parser_parse_type(parser));
    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  return ast_declaration_make_union(position, access_modifier, name, types); 
} 

/**
* Parses a routine declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @param kind            The kind of the routine declaration.
* @param flags           The flags of the routine declaration.
* @return The parsed routine declaration.
*/
parser_parse_declaration_routine :: proc(
  parser: ^Parser,
  position: Source_Position,
  access_modifier: Access_Modifier,
  kind: Declaration_Kind,
  flags: Declaration_Flags,
) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  assert(kind == .Procedure || kind == .Function);

  flags := flags;

  calling_convention := parser_parse_calling_convention(parser, .Extern in flags ? .C : .Nox);
  name := parser_parse_name(parser);
  generic_type_names, has_generic_type_names := parser_parse_declaration_generic_type_names(parser, position);
  flags += has_generic_type_names ? {.Generic} : {};

  parameters: #soa [dynamic]Declaration_Routine_Parameter;
  has_params := false;
  has_c_varargs := false;
  last_was_default_parameter := false;

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  for !lexer_is_token(parser.lexer, .Right_Parentheses) {
    if has_params {
      report_error_fatal(parser, "Parameter marked with 'params' has to be the last parameter in a routine");
    }
    if has_c_varargs {
      report_error_fatal(parser, "C varargs have to be the last parameter in a routine");
    }
    
    parameter, parameter_is_params, parameter_is_c_varargs := parser_parse_declaration_routine_parameter(parser);
    if last_was_default_parameter && parameter.initializer == nil {
      report_error_fatal(parameter.position, "Non-default routine parameters cannot be declared after default parameters");
    }
    last_was_default_parameter = parameter.initializer != nil;

    has_params = parameter_is_params;
    has_c_varargs = parameter_is_c_varargs;
    if !parameter_is_c_varargs {
      append_soa(&parameters, parameter);
    }

    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Parentheses);

  if has_params {
    flags += {.Routine_Has_Params};
  } else if has_c_varargs {
    flags += {.Routine_Has_C_Varargs};
  }

  return_type: ^Type_Specification = nil;
  if lexer_match_token(parser.lexer, .Arrow) {
    return_type = parser_parse_type(parser);
  }

  generic_constraints: #soa [dynamic]Declaration_Routine_Generic_Constraint;
  if has_generic_type_names {
    for lexer_match_token(parser.lexer, .Keyword_Where) {
      position := parser.lexer.token.position;

      constraint_kind: Declaration_Routine_Generic_Constraint_Kind;
      constraint_value: Declaration_Routine_Generic_Constraint_Value;
      if lexer_match_token(parser.lexer, .Left_Parentheses) {
        expression := parser_parse_expression(parser);
        lexer_expect_token(parser.lexer, .Right_Parentheses);

        constraint_kind = .Expression;
        constraint_value = Declaration_Routine_Generic_Constraint_Expression{expression};
      } else {
        generic_type_name := parser_parse_name(parser);
        lexer_expect_token(parser.lexer, .Colon);
        type := parser_parse_type_name(parser, parser.lexer.token.position);

        constraint_kind = .Interface;
        constraint_value = Declaration_Routine_Generic_Constraint_Interface{generic_type_name, type};
      }
      
      append_soa(&generic_constraints, Declaration_Routine_Generic_Constraint{position, constraint_kind, constraint_value});
    }
  } else if lexer_is_token(parser.lexer, .Keyword_Where) {
    report_error_fatal(parser, "Routine with no generic type parameters can not have generic constraints");
  }

  block: Statement_Block;
  if lexer_is_token(parser.lexer, .Left_Brace) {
    block = parser_parse_statement_block(parser);
    flags += {.Routine_Has_Block};
  }

  return ast_declaration_make_routine(
    kind,
    position,
    access_modifier,
    name,
    flags,
    calling_convention,
    generic_type_names,
    parameters,
    return_type,
    generic_constraints,
    block,
  );
}

/**
* Parses a routine declaration parameter.
*
* @param parser The reference to the parser.
* @return 1. The parsed routine declaration parameter; 2. True if the parameter is params otherwise false; 3. True if the parameter is c varargs otherwise false.
*/
parser_parse_declaration_routine_parameter :: proc(parser: ^Parser) -> (Declaration_Routine_Parameter, bool, bool) {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  name: string;
  type: ^Type_Specification;
  is_params := false;
  is_c_varargs := false;
  initializer: ^Expression;

  if lexer_match_token(parser.lexer, .Ellipsis) {
    is_c_varargs = true;
  } else {
    if lexer_match_token(parser.lexer, .Keyword_Params) {
      is_params = true;
    }

    name = parser_parse_name(parser);
    lexer_expect_token(parser.lexer, .Colon);
    type = parser_parse_type(parser);

    // We don't allow default initializer for params as that is implicit.
    if !is_params && lexer_match_token(parser.lexer, .Assign) {
      initializer = parser_parse_expression(parser);
    }
  }

  return {position, name, type, initializer}, is_params, is_c_varargs;
}

/**
* Parses an interface declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed interface declaration.
*/
parser_parse_declaration_interface :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);
  methods := parser_parse_declaration_method_block(parser);

  return ast_declaration_make_interface(position, access_modifier, name, methods);
}

/**
* Parses an implementation declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed implementation declaration.
*/
parser_parse_declaration_implementation :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  interface_type: ^Type_Specification;
  implementation_type := parser_parse_type(parser);
  if lexer_match_token(parser.lexer, .Keyword_For) {
    interface_type = implementation_type;
    implementation_type = parser_parse_type(parser);
  }
  methods := parser_parse_declaration_method_block(parser);

  return ast_declaration_make_implementation(position, interface_type, implementation_type, methods);
}

/**
* Parses method declarations.
*
* @param parser The reference to the parser.
* @return The parsed method declarations.
*/
parser_parse_declaration_method_block :: proc(parser: ^Parser) -> [dynamic]^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Brace);
  methods: [dynamic]^Declaration;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    attributes := parser_parse_attributes(parser);

    access_modifier := parser_parse_access_modifier(parser, .Declaration_Default);

    position := parser.lexer.token.position;
    if lexer_is_token(parser.lexer, .Keyword_Proc) || lexer_is_token(parser.lexer, .Keyword_Func) {
      is_procedure := lexer_is_token(parser.lexer, .Keyword_Proc);
      lexer_get_next_token(parser.lexer);

      method := parser_parse_declaration_routine(parser, position, access_modifier, is_procedure ? .Procedure : .Function, {});
      method.attributes = attributes;
      append(&methods, method);
    } else {
      if lexer_is_token(parser.lexer, .Keyword_Pure) {
        report_error_fatal(parser, "Pure functions are not allowed as methods");
      } else {
        report_error_fatal(parser, "Expected 'proc' or 'func' declaration. Got '%v'", lexer_token_info(parser.lexer));
      }
    }
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  return methods;
}

/**
* Parses a type alias declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed type alias declaration.
*/
parser_parse_declaration_type_alias :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);
  generic_type_names, has_generic_type_names := parser_parse_declaration_generic_type_names(parser, position);
  flags: Declaration_Flags = has_generic_type_names ? {.Generic} : {};

  lexer_expect_token(parser.lexer, .Colon_Assign);
  type := parser_parse_type(parser);
  lexer_expect_token(parser.lexer, .Semicolon);

  return ast_declaration_make_type_alias(position, access_modifier, name, flags, type, generic_type_names);
}

/**
* Parses a type define declaration.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed type define declaration.
*/
parser_parse_declaration_type_define :: proc(parser: ^Parser, position: Source_Position, access_modifier: Access_Modifier) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);
  lexer_expect_token(parser.lexer, .Colon_Assign);
  type := parser_parse_type(parser);
  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_declaration_make_type_define(position, access_modifier, name, type);
}

/**
* Parses a declaration marked extern.
*
* @param parser          The reference to the parser.
* @param position        The source position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @return The parsed declaration marked extern.
*/
parser_parse_declaration_extern :: proc(parser: ^Parser, access_modifier: Access_Modifier) -> ^Declaration {
  position := parser.lexer.token.position;
  if lexer_match_token(parser.lexer, .Keyword_Global) {
    return parser_parse_declaration_global(parser, position, access_modifier, {.Extern});
  } else if lexer_match_token(parser.lexer, .Keyword_Proc) {
    return parser_parse_declaration_routine(parser, position, access_modifier, .Procedure, {.Extern});
  } else {
    report_error_fatal(parser, "Expected 'global' or 'proc' after 'extern'. Got '%v'", lexer_token_info(parser.lexer));
    return nil;
  }
}

/**
* Parses a directive declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed directive declaration.
*/
parser_parse_declaration_directive :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name, is_name := parser.lexer.token.value.(string);
  if lexer_is_token(parser.lexer, .Name) {
    lexer_expect_token(parser.lexer, .Name);
  } else {
    lexer_expect_token(parser.lexer, .Keyword_If);
  }

  if name == DECLARATION_DIRECTIVE_STATIC_ASSERT {
    return parser_parse_declaration_directive_assert(parser, position);
  } else if name == DECLARATION_DIRECTIVE_STATIC_IF {
    return parser_parse_declaration_directive_if(parser, position);
  } else if name == DECLARATION_DIRECTIVE_EXPAND_CONTEXT {
    return parser_parse_declaration_directive_expand_context(parser, position);
  } else {
    report_error_fatal(parser, "Unknown directive '%v'", name);
    return nil;
  }
}

/**
* Parses an assert directive declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed assert directive declaration.
*/
parser_parse_declaration_directive_assert :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_parenthesized(parser);

  return ast_declaration_make_directive_assert(position, expression);
}

/**
* Parses an if directive declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed if directive declaration.
*/
parser_parse_declaration_directive_if :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  condition := parser_parse_expression_parenthesized(parser);
  then_declarations := parser_parse_declaration_directive_if_block(parser);

  else_ifs: #soa [dynamic]Declaration_Directive_If_Else;
  else_declarations: [dynamic]^Declaration;
  for lexer_match_token(parser.lexer, .Keyword_Else) {
    if lexer_match_token(parser.lexer, .Pound) && lexer_match_token(parser.lexer, .Keyword_If) {
      else_if_condition := parser_parse_expression_parenthesized(parser);
      else_if_declarations := parser_parse_declaration_directive_if_block(parser);
      append_soa(&else_ifs, Declaration_Directive_If_Else{else_if_condition, else_if_declarations});
    } else {
      else_declarations = parser_parse_declaration_directive_if_block(parser);
      break;
    }
  }

  return ast_declaration_make_directive_if(position, condition, then_declarations, else_ifs, else_declarations);
}

/**
* Parses an if directive declaration block.
*
* @param parser The reference to the parser.
* @return The parsed if directive declaration block.
*/
parser_parse_declaration_directive_if_block :: proc(parser: ^Parser) -> [dynamic]^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Brace);
  declarations: [dynamic]^Declaration;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append(&declarations, parser_parse_declaration(parser));
  }
  lexer_expect_token(parser.lexer, .Right_Brace);
  return declarations;
}

/**
* Parses an expand context directive declaration.
*
* @param parser   The reference to the parser.
* @param position The source position of the declaration.
* @return The parsed expand context directive declaration.
*/
parser_parse_declaration_directive_expand_context :: proc(parser: ^Parser, position: Source_Position) -> ^Declaration {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  field_name := parser_parse_name(parser);
  lexer_expect_token(parser.lexer, .Comma);
  field_type := parser_parse_type(parser);
  lexer_expect_token(parser.lexer, .Right_Parentheses);

  return ast_declaration_make_directive_expand_context(position, field_name, field_type);
}

/**
* Parses generic type name declarations.
*
* @param parser   The reference to the parser.
* @param position The source position of the generic type names.
* @return 1. The parsed generic type name declarations; 2. True if generic type names where present otherwise false.
*/
parser_parse_declaration_generic_type_names :: proc(parser: ^Parser, position: Source_Position) -> ([dynamic]string, bool) {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  has_generic_type_names := false;
  generic_type_names: [dynamic]string;
  if lexer_match_token(parser.lexer, .Not) {
    lexer_expect_token(parser.lexer, .Left_Parentheses);
    for !lexer_is_token(parser.lexer, .Right_Parentheses) {
      append(&generic_type_names, parser_parse_name(parser));
      if !lexer_match_token(parser.lexer, .Comma) {
        break;
      }
    } 
    lexer_expect_token(parser.lexer, .Right_Parentheses);

    if len(generic_type_names) == 0 {
      report_error_fatal(position, "Expected at least one generic type name");
    }

    has_generic_type_names = true;
  }
  

  return generic_type_names, has_generic_type_names;
}

/**
* Parses a type specification.
*
* @param parser The reference to the parser.
* @return The parsed type specification.
*/
parser_parse_type :: proc(parser: ^Parser) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;

  if lexer_is_token(parser.lexer, .Name) {
    return parser_parse_type_name(parser, position);
  } else if lexer_match_token(parser.lexer, .Left_Bracket) {
    return parser_parse_type_array_or_slice(parser, position, .None);
  } else if lexer_match_token(parser.lexer, .Keyword_SoA) {
    if lexer_is_token(parser.lexer, .Multiply) {
      return parser_parse_type_pointer(parser, position, .SoA_Layout);
    } else {
      return parser_parse_type_array_or_slice(parser, position, .SoA);
    }
  } else if lexer_match_token(parser.lexer, .Keyword_AoSoA) {
    if lexer_is_token(parser.lexer, .Multiply) {
      return parser_parse_type_pointer(parser, position, .AoSoA_Layout);
    } else {
      return parser_parse_type_array_or_slice(parser, position, .AoSoA);
    }
  } else if lexer_match_token(parser.lexer, .Keyword_Map) {
    return parser_parse_type_map(parser, position);
  } else if lexer_match_token(parser.lexer, .Multiply) {
    return parser_parse_type_pointer(parser, position, .Absolute);
  } else if lexer_match_token(parser.lexer, .Negate) {
    return parser_parse_type_pointer(parser, position, .Self_Relative);
  } else if lexer_match_token(parser.lexer, .Xor) {
    return parser_parse_type_pointer(parser, position, .Offset_Relative);
  } else if lexer_match_token(parser.lexer, .Keyword_Dynamic) {
    return parser_parse_type_pointer(parser, position, .Dynamic);
  } else if lexer_match_token(parser.lexer, .Left_Parentheses) {
    type := parser_parse_type(parser);
    if lexer_is_token(parser.lexer, .Comma) {
      type = parser_parse_type_tuple(parser, position, type);
    } else {
      lexer_expect_token(parser.lexer, .Right_Parentheses);
    }
    return type;
  } else if lexer_match_token(parser.lexer, .Keyword_Proc) {
    return parser_parse_type_routine(parser, position, .Procedure, false);
  } else if lexer_match_token(parser.lexer, .Keyword_Func) {
    return parser_parse_type_routine(parser, position, .Function, false);
  } else if lexer_match_token(parser.lexer, .Keyword_Pure) {
    lexer_expect_token(parser.lexer, .Keyword_Func);
    return parser_parse_type_routine(parser, position, .Function, true);
  }

  report_error_fatal(parser, "Unexpected token '%v' in type", lexer_token_info(parser.lexer));
  return nil;
}

/**
* Parses a name type specification.
*
* @param parser   The reference to the parser.
* @param position The source position of the type specification.
* @return The parsed name type specification.
*/
parser_parse_type_name :: proc(parser: ^Parser, position: Source_Position) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  package_name := "";
  name := parser_parse_name(parser);
  if lexer_match_token(parser.lexer, .Dot) {
    package_name = name;
    name = parser_parse_name(parser);
  }
  generic_types := parser_parse_generic_types(parser);

  return ast_type_specification_make_name(position, package_name, name, generic_types);
}

/**
* Parses an array or slice type specification.
*
* @param parser          The reference to the parser.
* @param position        The source position of the type specification.
* @param layout_modifier The layout modifier of the type specification.
* @return The parsed array or slice type specification.
*/
parser_parse_type_array_or_slice :: proc(parser: ^Parser, position: Source_Position, layout_modifier: Layout_Modifier) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  if layout_modifier != .None {
    lexer_expect_token(parser.lexer, .Left_Bracket);
  }

  is_array := true;
  array_kind := Type_Specification_Array_Kind.Fixed;
  size: ^Expression = nil;
  if lexer_match_token(parser.lexer, .Ellipsis) {
    is_array = true;
    array_kind = .Fixed;
  } else if lexer_match_token(parser.lexer, .Keyword_Dynamic) {
    is_array = true;
    array_kind = .Dynamic;
  } else {
    if lexer_is_token(parser.lexer, .Right_Bracket) {
      is_array = false;
    } else {
      is_array = true;
      array_kind = .Fixed;
      size = parser_parse_expression(parser);
    }
  }
  lexer_expect_token(parser.lexer, .Right_Bracket);
  base := parser_parse_type(parser);

  if is_array {
    return ast_type_specification_make_array(position, array_kind, layout_modifier, base, size);
  } else {
    return ast_type_specification_make_slice(position, layout_modifier, base);
  }
}

/**
* Parses a map type specification.
*
* @param parser   The reference to the parser.
* @param position The source position of the type specification.
* @return The parsed map type specification.
*/
parser_parse_type_map :: proc(parser: ^Parser, position: Source_Position) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Bracket);
  key := parser_parse_type(parser);
  lexer_expect_token(parser.lexer, .Right_Bracket);
  value := parser_parse_type(parser);

  return ast_type_specification_make_map(position, key, value);
}

/**
* Parses a pointer type specification.
*
* @param parser   The reference to the parser.
* @param position The source position of the type specification.
* @param kind     The pointer kind of the type specification.
* @return The parsed pointer type specification.
*/
parser_parse_type_pointer :: proc(parser: ^Parser, position: Source_Position, pointer_kind: Type_Specification_Pointer_Kind) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  relative_base: ^Type_Specification;
  if pointer_kind != .Absolute {
    if (pointer_kind == .Self_Relative || pointer_kind == .Offset_Relative) {
      if !lexer_is_token(parser.lexer, .Multiply) {
        relative_base = parser_parse_type(parser);
      }
    }
    lexer_expect_token(parser.lexer, .Multiply);
  }

  base := parser_parse_type(parser);
  return ast_type_specification_make_pointer(position, pointer_kind, base, relative_base);
}

/**
* Parses a tuple specification.
*
* @param parser        The reference to the parser.
* @param position      The source position of the type specification.
* @param first_element The first element of the tuple type specification.
* @return The parsed tuple specification.
*/
parser_parse_type_tuple :: proc(parser: ^Parser, position: Source_Position, first_element: ^Type_Specification) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  elements: [dynamic]^Type_Specification;
  append(&elements, first_element);
  for lexer_match_token(parser.lexer, .Comma) {
    element := parser_parse_type(parser);
    append(&elements, element);
  }
  lexer_expect_token(parser.lexer, .Right_Parentheses);

  return ast_type_specification_make_tuple(position, elements);
}

/**
* Parses a routine type specification.
*
* @param parser   The reference to the parser.
* @param position The source position of the type specification.
* @param kind     The kind of the routine type specification.
* @param is_pure  Is the routine type specification a pure function?
* @return The parsed routine type specification.
*/
parser_parse_type_routine :: proc(parser: ^Parser, position: Source_Position, kind: Type_Specification_Kind, is_pure: bool) -> ^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  assert(kind == .Procedure || kind == .Function);

  calling_convention := parser_parse_calling_convention(parser, .Nox);

  parameters: [dynamic]^Type_Specification;
  has_params := false;
  has_c_varargs := false;

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  for !lexer_is_token(parser.lexer, .Right_Parentheses) {
    if has_c_varargs {
      report_error_fatal(parser, "C varargs have to be the last parameter in a routine");
    }
    if has_params {
      report_error_fatal(parser, "Parameter marked with 'params' has to be the last parameter in a routine");
    }
    
    parameter, parameter_is_params, parameter_is_c_varargs := parser_parse_type_routine_parameter(parser);
    has_params = parameter_is_params;
    has_c_varargs = parameter_is_c_varargs;
    if !parameter_is_c_varargs {
      append(&parameters, parameter);
    }

    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Parentheses);

  return_type: ^Type_Specification = nil;
  if lexer_match_token(parser.lexer, .Arrow) {
    return_type = parser_parse_type(parser);
  }

  return ast_type_specification_make_routine(kind, position, calling_convention, parameters, has_params, has_c_varargs, return_type, is_pure);
}

/**
* Parses a routine type specification parameter.
*
* @param parser The reference to the parser.
* @return 1. The parsed routine specification parameter; 2. True if the parameter is params otherwise false; 3. True if the parameter is c varargs otherwise false.
*/
parser_parse_type_routine_parameter :: proc(parser: ^Parser) -> (^Type_Specification, bool, bool) {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  type: ^Type_Specification;
  is_params := false;
  is_c_varargs := false;
  if lexer_match_token(parser.lexer, .Ellipsis) {
    is_c_varargs = true;
  } else {
    if lexer_match_token(parser.lexer, .Keyword_Params) {
      is_params = true;
    }
    
    type = parser_parse_type(parser);
    if lexer_match_token(parser.lexer, .Colon) && !is_params {
      if type.kind != .Name {
        report_error_fatal(parser, "Colons in parameters of procedure types must be preceded by a name");
      }
      
      if lexer_match_token(parser.lexer, .Keyword_Params) {
        is_params = true;
      }
      // No destroying of 'type' necessary here because we have a dedicated AST allocator.
      type = parser_parse_type(parser);
    }
  }
  
  return type, is_params, is_c_varargs;
}

/**
* Parses a statement.
*
* @param parser The reference to the parser.
* @return The parsed statement.
*/
parser_parse_statement :: proc(parser: ^Parser) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  // We skip every standalone semicolon as a statement.
  for lexer_match_token(parser.lexer, .Semicolon) {}

  position := parser.lexer.token.position;

  statement: ^Statement = nil;
  if lexer_match_token(parser.lexer, .Keyword_Return) {
    statement = parser_parse_statement_return(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Break) {
    statement = parser_parse_statement_break(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Continue) {
    statement = parser_parse_statement_continue(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Fallthrough) {
    statement = parser_parse_statement_fallthrough(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Defer) {
    statement = parser_parse_statement_defer(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Push_Context) {
    statement = parser_parse_statement_push_context(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_If) {
    statement = parser_parse_statement_if(parser, position, false);
  } else if lexer_match_token(parser.lexer, .Keyword_For) {
    statement = parser_parse_statement_for(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Foreach) {
    statement = parser_parse_statement_foreach(parser, position);
  } else if lexer_match_token(parser.lexer, .Keyword_Switch) {
    statement = parser_parse_statement_switch(parser, position);
  } else if lexer_is_token(parser.lexer, .Left_Brace) {
    statement = ast_statement_make_scope(position, parser_parse_statement_block(parser));
  } else if lexer_match_token(parser.lexer, .Pound) {
    if lexer_match_token(parser.lexer, .Keyword_If) {
      statement = parser_parse_statement_if(parser, position, true);
    } else {
      name := parser_parse_name(parser);
      report_error_fatal(parser, "Unknown directive '%v'", name);
    }
  } else {
    statement = parser_parse_statement_simple(parser);
    lexer_expect_token(parser.lexer, .Semicolon);
  }

  return statement;
}

/**
* Parses a return statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed return statement.
*/
parser_parse_statement_return :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression: ^Expression;
  if !lexer_is_token(parser.lexer, .Semicolon) {
    expression = parser_parse_expression(parser);
  }
  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_statement_make_return(position, expression);
}

/**
* Parses a break statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed break statement.
*/
parser_parse_statement_break :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_statement_make_break(position);
}

/**
* Parses a continue statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed continue statement.
*/
parser_parse_statement_continue :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_statement_make_continue(position);
}

/**
* Parses a fallthrough statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed fallthrough statement.
*/
parser_parse_statement_fallthrough :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Semicolon);
  return ast_statement_make_fallthrough(position);
}

/**
* Parses a defer statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed defer statement.
*/
parser_parse_statement_defer :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  statement := parser_parse_statement(parser);
  return ast_statement_make_defer(position, statement);
}

/**
* Parses a push context statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed push context statement.
*/
parser_parse_statement_push_context :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_parenthesized(parser);
  block := parser_parse_statement_block(parser);
  return ast_statement_make_push_context(position, expression, block);
}

/**
* Parses an if statement.
*
* @param parser    The reference to the parser.
* @param position  The source position of the statement.
* @param is_static Is the if statement static?
* @return The parsed if statement.
*/
parser_parse_statement_if :: proc(parser: ^Parser, position: Source_Position, is_static: bool) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  condition := parser_parse_expression_parenthesized(parser);

  then_block: Statement_Block;
  else_block: Statement_Block;
  else_ifs: #soa [dynamic]Statement_If_Else;
  if lexer_match_token(parser.lexer, .Keyword_Then) {
    then_block.start_position = parser.lexer.token.position;
    append(&then_block.statements, parser_parse_statement(parser));
    then_block.end_position = parser.lexer.token.position;
  } else {
    then_block = parser_parse_statement_block(parser);
    for lexer_match_token(parser.lexer, .Keyword_Else) {
      if (is_static && lexer_match_token(parser.lexer, .Pound) && lexer_match_token(parser.lexer, .Keyword_If)) || (!is_static && lexer_match_token(parser.lexer, .Keyword_If)) {
        else_if_condition := parser_parse_expression_parenthesized(parser);
        else_if_block := parser_parse_statement_block(parser);
        append_soa(&else_ifs, Statement_If_Else{else_if_condition, else_if_block});
      } else {
        else_block = parser_parse_statement_block(parser);
        break;
      }
    }
  }

  return ast_statement_make_if(position, is_static ? .Static_If : .If, condition, then_block, else_ifs, else_block);
}

/**
* Parses a for statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed for statement.
*/
parser_parse_statement_for :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  if lexer_match_token(parser.lexer, .Left_Parentheses) {
    if lexer_is_token(parser.lexer, .Semicolon) {
      return parser_parse_statement_for_statements(parser, position, nil);
    } else {
      statement := parser_parse_statement_simple(parser);

      if statement.kind == .Expression {
        // This is a loop with a simple condition.
        lexer_expect_token(parser.lexer, .Right_Parentheses);
        condition := (cast(^Statement_Expression) statement).expression;
        // No freeing of 'statement' necessary here because we have a dedicated AST allocator.
        return ast_statement_make_for(position, nil, condition, nil, parser_parse_statement_block(parser));
      } else {
        assert(statement.kind == .Initialize || statement.kind == .Assign);
        return parser_parse_statement_for_statements(parser, position, statement);
      }
    }
  } else {
    // Here we know we have an empty loop, meaning an endless one.
    return ast_statement_make_for(position, nil, nil, nil, parser_parse_statement_block(parser));
  }
}

/**
* Parses a for statement with regular statements.
*
* @param parser      The reference to the parser.
* @param position    The source position of the statement.
* @param initializer The initializer statement of the for statement.
* @return The parsed for statement.
*/
parser_parse_statement_for_statements :: proc(parser: ^Parser, position: Source_Position, initializer: ^Statement) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  // This parses a for-loop that can have an initializer, condition and/or a next statement.
  condition: ^Expression = nil;
  next: ^Statement = nil;

  if lexer_match_token(parser.lexer, .Semicolon) {
    if !lexer_is_token(parser.lexer, .Semicolon) {
      condition = parser_parse_expression(parser);
    }
    lexer_expect_token(parser.lexer, .Semicolon)
    if !lexer_is_token(parser.lexer, .Right_Parentheses) {
      next = parser_parse_statement_simple(parser);
      if next.kind == .Initialize {
        report_error_fatal(parser, "Initializer statement not allowed in for-statement's next clause");
      }
    }
  }

  lexer_expect_token(parser.lexer, .Right_Parentheses);
  return ast_statement_make_for(position, initializer, condition, next, parser_parse_statement_block(parser));
}

/**
* Parses a foreach statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed foreach statement.
*/
parser_parse_statement_foreach :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  element_name := parser_parse_name(parser);
  index_name: string;
  if lexer_match_token(parser.lexer, .Comma) {
    index_name = parser_parse_name(parser);
  }
  lexer_expect_token(parser.lexer, .Keyword_In);
  collection := parser_parse_expression(parser);
  lexer_expect_token(parser.lexer, .Right_Parentheses);

  block := parser_parse_statement_block(parser);

  return ast_statement_make_foreach(position, element_name, index_name, collection, block);
}

/**
* Parses a switch statement.
*
* @param parser   The reference to the parser.
* @param position The source position of the statement.
* @return The parsed switch statement.
*/
parser_parse_statement_switch :: proc(parser: ^Parser, position: Source_Position) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_parenthesized(parser);

  cases: #soa [dynamic]Statement_Switch_Case;
  has_default_case := false;
  
  lexer_expect_token(parser.lexer, .Left_Brace);
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    switch_case, is_default := parser_parse_statement_switch_case(parser);
    if is_default {
      if has_default_case {
        report_error_fatal(position, "Switch statement can't have multiple default cases");
      }
      has_default_case = true;
    }
    append_soa(&cases, switch_case);
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  if len(cases) == 0 {
    report_warning(position, "Switch statement is empty");
  }

  return ast_statement_make_switch(position, expression, cases);
}

/**
* Parses a switch statement case.
*
* @param parser The reference to the parser.
* @return 1. The parsed switch statement case; 2. True if the case is the default case otherwise false.
*/
parser_parse_statement_switch_case :: proc(parser: ^Parser) -> (Statement_Switch_Case, bool) {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  is_default := false;
  patterns: [dynamic]^Expression;
  block: Statement_Block;
  lexer_expect_token(parser.lexer, .Keyword_Case);

  if (lexer_is_token(parser.lexer, .Colon)) {
    is_default = true;
  } else {
    append(&patterns, parser_parse_expression(parser));
    for lexer_match_token(parser.lexer, .Comma) {
      append(&patterns, parser_parse_expression(parser));
    }
  }
  lexer_expect_token(parser.lexer, .Colon);

  block.start_position = parser.lexer.token.position;
  for !lexer_is_token(parser.lexer, .Right_Brace) && !lexer_is_token(parser.lexer, .Keyword_Case) {
    append(&block.statements, parser_parse_statement(parser));
  }
  block.end_position = parser.lexer.token.position;

  return {patterns, block}, is_default;
}

/**
* Parses a statement block.
*
* @param parser The reference to the parser.
* @return The parsed statement block.
*/
parser_parse_statement_block :: proc(parser: ^Parser) -> Statement_Block {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  start_position := parser.lexer.token.position;
  lexer_expect_token(parser.lexer, .Left_Brace);
  statements: [dynamic]^Statement;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append(&statements, parser_parse_statement(parser));
  }
  end_position := parser.lexer.token.position;
  lexer_expect_token(parser.lexer, .Right_Brace);
  return {start_position, end_position, statements};
}

/**
* Parses a simple statement.
*
* @param parser The reference to the parser.
* @return The parsed simple statement.
*/
parser_parse_statement_simple :: proc(parser: ^Parser) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  expression := parser_parse_expression(parser);
  
  // We only use the dynamic array if we actually get more than one expression on the left. 
  left_expressions: [dynamic]^Expression;
  if lexer_is_token(parser.lexer, .Comma) {
    append(&left_expressions, expression);
    for lexer_match_token(parser.lexer, .Comma) {
      append(&left_expressions, parser_parse_expression(parser));
    }
  }

  statement := parser_parse_statement_initialize(parser, expression, left_expressions);
  if statement == nil {
    if lexer_is_assign_operator(parser.lexer) {
      operator := parser.lexer.token.kind;
      lexer_get_next_token(parser.lexer);
      if len(left_expressions) == 0 {
        append(&left_expressions, expression);
      }
      statement = ast_statement_make_assign(position, operator, left_expressions, parser_parse_expression(parser));
    } else {
      statement = ast_statement_make_expression(position, expression);
    }
  } else {
    // No destroying of 'expressions' necessary here because we have a dedicated AST allocator.
  }
  return statement;
}

/**
* Parses an initialize statement.
*
* @param parser           The reference to the parser.
* @param left_expression  The left expression of the initialize statement.
* @param left_expressions The left expressions of the initialize statement (Only filled if there are multiple).
* @return The parsed initialize statement.
*/
parser_parse_statement_initialize :: proc(parser: ^Parser, left_expression: ^Expression, left_expressions: [dynamic]^Expression) -> ^Statement {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  names: [dynamic]string;
  if lexer_match_token(parser.lexer, .Colon_Assign) {
    if len(left_expressions) == 0 {
      if left_expression.kind != .Name {
        report_error_fatal(parser, "':=' must be preceded by a name");
      }
      name := (cast(^Expression_Name) left_expression).name;
      append(&names, name);
    } else {
      for expression in left_expressions {
        if expression.kind != .Name {
          report_error_fatal(parser, "':=' must be preceded by a name");
        }
        append(&names, (cast(^Expression_Name) expression).name);
      }
    }

    return ast_statement_make_initialize(left_expression.position, names, nil, parser_parse_expression(parser));
  } else if lexer_match_token(parser.lexer, .Colon) {
    if len(left_expressions) > 1 {
      report_error_fatal(parser, "':' cannot be used with multiple names");
    }

    if left_expression.kind != .Name {
      report_error_fatal(parser, "':' must be preceded by a name");
    }
    name := (cast(^Expression_Name) left_expression).name;
    append(&names, name);

    type := parser_parse_type(parser);

    expression: ^Expression = nil;
    if lexer_match_token(parser.lexer, .Assign) {
      expression = parser_parse_expression(parser);
    }
    return ast_statement_make_initialize(left_expression.position, names, type, expression);
  } else {
    return nil;
  }
}

/**
* Parses an expression.
*
* @param parser The reference to the parser.
* @return The parsed expression.
*/
parser_parse_expression :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  return parser_parse_expression_ternary(parser);
}

/**
* Parses a parenthesized expression.
*
* @param parser The reference to the parser.
* @return The parsed parenthesized expression.
*/
parser_parse_expression_parenthesized :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  expression := parser_parse_expression(parser);
  lexer_expect_token(parser.lexer, .Right_Parentheses);
  return expression;
}

/**
* Parses a ternary expression.
*
* @param parser The reference to the parser.
* @return The parsed ternary expression.
*/
parser_parse_expression_ternary :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  expression := parser_parse_expression_or(parser);
  if lexer_match_token(parser.lexer, .Question) {
    then_expression := parser_parse_expression_ternary(parser);
    lexer_expect_token(parser.lexer, .Colon);
    else_expression := parser_parse_expression_ternary(parser);
    expression = ast_expression_make_ternary(position, expression, then_expression, else_expression);
  }
  return expression;
}

/**
* Parses an or expression.
*
* @param parser The reference to the parser.
* @return The parsed or expression.
*/
parser_parse_expression_or :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_and(parser);
  for lexer_match_token(parser.lexer, .Or_Or) {
    position := parser.lexer.token.position;
    expression = ast_expression_make_binary(position, .Or_Or, expression, parser_parse_expression_and(parser));
  }
  return expression;
}

/**
* Parses an and expression.
*
* @param parser The reference to the parser.
* @return The parsed and expression.
*/
parser_parse_expression_and :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_comparison(parser);
  for lexer_match_token(parser.lexer, .And_And) {
    position := parser.lexer.token.position;
    expression = ast_expression_make_binary(position, .And_And, expression, parser_parse_expression_comparison(parser));
  }
  return expression;
}

/**
* Parses a comparison expression.
*
* @param parser The reference to the parser.
* @return The parsed comparison expression.
*/
parser_parse_expression_comparison :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_addition(parser);
  for lexer_is_comparison_operator(parser.lexer) {
    position := parser.lexer.token.position;
    operator := parser.lexer.token.kind;
    lexer_get_next_token(parser.lexer);
    expression = ast_expression_make_binary(position, operator, expression, parser_parse_expression_addition(parser));
  }
  return expression;
}

/**
* Parses an addition expression.
*
* @param parser The reference to the parser.
* @return The parsed addition expression.
*/
parser_parse_expression_addition :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_multiplication(parser);
  for lexer_is_additive_operator(parser.lexer) {
    position := parser.lexer.token.position;
    operator := parser.lexer.token.kind;
    lexer_get_next_token(parser.lexer);
    expression = ast_expression_make_binary(position, operator, expression, parser_parse_expression_multiplication(parser));
  }
  return expression;
}

/**
* Parses a multiplication expression.
*
* @param parser The reference to the parser.
* @return The parsed multiplication expression.
*/
parser_parse_expression_multiplication :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_unary(parser);
  for lexer_is_multiplicative_operator(parser.lexer) {
    position := parser.lexer.token.position;
    operator := parser.lexer.token.kind;
    lexer_get_next_token(parser.lexer);
    expression = ast_expression_make_binary(position, operator, expression, parser_parse_expression_unary(parser));
  }
  return expression;
}

/**
* Parses a unary expression.
*
* @param parser The reference to the parser.
* @return The parsed unary expression.
*/
parser_parse_expression_unary :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  if lexer_is_unary_operator(parser.lexer) {
    position := parser.lexer.token.position;
    operator := parser.lexer.token.kind;
    lexer_get_next_token(parser.lexer);
    if operator == .Increment || operator == .Decrement {
      return ast_expression_make_modify(position, operator, false, parser_parse_expression_unary(parser));
    } else {
      return ast_expression_make_unary(position, operator, parser_parse_expression_unary(parser));
    }
  } else {
    return parser_parse_expression_base(parser);
  }
}

/**
* Parses a base expression.
*
* @param parser The reference to the parser.
* @return The parsed base expression.
*/
parser_parse_expression_base :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  expression := parser_parse_expression_operand(parser);
  
  for lexer_is_token(parser.lexer, .Left_Parentheses) ||
    lexer_is_token(parser.lexer, .Left_Bracket) ||
    lexer_is_token(parser.lexer, .Dot) ||
    lexer_is_token(parser.lexer, .Increment) ||
    lexer_is_token(parser.lexer, .Decrement)
  {
    position := parser.lexer.token.position;

    if lexer_match_token(parser.lexer, .Left_Parentheses) {
      arguments: [dynamic]^Expression;
      for !lexer_is_token(parser.lexer, .Right_Parentheses) {
        append(&arguments, parser_parse_expression(parser));
        if !lexer_match_token(parser.lexer, .Comma) {
          break;
        }
      }
      lexer_expect_token(parser.lexer, .Right_Parentheses);
      expression = ast_expression_make_call(position, expression, arguments);
    } else if lexer_match_token(parser.lexer, .Left_Bracket) {
      if lexer_match_token(parser.lexer, .Colon) {
        higher: ^Expression;
        if !lexer_is_token(parser.lexer, .Right_Bracket) {
          higher = parser_parse_expression(parser);
        }
        lexer_expect_token(parser.lexer, .Right_Bracket);
        expression = ast_expression_make_slice(position, expression, nil, higher);
      } else {
        index := parser_parse_expression(parser);
        if lexer_match_token(parser.lexer, .Colon) {
          lower := index;
          higher: ^Expression;
          if !lexer_is_token(parser.lexer, .Right_Bracket) {
            higher = parser_parse_expression(parser);
          }
          lexer_expect_token(parser.lexer, .Right_Bracket);
          expression = ast_expression_make_slice(position, expression, lower, higher);
        } else {
          lexer_expect_token(parser.lexer, .Right_Bracket);
          expression = ast_expression_make_index(position, expression, index);
        }
      }
    } else if lexer_match_token(parser.lexer, .Dot) {
      if lexer_match_token(parser.lexer, .Left_Parentheses) {
        selection_type := parser_parse_type(parser);
        lexer_expect_token(parser.lexer, .Right_Parentheses);
        expression = ast_expression_make_selector(position, expression, selection_type);
      } else {
        field, is_string := parser.lexer.token.value.(string);
        lexer_expect_token(parser.lexer, .Name);
        generic_types := parser_parse_generic_types(parser);
        expression = ast_expression_make_member(position, expression, field, generic_types);
      }
    } else {
      assert(lexer_is_token(parser.lexer, .Increment) || lexer_is_token(parser.lexer, .Decrement))
      operator := parser.lexer.token.kind;
      lexer_get_next_token(parser.lexer);
      expression = ast_expression_make_modify(position, operator, true, expression);
    }
  }

  return expression;
}

/**
* Parses an operand expression.
*
* @param parser The reference to the parser.
* @return The parsed operand expression.
*/
parser_parse_expression_operand :: proc(parser: ^Parser) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;

  if lexer_is_token(parser.lexer, .Boolean) {
    value, is_integer := parser.lexer.token.value.(bool);
    lexer_get_next_token(parser.lexer);
    return ast_expression_make_boolean(position, value);
  } else if lexer_is_token(parser.lexer, .Integer) {
    value, is_integer := parser.lexer.token.value.(u64);
    lexer_get_next_token(parser.lexer);
    return ast_expression_make_integer(position, value);
  } else if lexer_is_token(parser.lexer, .Float) {
    value, is_float := parser.lexer.token.value.(f64);
    lexer_get_next_token(parser.lexer);
    return ast_expression_make_float(position, value);
  } else if lexer_is_token(parser.lexer, .Character) {
    value, is_rune := parser.lexer.token.value.(rune);
    lexer_get_next_token(parser.lexer);
    return ast_expression_make_character(position, value);
  } else if lexer_is_token(parser.lexer, .String) {
    value, is_string := parser.lexer.token.value.(string);
    lexer_get_next_token(parser.lexer);
    return ast_expression_make_string(position, value);
  } else if lexer_is_token(parser.lexer, .Name) {
    // Name expressions are special as they could also refer to types at the start of a compound literal.
    // That needs to be handled explicitly (including selector expressions).
    name, is_string := parser.lexer.token.value.(string);
    lexer_get_next_token(parser.lexer);
    first_generic_types := parser_parse_generic_types(parser);
    if lexer_match_token(parser.lexer, .Dot) {
      if lexer_match_token(parser.lexer, .Left_Parentheses) {
        selection_type := parser_parse_type(parser);
        lexer_expect_token(parser.lexer, .Right_Parentheses);
        return ast_expression_make_selector(position, ast_expression_make_name(position, name, nil), selection_type);
      } else {
        field, is_string := parser.lexer.token.value.(string);
        lexer_expect_token(parser.lexer, .Name);
        second_generic_types := parser_parse_generic_types(parser);
        if lexer_is_token(parser.lexer, .Left_Brace) {
          return parser_parse_expression_compound(parser, ast_type_specification_make_name(position, name, field, second_generic_types));
        } else {
          return ast_expression_make_member(position, ast_expression_make_name(position, name, first_generic_types), field, second_generic_types);
        }
      }
    } else if lexer_is_token(parser.lexer, .Left_Brace) {
      return parser_parse_expression_compound(parser, ast_type_specification_make_name(position, "", name, first_generic_types));
    } else {
      return ast_expression_make_name(position, name, first_generic_types);
    }
  } else if lexer_match_token(parser.lexer, .Dot) {
    name := parser_parse_name(parser);
    return ast_expression_make_implicit_selector(position, name);
  } else if lexer_is_token(parser.lexer, .Left_Brace) {
    return parser_parse_expression_compound(parser, nil);
  } else if lexer_match_token(parser.lexer, .Left_Parentheses) {
    if lexer_match_token(parser.lexer, .Colon) {
      type := parser_parse_type(parser);
      lexer_expect_token(parser.lexer, .Right_Parentheses);
      return parser_parse_expression_compound(parser, type);
    } else {
      expression := parser_parse_expression(parser);
      lexer_expect_token(parser.lexer, .Right_Parentheses);
      return ast_expression_make_parenthesized(position, expression);
    }
  } else if lexer_match_token(parser.lexer, .Keyword_Cast) {
    lexer_expect_token(parser.lexer, .Left_Parentheses);
    type := parser_parse_type(parser);
    lexer_expect_token(parser.lexer, .Right_Parentheses);
    return ast_expression_make_cast(position, type, parser_parse_expression_unary(parser));
  } else if lexer_match_token(parser.lexer, .Keyword_Size_Of) {
    return parser_parse_expression_query(parser, position, .Size_Of_Expression, .Size_Of_Type);    
  } else if lexer_match_token(parser.lexer, .Keyword_Typeid_Of) {
    return parser_parse_expression_query(parser, position, .Typeid_Of_Expression, .Typeid_Of_Type);
  } else if lexer_match_token(parser.lexer, .Keyword_Type_Info_Of) {
    return parser_parse_expression_query(parser, position, .Type_Info_Of_Expression, .Type_Info_Of_Type);
  } else if lexer_match_token(parser.lexer, .Pound) {
    return parser_parse_expression_directive(parser, position);
  } else {
    report_error_fatal(position, "Unexpected token '%v' in expression", lexer_token_info(parser.lexer));
    return nil;
  }
}

/**
* Parses a compound expression.
*
* @param parser The reference to the parser.
* @param type   The type of the compound expression.
* @return The parsed compound expression.
*/
parser_parse_expression_compound :: proc(parser: ^Parser, type: ^Type_Specification) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;
  lexer_expect_token(parser.lexer, .Left_Brace);
  fields: #soa [dynamic]Expression_Compound_Field;
  for !lexer_is_token(parser.lexer, .Right_Brace) {
    append_soa(&fields, parser_parse_expression_compound_field(parser))
    if !lexer_match_token(parser.lexer, .Comma) {
      break;
    }
  }
  lexer_expect_token(parser.lexer, .Right_Brace);

  return ast_expression_make_compound(position, type, fields);
}

/**
* Parses a compound expression field.
*
* @param parser The reference to the parser.
* @return The parsed compound expression field.
*/
parser_parse_expression_compound_field :: proc(parser: ^Parser) -> Expression_Compound_Field {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  position := parser.lexer.token.position;

  if lexer_match_token(parser.lexer, .Left_Bracket) {
    index := parser_parse_expression(parser);
    lexer_expect_token(parser.lexer, .Right_Bracket);
    lexer_expect_token(parser.lexer, .Assign);
    return {.Index, position, parser_parse_expression(parser), index};
  } else {
    expression := parser_parse_expression(parser);
    if lexer_match_token(parser.lexer, .Assign) {
      if expression.kind != .Name {
        report_error_fatal(parser, "Named initializer in compound literal must be preceded by a field name");
      }
      return {.Name, position, parser_parse_expression(parser), (cast(^Expression_Name) expression).name};
    } else {
      return {.Default, position, expression, {}};
    }
  }
}

/**
* Parses a query expression.
*
* @param parser          The reference to the parser.
* @param position        The source position of the expression.
* @param expression_kind The kind of the query expression in case of an expression.
* @param type_kind       The kind of the query expression in case of a type.
* @return The parsed query expression.
*/
parser_parse_expression_query :: proc(
  parser: ^Parser,
  position: Source_Position,
  expression_kind: Expression_Query_Kind,
  type_kind: Expression_Query_Kind,
) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  kind: Expression_Query_Kind;
  value: Expression_Query_Value;

  lexer_expect_token(parser.lexer, .Left_Parentheses);
  if lexer_match_token(parser.lexer, .Colon) {
    kind = type_kind;
    value = parser_parse_type(parser);
  } else {
    kind = expression_kind;
    value = parser_parse_expression(parser);
  }
  lexer_expect_token(parser.lexer, .Right_Parentheses);
  
  return ast_expression_make_query(position, kind, value);
}

/**
* Parses a directive expression.
*
* @param parser   The reference to the parser.
* @param position The source position of the expression.
* @return The parsed directive expression.
*/
parser_parse_expression_directive :: proc(parser: ^Parser, position: Source_Position) -> ^Expression {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name := parser_parse_name(parser);

  kind: Expression_Directive_Kind;
  if name == EXPRESSION_DIRECTIVE_LINE {
    kind = .Line;
  } else if name == EXPRESSION_DIRECTIVE_ROUTINE {
    kind = .Routine;
  } else if name == EXPRESSION_DIRECTIVE_FILE {
    kind = .File;
  } else if name == EXPRESSION_DIRECTIVE_LOCATION {
    kind = .Location;
  } else {
    report_error_fatal(parser, "Unknown directive '%v'", name);
  }

  return ast_expression_make_directive(position, kind);
}

/**
* Parses a name.
*
* @param parser The reference to the parser.
* @return The parsed name.
*/
parser_parse_name :: proc(parser: ^Parser) -> string {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  name, is_string := parser.lexer.token.value.(string);
  lexer_expect_token(parser.lexer, .Name);
  return name;
}

/**
* Parses generic types.
*
* @param parser The reference to the parser.
* @return The parsed generic types.
*/
parser_parse_generic_types :: proc(parser: ^Parser) -> [dynamic]^Type_Specification {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  generic_types: [dynamic]^Type_Specification;

  position := parser.lexer.token.position;

  // This '!' is required for an unambiguous grammar with the '<' operator.
  if lexer_match_token(parser.lexer, .Not) {
    lexer_expect_token(parser.lexer, .Left_Parentheses);
    for !lexer_is_token(parser.lexer, .Right_Parentheses) {
      append(&generic_types, parser_parse_type(parser));
      if !lexer_match_token(parser.lexer, .Comma) {
        break;
      }
    }
    lexer_expect_token(parser.lexer, .Right_Parentheses);

    if len(generic_types) == 0 {
      report_error_fatal(position, "Expected at least one generic type parameter");
    }
  }

  return generic_types;
}

/**
* Parses an attribute kind.
*
* @param parser The reference to the parser.
* @return The parsed attribute kind.
*/
parser_parse_attribute_kind :: proc(parser: ^Parser) -> Attribute_Kind {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  attribute_kind_name := parser_parse_name(parser);

  result := Attribute_Kind.Invalid;
  switch attribute_kind_name {
    case ATTRIBUTE_FLAGS:        result = .Flags;
    case ATTRIBUTE_THREAD_LOCAL: result = .Thread_Local;
    case ATTRIBUTE_INTRINSIC:    result = .Intrinsic;
    case ATTRIBUTE_BUILTIN:      result = .Builtin;
    case ATTRIBUTE_DISABLED:     result = .Disabled;
    case: report_error_fatal(parser.lexer, "Invalid attribute '%v'", attribute_kind_name);
  }

  return result;
}

/**
* Parses an access modifier.
*
* @param parser The reference to the parser.
* @return The parsed access modifier.
*/
parser_parse_access_modifier :: proc(parser: ^Parser, default: Access_Modifier) -> Access_Modifier {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  access_modifier := default;
  if lexer_match_token(parser.lexer, .Keyword_Private) {
    access_modifier = .Private
  } else if lexer_match_token(parser.lexer, .Keyword_Internal) {
    access_modifier = .Internal;
  } else if lexer_match_token(parser.lexer, .Keyword_Public) {
    access_modifier = .Public;
  }

  return access_modifier;
}

/**
* Parses a calling convention.
*
* @param parser The reference to the parser.
* @return The parsed calling convention.
*/
parser_parse_calling_convention :: proc(parser: ^Parser, default: Calling_Convention) -> Calling_Convention {
  tracy.ZoneC(ZONE_COLOR_PARSER);

  result := default;
  if lexer_is_token(parser.lexer, .String) {
    calling_convention_string := parser.lexer.token.value.(string);
    lexer_get_next_token(parser.lexer);

    switch calling_convention_string {
      case CALLING_CONVENTION_NOX:       result = .Nox;
      case CALLING_CONVENTION_NOCONTEXT: result = .No_Context;
      case CALLING_CONVENTION_C:         result = .C;
      case CALLING_CONVENTION_STDCALL:   result = .Std_Call;
      case CALLING_CONVENTION_FASTCALL:  result = .Fast_Call;
      case CALLING_CONVENTION_WIN64:     result = .Win64;
      case: report_error_fatal(parser.lexer, "Invalid calling convention '%v'", calling_convention_string);
    }
    
    // No destroying of 'calling_convention_string' necessary here because we have a dedicated AST allocator.
  }

  return result;
}

ATTRIBUTE_FLAGS :: "flags";
ATTRIBUTE_THREAD_LOCAL :: "thread_local";
ATTRIBUTE_INTRINSIC :: "intrinsic";
ATTRIBUTE_BUILTIN :: "builtin";
ATTRIBUTE_DISABLED :: "disabled";

DECLARATION_DIRECTIVE_STATIC_ASSERT :: "assert";
DECLARATION_DIRECTIVE_STATIC_IF :: "if";
DECLARATION_DIRECTIVE_EXPAND_CONTEXT :: "expand_context";

EXPRESSION_DIRECTIVE_LINE :: "line";
EXPRESSION_DIRECTIVE_ROUTINE :: "routine";
EXPRESSION_DIRECTIVE_FILE :: "file";
EXPRESSION_DIRECTIVE_LOCATION :: "location";

CALLING_CONVENTION_NOX :: "nox";
CALLING_CONVENTION_NOCONTEXT :: "nocontext";
CALLING_CONVENTION_C :: "c";
CALLING_CONVENTION_STDCALL :: "stdcall";
CALLING_CONVENTION_FASTCALL :: "fastcall";
CALLING_CONVENTION_WIN64 :: "win64";
