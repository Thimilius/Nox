package nox

import "core:runtime"

/**
* Represents a generic context for the instantiation of an AST.
*/ 
Instantiation_Context :: struct {
  generic_type_names: [dynamic]string,         // The generic type names that need to be instantiated.
  generic_types: [dynamic]^Type_Specification, // The generic types that are used for the instantiation corresponding to type name.
}

/**
* Instantiates the AST of a routine declaration.
*
* @param declaration           The routine declaration to instantiate.
* @param instantiation_context The generic instantiation context to use in the process.
* @param allocator             The allocator to use in the process.
*/
ast_declaration_routine_instantiate :: proc(declaration: ^Declaration, instantiation_context: Instantiation_Context, allocator: runtime.Allocator) {
  assert(declaration.kind == .Procedure || declaration.kind == .Function);
  assert(len(instantiation_context.generic_type_names) == len(instantiation_context.generic_types));

  context.allocator = allocator;

  declaration_routine := cast(^Declaration_Routine) declaration;

  instantiated_parameters: #soa [dynamic]Declaration_Routine_Parameter;
  for paramter in declaration_routine.parameters {
    new_parameter := paramter;
    new_parameter.type = ast_type_specification_instantiate(paramter.type, instantiation_context);
    if new_parameter.initializer != nil {
      ast_expression_instantiate(new_parameter.initializer, instantiation_context);
    }
    append_soa(&instantiated_parameters, new_parameter);
  }
  declaration_routine.parameters = instantiated_parameters;
  declaration_routine.return_type = ast_type_specification_instantiate(declaration_routine.return_type, instantiation_context);

  instantiated_generic_constraints: #soa [dynamic]Declaration_Routine_Generic_Constraint;
  for generic_constraint in declaration_routine.generic_constraints {
    new_generic_constraint := generic_constraint;
    switch generic_constraint.kind {
      case .Expression: {
        generic_constraint_expression := generic_constraint.value.(Declaration_Routine_Generic_Constraint_Expression);
        ast_expression_instantiate(generic_constraint_expression.expression, instantiation_context);
      }
      case .Interface: {
        generic_constraint_interface := generic_constraint.value.(Declaration_Routine_Generic_Constraint_Interface);
        generic_constraint_interface.type = ast_type_specification_instantiate(generic_constraint_interface.type, instantiation_context);
        new_generic_constraint.value = generic_constraint_interface;
      }
    }
    append_soa(&instantiated_generic_constraints, new_generic_constraint);
  }
  declaration_routine.generic_constraints = instantiated_generic_constraints;

  ast_statement_block_instantiate(declaration_routine.block, instantiation_context);
}

/**
* Instantiates the AST of a type specification.
*
* @param type                  The type specification to instantiate.
* @param instantiation_context The generic instantiation context to use in the process.
* @return The instantiated type specification.
*/
ast_type_specification_instantiate :: proc(type: ^Type_Specification, instantiation_context: Instantiation_Context) -> ^Type_Specification {
  type := type;

  if type == nil do return nil;

  switch type.kind {
    case .Name: {
      type_value := cast(^Type_Specification_Name) type;
      for generic_type_name, i in instantiation_context.generic_type_names {
        if type_value.name == generic_type_name {
          // No destroying of non-instantiated 'type' necessary here because we have a dedicated AST allocator.
          type = ast_type_specification_clone(instantiation_context.generic_types[i]);
          if type.kind == .Name {
            type_value = cast(^Type_Specification_Name) type;
          } else {
            return type;
          }
        }
      }
      new_generic_types: [dynamic]^Type_Specification;
      for generic_type in type_value.generic_types {
        append(&new_generic_types, ast_type_specification_instantiate(generic_type, instantiation_context));
      }
      delete(type_value.generic_types);
      type_value.generic_types = new_generic_types;
      return type_value;
    }
    case .Pointer: {
      type_value := cast(^Type_Specification_Pointer) type;
      type_value.base = ast_type_specification_instantiate(type_value.base, instantiation_context);
      type_value.relative_base = ast_type_specification_instantiate(type_value.relative_base, instantiation_context);
      return type_value;
    }
    case .Array: {
      type_value := cast(^Type_Specification_Array) type;
      type_value.base = ast_type_specification_instantiate(type_value.base, instantiation_context);
      return type_value;
    }
    case .Slice: {
      type_value := cast(^Type_Specification_Slice) type;
      type_value.base = ast_type_specification_instantiate(type_value.base, instantiation_context);
      return type_value;
    }
    case .Map: {
      type_value := cast(^Type_Specification_Map) type;
      type_value.key = ast_type_specification_instantiate(type_value.key, instantiation_context);
      type_value.value = ast_type_specification_instantiate(type_value.value, instantiation_context);
      return type_value;
    }
    case .Tuple: {
      type_value := cast(^Type_Specification_Tuple) type;
      elements: [dynamic]^Type_Specification;
      for element in type_value.elements {
        append(&elements, ast_type_specification_instantiate(element, instantiation_context));
      }
      delete(type_value.elements);
      type_value.elements = elements;
      return type_value;
    }
    case .Procedure, .Function: {
      type_value := cast(^Type_Specification_Routine) type;
      parameters: [dynamic]^Type_Specification;
      for parameter in type_value.parameters {
        append(&parameters, ast_type_specification_instantiate(parameter, instantiation_context));
      }
      delete(type_value.parameters);
      type_value.parameters = parameters;
      type_value.return_type = ast_type_specification_instantiate(type_value.return_type, instantiation_context);
      return type_value;
    }

    case .None: fallthrough;
    case: assert(false);
  }

  return type;
}

/**
* Instantiates the AST of a statement.
*
* @param type                  The statement to instantiate.
* @param instantiation_context The generic instantiation context to use in the process.
*/
ast_statement_instantiate :: proc(statement: ^Statement, instantiation_context: Instantiation_Context) {
  if statement == nil do return;

  switch statement.kind {
    case .Return: {
      ast_expression_instantiate((cast(^Statement_Return) statement).expression, instantiation_context);
    }
    case .Break, .Continue, .Fallthrough:
    case .Defer: {
      ast_statement_instantiate((cast(^Statement_Defer) statement).statement, instantiation_context);
    }
    case .Push_Context: {
      statement_value := cast(^Statement_Push_Context) statement;
      ast_expression_instantiate(statement_value.expression, instantiation_context);
      ast_statement_block_instantiate(statement_value.block, instantiation_context);
    }
    
    case .Scope: {
      ast_statement_block_instantiate((cast(^Statement_Scope )statement).block, instantiation_context);
    }
    case .If, .Static_If: {
      statement_value := cast(^Statement_If) statement;
      ast_expression_instantiate(statement_value.condition, instantiation_context);
      ast_statement_block_instantiate(statement_value.then_block, instantiation_context);
      for else_if in statement_value.else_ifs {
        ast_expression_instantiate(else_if.condition, instantiation_context);
        ast_statement_block_instantiate(else_if.block, instantiation_context);
      }
      ast_statement_block_instantiate(statement_value.else_block, instantiation_context);
    }
    case .For: {
      statement_value := cast(^Statement_For) statement;
      ast_statement_instantiate(statement_value.initializer, instantiation_context);
      ast_expression_instantiate(statement_value.condition, instantiation_context);
      ast_statement_instantiate(statement_value.next, instantiation_context);
      ast_statement_block_instantiate(statement_value.block, instantiation_context);
    }
    case .Foreach: {
      statement_value := cast(^Statement_Foreach) statement;
      ast_expression_instantiate(statement_value.collection, instantiation_context);
      ast_statement_block_instantiate(statement_value.block, instantiation_context);
    }
    case .Switch: {
      statement_value := cast(^Statement_Switch) statement;
      ast_expression_instantiate(statement_value.expression, instantiation_context);
      for switch_case in statement_value.cases {
        for pattern in switch_case.patterns {
          ast_expression_instantiate(pattern, instantiation_context);
        }
        ast_statement_block_instantiate(switch_case.block, instantiation_context);
      }
    }

    case .Assign: {
      statement_value := cast(^Statement_Assign) statement;
      for left_expression in statement_value.left_expressions {
        ast_expression_instantiate(left_expression, instantiation_context);
      }
      ast_expression_instantiate(statement_value.right_expression, instantiation_context);
    }
    case .Initialize: {
      statement_value := cast(^Statement_Initialize) statement;
      statement_value.type = ast_type_specification_instantiate(statement_value.type, instantiation_context);
      ast_expression_instantiate(statement_value.expression, instantiation_context);
    }
    case .Expression: {
      ast_expression_instantiate((cast(^Statement_Expression) statement).expression, instantiation_context);
    }

    case .None: fallthrough;
    case: assert(false);
  }
}

/**
* Instantiates the AST of a statement block.
*
* @param type                  The statement block to instantiate.
* @param instantiation_context The generic instantiation context to use in the process.
*/
ast_statement_block_instantiate :: proc(block: Statement_Block, instantiation_context: Instantiation_Context) {
  for statement in block.statements {
    ast_statement_instantiate(statement, instantiation_context);
  }
}

/**
* Instantiates the AST of an expression.
*
* @param type                  The expression to instantiate.
* @param instantiation_context The generic instantiation context to use in the process.
*/
ast_expression_instantiate :: proc(expression: ^Expression, instantiation_context: Instantiation_Context) {
  if expression == nil do return;

  switch expression.kind {
    case .Parenthesized: {
      ast_expression_instantiate((cast(^Expression_Parenthesized) expression).expression, instantiation_context);
    }

    case .Boolean, .Integer, .Float, .Character, .String:
    case .Name: {
      expression_value := cast(^Expression_Name) expression;
      new_generic_types: [dynamic]^Type_Specification;
      for generic_type in expression_value.generic_types {
        append(&new_generic_types, ast_type_specification_instantiate(generic_type, instantiation_context));
      }
      delete(expression_value.generic_types);
      expression_value.generic_types = new_generic_types;
    }

    case .Cast: {
      expression_value := cast(^Expression_Cast) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      expression_value.type = ast_type_specification_instantiate(expression_value.type, instantiation_context);
    }
    case .Call: {
      expression_value := cast(^Expression_Call) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      for argument in expression_value.arguments {
        ast_expression_instantiate(argument, instantiation_context);
      }
    }
    case .Index: {
      expression_value := cast(^Expression_Index) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      ast_expression_instantiate(expression_value.index, instantiation_context);
    }
    case .Slice: {
      expression_value := cast(^Expression_Slice) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      ast_expression_instantiate(expression_value.lower, instantiation_context);
      ast_expression_instantiate(expression_value.higher, instantiation_context);
    }
    case .Member: {
      expression_value := cast(^Expression_Member) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      new_generic_types: [dynamic]^Type_Specification;
      for generic_type in expression_value.generic_types {
        append(&new_generic_types, ast_type_specification_instantiate(generic_type, instantiation_context));
      }
      delete(expression_value.generic_types);
      expression_value.generic_types = new_generic_types;
    }
    case .Compound: {
      expression_value := cast(^Expression_Compound) expression;
      for field in expression_value.fields {
        ast_expression_instantiate(field.initializer, instantiation_context);
        if field.kind == .Index {
          ast_expression_instantiate(field.value.(^Expression), instantiation_context);
        }
      }
      expression_value.type = ast_type_specification_instantiate(expression_value.type, instantiation_context);
    }
    case .Selector: {
      expression_value := cast(^Expression_Selector) expression;
      ast_expression_instantiate(expression_value.expression, instantiation_context);
      expression_value.type = ast_type_specification_instantiate(expression_value.type, instantiation_context);
    }
    case .Implicit_Selector:

    case .Unary: {
      ast_expression_instantiate((cast(^Expression_Unary) expression).expression, instantiation_context);
    }
    case .Binary: {
      expression_value := cast(^Expression_Binary) expression;
      ast_expression_instantiate(expression_value.left, instantiation_context);
      ast_expression_instantiate(expression_value.right, instantiation_context);
    }
    case .Ternary: {
      expression_value := cast(^Expression_Ternary) expression;
      ast_expression_instantiate(expression_value.condition, instantiation_context);
      ast_expression_instantiate(expression_value.then_expression, instantiation_context);
      ast_expression_instantiate(expression_value.else_expression, instantiation_context);
    }
    case .Modify: {
      ast_expression_instantiate((cast(^Expression_Modify) expression).expression, instantiation_context);
    }

    case .Query: {
      expression_value := cast(^Expression_Query) expression;
      switch expression_value.query_kind {
        case .Size_Of_Expression, .Typeid_Of_Expression, .Type_Info_Of_Expression: ast_expression_instantiate(
          expression_value.value.(^Expression),
          instantiation_context,
        );
        case .Size_Of_Type, .Typeid_Of_Type, .Type_Info_Of_Type: expression_value.value = ast_type_specification_instantiate(
          expression_value.value.(^Type_Specification),
          instantiation_context,
        );
      }
    }
    case .Directive:

    case .None: fallthrough;
    case: assert(false);
  }
}
