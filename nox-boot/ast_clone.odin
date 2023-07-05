
package nox

import "core:runtime"
import "core:strings"

/**
* Clones the AST of a routine declaration.
*
* @param original The original routine declaration to clone
* @param allocator The allocator to use in the process.
* @return The cloned routine declaration.
*/
ast_declaration_clone_routine :: proc(original: ^Declaration, allocator: runtime.Allocator) -> ^Declaration {
  assert(original.kind == .Procedure || original.kind == .Function);

  context.allocator = allocator;

  original_value := cast(^Declaration_Routine) original;
  clone_value := ast_make_clone(original_value);

  clone_parameters: #soa [dynamic]Declaration_Routine_Parameter;
  for parameter in original_value.parameters {
    parameter_type := ast_type_specification_clone(parameter.type);
    parameter_initializer := ast_expression_clone(parameter.initializer);
    append_soa(&clone_parameters, Declaration_Routine_Parameter{parameter.position, parameter.name, parameter_type, parameter_initializer});
  }
  clone_value.parameters = clone_parameters;
  clone_value.return_type = ast_type_specification_clone(original_value.return_type);

  // NOTE: We deliberatly don't clone generic type names as they are not going to be needed after the clone.
  // The reason for this is that we currently only ever clone routines when we are dealing with instantiation of generic routines.
  clone_value.generic_type_names = {};

  // We do clone generic constraints however as they can contain expression we might need to instantiate.
  generic_constraints: #soa [dynamic]Declaration_Routine_Generic_Constraint;
  for generic_constraint in original_value.generic_constraints {
    clone_generic_constraint := generic_constraint;
    switch generic_constraint.kind {
      case .Expression: {
        generic_constraint_expression := generic_constraint.value.(Declaration_Routine_Generic_Constraint_Expression).expression;
        clone_generic_constraint.value = Declaration_Routine_Generic_Constraint_Expression{ast_expression_clone(generic_constraint_expression)};
      }
      case .Interface: {
        generic_constraint_interface := generic_constraint.value.(Declaration_Routine_Generic_Constraint_Interface);
        generic_constraint_type := generic_constraint_interface.type;
        clone_generic_constraint.value = Declaration_Routine_Generic_Constraint_Interface{generic_constraint_interface.generic_type_name, ast_type_specification_clone(generic_constraint_type)};
      }
    }
    append_soa(&generic_constraints, clone_generic_constraint);
  }
  clone_value.generic_constraints = generic_constraints;

  clone_value.block = ast_statement_clone_block(original_value.block);

  return clone_value;
}

/**
* Clones the AST of a type specification.
*
* @param original The original type specification to clone.
* @return The cloned type specification.
*/
ast_type_specification_clone :: proc(original: ^Type_Specification) -> ^Type_Specification {
  if original == nil do return nil;

  switch original.kind {
    case .Name: {
      original_value := cast(^Type_Specification_Name) original;
      clone_value := ast_make_clone(original_value);
      clone_generic_types: [dynamic]^Type_Specification;
      for generic_type in original_value.generic_types {
        append(&clone_generic_types, ast_type_specification_clone(generic_type));
      }
      clone_value.generic_types = clone_generic_types;
      return clone_value;
    }
    case .Pointer: {
      original_value := cast(^Type_Specification_Pointer) original;
      clone_value := ast_make_clone(original_value);
      clone_value.base = ast_type_specification_clone(original_value.base);
      clone_value.relative_base = ast_type_specification_clone(original_value.relative_base);
      return clone_value;
    }
    case .Array: {
      original_value := cast(^Type_Specification_Array) original;
      clone_value := ast_make_clone(original_value);
      clone_value.base = ast_type_specification_clone(original_value.base);
      clone_value.size = ast_expression_clone(original_value.size);
      return clone_value;
    }
    case .Slice: {
      original_value := cast(^Type_Specification_Slice) original;
      clone_value := ast_make_clone(original_value);
      clone_value.base = ast_type_specification_clone(original_value.base);
      return clone_value;
    }
    case .Map: {
      original_value := cast(^Type_Specification_Map) original;
      clone_value := ast_make_clone(original_value);
      clone_value.key = ast_type_specification_clone(original_value.key);
      clone_value.value = ast_type_specification_clone(original_value.value);
      return clone_value;
    }
    case .Tuple: {
      original_value := cast(^Type_Specification_Tuple) original;
      clone_value := ast_make_clone(original_value);
      clone_elements: [dynamic]^Type_Specification;
      for element in original_value.elements {
        append(&clone_elements, ast_type_specification_clone(element));
      }
      clone_value.elements = clone_elements;
      return clone_value;
    }
    case .Procedure, .Function: {
      original_value := cast(^Type_Specification_Routine) original;
      clone_value := ast_make_clone(original_value);
      clone_value.return_type = ast_type_specification_clone(original_value.return_type);
      clone_parameters: [dynamic]^Type_Specification;
      for parameter in original_value.parameters {
        append(&clone_parameters, ast_type_specification_clone(parameter));
      }
      clone_value.parameters = clone_parameters;
      return clone_value;
    }

    case .None: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Clones the AST of a statement.
*
* @param original The original statement to clone.
* @return The cloned statement.
*/
ast_statement_clone :: proc(original: ^Statement) -> ^Statement {
  if original == nil do return nil;

  switch original.kind {
    case .Return: {
      original_value := cast(^Statement_Return) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }
    case .Break, .Continue, .Fallthrough: {
      original_value := cast(^Statement) original;
      clone_value := ast_make_clone(original_value);
      return clone_value;
    }
    case .Defer: {
      original_value := cast(^Statement_Defer) original;
      clone_value := ast_make_clone(original_value);
      clone_value.statement = ast_statement_clone(original_value.statement);
      return clone_value;
    }
    case .Push_Context: {
      original_value := cast(^Statement_Push_Context) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_value.block = ast_statement_clone_block(original_value.block);
      return clone_value;
    }

    case .Scope: {
      original_value := cast(^Statement_Scope) original;
      clone_value := ast_make_clone(original_value);
      clone_value.block = ast_statement_clone_block(original_value.block);
      return clone_value;
    }
    case .If, .Static_If: {
      original_value := cast(^Statement_If) original;
      clone_value := ast_make_clone(original_value);
      clone_value.condition = ast_expression_clone(original_value.condition);
      clone_value.then_block = ast_statement_clone_block(original_value.then_block);
      clone_else_ifs: #soa [dynamic]Statement_If_Else;
      for else_if in original_value.else_ifs {
        append_soa(&clone_else_ifs, Statement_If_Else{ast_expression_clone(else_if.condition), ast_statement_clone_block(else_if.block)});
      }
      clone_value.else_ifs = clone_else_ifs;
      clone_value.else_block = ast_statement_clone_block(original_value.else_block);
      return clone_value;
    }
    case .For: {
      original_value := cast(^Statement_For) original;
      clone_value := ast_make_clone(original_value);
      clone_value.initializer = ast_statement_clone(original_value.initializer);
      clone_value.condition = ast_expression_clone(original_value.condition);
      clone_value.next = ast_statement_clone(original_value.next);
      clone_value.block = ast_statement_clone_block(original_value.block);
      return clone_value;
    }
    case .Foreach: {
      original_value := cast(^Statement_Foreach) original;
      clone_value := ast_make_clone(original_value);
      clone_value.collection = ast_expression_clone(original_value.collection);
      clone_value.block = ast_statement_clone_block(original_value.block);
      return clone_value;
    }
    case .Switch: {
      original_value := cast(^Statement_Switch) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_cases: #soa [dynamic]Statement_Switch_Case;
      for switch_case in original_value.cases {
        patterns: [dynamic]^Expression;
        for pattern in switch_case.patterns {
          append(&patterns, ast_expression_clone(pattern));
        }
        append_soa(&clone_cases, Statement_Switch_Case{patterns, ast_statement_clone_block(switch_case.block)});
      }
      clone_value.cases = clone_cases;
      return clone_value;
    }

    case .Assign: {
      original_value := cast(^Statement_Assign) original;
      clone_value := ast_make_clone(original_value);
      clone_left_expressions: [dynamic]^Expression;
      for left_expression in original_value.left_expressions {
        append(&clone_left_expressions, ast_expression_clone(left_expression));
      } 
      clone_value.left_expressions = clone_left_expressions;
      clone_value.right_expression = ast_expression_clone(original_value.right_expression);
      return clone_value;
    }
    case .Initialize: {
      original_value := cast(^Statement_Initialize) original;
      clone_value := ast_make_clone(original_value);
      clone_names: [dynamic]string;
      for name in original_value.names {
        append(&clone_names, name);
      }
      clone_value.names = clone_names;
      clone_value.type = ast_type_specification_clone(original_value.type);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }
    case .Expression: {
      original_value := cast(^Statement_Expression) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }

    case .None: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Clones the AST of a statement block.
*
* @param original The original statement block to clone.
* @return The cloned statement block.
*/
ast_statement_clone_block :: proc(original: Statement_Block) -> Statement_Block {
  clone := original;
  clone_statements := make_dynamic_array_len_cap([dynamic]^Statement, 0, len(original.statements));
  for statement in original.statements {
    append(&clone_statements, ast_statement_clone(statement));
  }
  clone.statements = clone_statements;
  return clone;
}

/**
* Clones the AST of an expression.
*
* @param original The original expression to clone.
* @return The cloned expression.
*/
ast_expression_clone :: proc(original: ^Expression) -> ^Expression {
  if original == nil do return nil;

  switch original.kind {
    case .Parenthesized: {
      original_value := cast(^Expression_Parenthesized) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }

    case .Boolean, .Integer, .Float, .Character: {
      original_value := cast(^Expression_Literal) original;
      clone_value := ast_make_clone(original_value);
      return clone_value;
    }
    case .String: {
      original_value := cast(^Expression_Literal) original;
      clone_value := ast_make_clone(original_value);
      clone_value.value = strings.clone(original_value.value.(string));
      return clone_value;
    }
    case .Name: {
      original_value := cast(^Expression_Name) original;
      clone_value := ast_make_clone(original_value);
      clone_generic_types: [dynamic]^Type_Specification;
      for generic_type in original_value.generic_types {
        append(&clone_generic_types, ast_type_specification_clone(generic_type));
      } 
      clone_value.generic_types = clone_generic_types;
      return clone_value;
    }

    case .Cast: {
      original_value := cast(^Expression_Cast) original;
      clone_value := ast_make_clone(original_value);
      clone_value.type = ast_type_specification_clone(original_value.type);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }
    case .Call: {
      original_value := cast(^Expression_Call) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_arguments: [dynamic]^Expression;
      for argument in original_value.arguments {
        append(&clone_arguments, ast_expression_clone(argument));
      }
      clone_value.arguments = clone_arguments;
      return clone_value;
    }
    case .Index: {
      original_value := cast(^Expression_Index) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_value.index = ast_expression_clone(original_value.index);
      return clone_value;
    }
    case .Slice: {
      original_value := cast(^Expression_Slice) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_value.lower = ast_expression_clone(original_value.lower);
      clone_value.higher = ast_expression_clone(original_value.higher);
      return clone_value;
    }
    case .Member: {
      original_value := cast(^Expression_Member) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_generic_types: [dynamic]^Type_Specification;
      for generic_type in original_value.generic_types {
        append(&clone_generic_types, ast_type_specification_clone(generic_type));
      }
      clone_value.generic_types = clone_generic_types;
      return clone_value;
    }
    case .Compound: {
      original_value := cast(^Expression_Compound) original;
      clone_value := ast_make_clone(original_value);
      clone_value.type = ast_type_specification_clone(original_value.type);
      clone_fields: #soa [dynamic]Expression_Compound_Field;
      for field in original_value.fields {
        field_clone := field;
        field_clone.initializer = ast_expression_clone(field.initializer);
        if field.kind == .Index {
          field_clone.value = ast_expression_clone(field.value.(^Expression));
        }
        append_soa(&clone_fields, field_clone);
      }
      clone_value.fields = clone_fields;
      return clone_value;
    }
    case .Selector: {
      original_value := cast(^Expression_Selector) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      clone_value.type = ast_type_specification_clone(original_value.type);
      return clone_value;
    }
    case .Implicit_Selector: {
      original_value := cast(^Expression_Implicit_Selector) original;
      clone_value := ast_make_clone(original_value);
      return clone_value;
    }

    case .Unary: {
      original_value := cast(^Expression_Unary) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }
    case .Binary: {
      original_value := cast(^Expression_Binary) original;
      clone_value := ast_make_clone(original_value);
      clone_value.left = ast_expression_clone(original_value.left);
      clone_value.right = ast_expression_clone(original_value.right);
      return clone_value;
    }
    case .Ternary: {
      original_value := cast(^Expression_Ternary) original;
      clone_value := ast_make_clone(original_value);
      clone_value.condition = ast_expression_clone(original_value.condition);
      clone_value.then_expression = ast_expression_clone(original_value.then_expression);
      clone_value.else_expression = ast_expression_clone(original_value.else_expression);
      return clone_value;
    }
    case .Modify: {
      original_value := cast(^Expression_Modify) original;
      clone_value := ast_make_clone(original_value);
      clone_value.expression = ast_expression_clone(original_value.expression);
      return clone_value;
    }

    case .Query: {
      original_value := cast(^Expression_Query) original;
      clone_value := ast_make_clone(original_value);
      switch original_value.query_kind {
        case .Size_Of_Expression, .Typeid_Of_Expression, .Type_Info_Of_Expression: clone_value.value = ast_expression_clone(original_value.value.(^Expression));
        case .Size_Of_Type, .Typeid_Of_Type, .Type_Info_Of_Type: clone_value.value = ast_type_specification_clone(original_value.value.(^Type_Specification));
      }
      return clone_value;
    }
    case .Directive: {
      original_value := cast(^Expression_Directive) original;
      clone_value := ast_make_clone(original_value);
      return clone_value;
    }

    case .None: fallthrough;
    case: assert(false);
  }

  assert(false);
  return nil;
}

/**
* Clones an AST element.
* 
* @param original The original AST element to clone.
* @return The cloned AST element
*/
ast_make_clone :: proc(original: ^$T) -> ^T {
  clone := new(T);
  clone^ = original^;
  return clone;
}
