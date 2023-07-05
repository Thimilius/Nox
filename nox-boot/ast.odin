package nox

import "core:mem"
import "core:runtime"

/**
* Represents an access modifier for symbols.
*/ 
Access_Modifier :: enum u8 {
  None,                           // No specific access modifier.

  Private,                        // 'private' access modifier.
  Internal,                       // 'internal' access modifier.
  Public,                         // 'public' access modifier.

  Member_Unspecified,             // The default for members (fields, methods) when not explicitly specified.
  
  Declaration_Default = Internal, // The default for declarations when not explicitly specified.
}

/**
* Represents an SoA modifier.
*/
Layout_Modifier :: enum u8 {
  None,  // No modifier.
  SoA,   // SoA modifier.
  AoSoA, // AoSoA modifier.
}

/**
* Represents a calling convention for routines (Has to match with the 'Calling_Convention' enum defined in the 'runtime' package).
*/
Calling_Convention :: enum u8 {
  Nox,
  No_Context,
  C,
  Std_Call,
  Fast_Call,
  Win64,
}

/**
* Represents an argument for an attribute.
*/
Attribute_Argument :: struct {
  position: Source_Position, // The source position of the argument.
  name: string,              // The name of the argument (Can be emty).
  expression: ^Expression,   // The value expression of the argument.
}

/**
* Represents all possible attributes.
*/ 
Attribute_Kind :: enum {
  Invalid,      // No/Invalid attribute.

  // Can be applied to enumerations.
  Flags,        // The '@flags' attribute.

  // Can be applied to globals.
  Thread_Local, // The '@thread_local' attribute.

  // Can be applied to routines.
  Intrinsic,    // The '@intrinsic' attribute.
  Builtin,      // The '@builtin' attribute.
  Disabled,     // The '@disabled' attribute.
}

/**
* Holds the names corresponding to a token kind. 
*/
ATTRIBUTE_NAMES := [?]string{
  Attribute_Kind.Invalid = "invalid",
  Attribute_Kind.Flags = "flags",
  Attribute_Kind.Thread_Local = "thread_local",
  Attribute_Kind.Intrinsic = "intrinsic",
  Attribute_Kind.Builtin = "builtin",
  Attribute_Kind.Disabled = "disabled",
}

/**
* Represents an attribute that can be applied to a declaration.
*/
Attribute :: struct {
  kind: Attribute_Kind,                        // The kind of the attribute.
  position: Source_Position,                   // The soure position of the attribute.
  arguments: #soa [dynamic]Attribute_Argument, // All arguments of the attribute.
}

/**
* Represents a list of attributes.
*/
Attributes :: struct {
  attributes: #soa [dynamic]Attribute, // The attributes in the list.
}

/**
* Represents all possible declarations.
*/ 
Declaration_Kind :: enum u8 {
  None,           // No/Invalid declaration.

  Import,         // An 'import' declaration.

  Constant,       // A 'const' declaration.
  Global,         // A 'global' declaration.
  Enumeration,    // An 'enum' declaration.
  Struct,         // A 'struct' declaration.
  Union,          // A 'union' declaration.

  Procedure,      // A 'proc' declaration.
  Function,       // A 'func' declaration.

  Interface,      // An 'interface' declaration.
  Implementation, // An 'implement' declaration.

  Type_Alias,     // A 'type_alias' declaration.
  Type_Define,    // A 'type_define' declaration.

  Directive,      // A declaration directive marked by '#'.
}

/**
* Represents different flags for declarations.
*/
Declaration_Flag :: enum {
  Extern,                   // The declaration is marked 'extern'.

  Generic,                  // The declaration has generic type parameters (Currently Only applicable for structs, routines and type aliases).

  Routine_Has_Block,        // The routine has a block attached to it.
  Routine_Has_Params,       // The routine has a parameter marked 'params'.
  Routine_Has_C_Varargs,    // The routine has a '...' parameter.
  Routine_Is_Pure_Function, // The function is marked with the 'pure' keyword.
}

Declaration_Flags :: bit_set[Declaration_Flag]

/**
* Represents a declaration inside the AST.
*/
Declaration :: struct {
  kind: Declaration_Kind,           // The kind of the declaration.
  flags: Declaration_Flags,         // The flags of the declaration.
  access_modifier: Access_Modifier, // The access modifier of the declaration.
  position: Source_Position,        // The source position of the declaration.
  name: string,                     // The name of the declaration (Can be emty if not applicable).
  attributes: Attributes,           // The attributes of the declaration.
}

/**
* Represents an 'import' declaration.
*/
Declaration_Import :: struct {
  using decl: Declaration, // The base declaration info.
  alias: string,           // The alias of the import (Can be emty).
  import_in_scope: bool,   // Should the import be done in scope?
}

/**
* Represents a 'const' declaration.
*/
Declaration_Constant :: struct {
  using decl: Declaration,   // The base declaration info.
  type: ^Type_Specification, // The type of the constant (Can be nil).
  expression: ^Expression,   // The initializer expression of the constant.
}

/**
* Represents a 'global' declaration.
*/
Declaration_Global :: struct {
  using decl: Declaration,   // The base declaration info.
  type: ^Type_Specification, // The type of the global (Can be nil).
  expression: ^Expression,   // The initializer expression of the global (Can be nil).
}

/**
* Represents an item inside an 'enum' declaration.
*/
Declaration_Enumeration_Item :: struct {
  position: Source_Position, // The source position of the item.
  name: string,              // The name of the item.
  initializer: ^Expression,  // The initializer expression of the item (Can be nil).
}

/**
* Represents an 'enum' declaration.
*/
Declaration_Enumeration :: struct {
  using decl: Declaration,                           // The base declaration info.
  type: ^Type_Specification,                         // The base type of the enumeration (Can be nil).
  items: #soa [dynamic]Declaration_Enumeration_Item, // The items contained in the enumeration.
}

/**
* Represents a field inside a 'struct' declaration.
*/
Declaration_Struct_Field :: struct {
  position: Source_Position,        // The source position of the field.
  access_modifier: Access_Modifier, // The access modifier of the field.
  is_composite: bool,               // Is the field marked by 'composite'?
  name: string,                     // The name of the field.
  type: ^Type_Specification,        // The type of the field.
}

/**
* Represents a 'struct' declaration.
*/
Declaration_Struct :: struct {
  using decl: Declaration,                        // The base declaration info.
  fields: #soa [dynamic]Declaration_Struct_Field, // The fields contained in the struct.
  generic_type_names: [dynamic]string,            // The generic type names of the struct.
}

/**
* Represents a 'union' declaration.
*/
Declaration_Union :: struct {
  using decl: Declaration,             // The base declaration info.
  types: [dynamic]^Type_Specification, // The type variants of the union.
}

/**
* Represents a parameter for a routine declaration.
*/
Declaration_Routine_Parameter :: struct {
  position: Source_Position, // The source position of the parameter.
  name: string,              // The name of the parameter.
  type: ^Type_Specification, // The type of the parameter.
  initializer: ^Expression,  // The initializer expression of the parameter (Can be nil).
}

/**
* Represents the kind of a generic routine constraint.
*/
Declaration_Routine_Generic_Constraint_Kind :: enum {
  Expression, // The generic constraint is an expression.
  Interface,  // The generic constraint is an interface.
}

/**
* Represents a generic routine expression constraint.
*/
Declaration_Routine_Generic_Constraint_Expression :: struct {
  expression: ^Expression, // The expression of the constraint.
}

/**
* Represents a generic routine interface constraint.
*/
Declaration_Routine_Generic_Constraint_Interface :: struct {
  generic_type_name: string, // The generic type name the constraint refers to.
  type: ^Type_Specification, // The type of the constraint.
}

/**
* Represents a generic routine constraint value.
*/
Declaration_Routine_Generic_Constraint_Value :: union {
  Declaration_Routine_Generic_Constraint_Expression, // An expression constraint.
  Declaration_Routine_Generic_Constraint_Interface,  // An interface constraint.
}

/**
* Represents a constraint for a generic routine.
*/
Declaration_Routine_Generic_Constraint :: struct {
  position: Source_Position,                           // The source position of the constraint.
  kind: Declaration_Routine_Generic_Constraint_Kind,   // The kind of the constraint.
  value: Declaration_Routine_Generic_Constraint_Value, // The value of the constraint.
}

/**
* Represents a routine ('proc' or 'func') declaration.
*/
Declaration_Routine :: struct {
  using decl: Declaration,                                                   // The base declaration info.
  calling_convention: Calling_Convention,                                    // The calling convention of the routine.
  generic_type_names: [dynamic]string,                                       // The generic type names of the routine.
  parameters: #soa [dynamic]Declaration_Routine_Parameter,                   // The parameters of the routine.
  return_type: ^Type_Specification,                                          // The return type of the routine (Can be nil).
  generic_constraints: #soa [dynamic]Declaration_Routine_Generic_Constraint, // The generic constraints of the routine.
  block: Statement_Block,                                                    // The statement block of the routine.
}

/**
* Represents an 'interface' declaration.
*/
Declaration_Interface :: struct {
  using decl: Declaration,        // The base declaration info.
  methods: [dynamic]^Declaration, // The methods that are part of the interface.
}

/**
* Represents an 'implement' declaration.
*/
Declaration_Implementation :: struct {
  using decl: Declaration,                  // The base declaration info.
  interface_type: ^Type_Specification,      // The interface that gets implemented (Can be nil).
  implementation_type: ^Type_Specification, // The actual type the implementation is for.
  methods: [dynamic]^Declaration,           // The methods that are part of the implementation.
}

/**
* Represents a 'type_alias' declaration.
*/
Declaration_Type_Alias :: struct {
  using decl: Declaration,             // The base declaration info.
  type: ^Type_Specification,           // The type to alias.
  generic_type_names: [dynamic]string, // The generic type names of the type alias.
}

/**
* Represents a 'type_define' declaration.
*/
Declaration_Type_Define :: struct {
  using decl: Declaration,   // The base declaration info.
  type: ^Type_Specification, // The underlying type of the newly defined type.
}

/**
* Represents the kind of a declaration directive.
*/
Declaration_Directive_Kind :: enum {
  Assert,         // An 'assert' directive.
  If,             // An 'if' directive.
  Expand_Context, // An 'expand_context' directive.
}

/**
* Represents a declaration directive marked by '#'.
*/
Declaration_Directive :: struct {
  using decl: Declaration,                    // The base declaration info.
  directive_kind: Declaration_Directive_Kind, // The kind of the directive.
}

/**
* Represents an 'assert' declaration directive.
*/
Declaration_Directive_Assert :: struct {
  using directive: Declaration_Directive, // The base declaration directive info.
  expression: ^Expression,                // The expression of the directive.
}

/**
* Represents an 'else if' block for an 'if' declaration directive.
*/
Declaration_Directive_If_Else :: struct {
  condition: ^Expression,              // The condition expression of the block.
  declarations: [dynamic]^Declaration, // The declarations inside the block.
}

/**
* Represents an 'if' declaration directive.
*/
Declaration_Directive_If :: struct {
  using directive: Declaration_Directive,                // The base declaration directive info.
  condition: ^Expression,                                // The condition expression of the directive.
  then_declarations: [dynamic]^Declaration,              // The declarations inside the 'if' block of the directive.
  else_ifs: #soa [dynamic]Declaration_Directive_If_Else, // The else if blocks of the directive.
  else_declarations: [dynamic]^Declaration,              // The declarations inside the 'else' block of the directive.
}

/**
* Represents an 'expand_context' declaration directive.
*/
Declaration_Directive_Expand_Context :: struct {
  using directive: Declaration_Directive, // The base declaration directive info.
  field_name: string,                     // The name of the field.
  field_type: ^Type_Specification,        // The type of the field.
}

/**
* Makes a new declaration of a given type.
* 
* @param T               The type of the declaration.
* @param kind            The kind of the declaration.
* @param position        The position of the declaration.
* @param access_modifier The access modifier of the declaration.
* @param name            The name of the declaration.
* @param flags           The flags of the declaration.
* @return The new declaration.
*/
ast_declaration_make :: proc(
  $T: typeid,
  kind: Declaration_Kind,
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  flags: Declaration_Flags,
) -> ^T {
  d := new(T);
  d.kind = kind;
  d.position = position;
  d.name = name;
  d.access_modifier = access_modifier;
  d.flags = flags;
  return d;
}

/**
* Makes a new import declaration.
* 
* @param position        The position of the import.
* @param name            The name of the import.
* @param flags           The flags of the import.
* @param alias           The alias of the import.
* @param import_in_scope Whether or not the import is done in scope.
* @return The new import declaration.
*/
ast_declaration_make_import :: proc(position: Source_Position, name: string, flags: Declaration_Flags, alias: string, import_in_scope: bool) -> ^Declaration {
  d := ast_declaration_make(Declaration_Import, .Import, position, .None, name, flags);
  d.alias = alias;
  d.import_in_scope = import_in_scope;
  return d;
}

/**
* Makes a new constant declaration.
* 
* @param position        The position of the constant.
* @param access_modifier The access modifier of the constant.
* @param name            The name of the constant.
* @param type            The type of the constant.
* @param expression      The expression of the constant.
* @return The new constant declaration.
*/
ast_declaration_make_constant :: proc(
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  type: ^Type_Specification,
  expression: ^Expression,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Constant, .Constant, position, access_modifier, name, {});
  d.type = type;
  d.expression = expression;
  return d;
}

/**
* Makes a new global declaration.
* 
* @param position        The position of the global.
* @param access_modifier The access modifier of the global.
* @param name            The name of the global.
* @param flags           The flags of the global.
* @param type            The type of the global.
* @param expression      The expression of the global.
* @return The new global declaration.
*/
ast_declaration_make_global :: proc(
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  flags: Declaration_Flags,
  type: ^Type_Specification,
  expression: ^Expression,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Global, .Global, position, access_modifier, name, flags);
  d.type = type;
  d.expression = expression;
  return d;
}

/**
* Makes a new enumeration declaration.
* 
* @param position        The position of the enumeration.
* @param access_modifier The access modifier of the enumeration.
* @param name            The name of the enumeration.
* @param type            The type of the enumeration.
* @param items           The items of the enumeration.
* @return The new enumeration declaration.
*/
ast_declaration_make_enumeration :: proc(
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  type: ^Type_Specification,
  items: #soa [dynamic]Declaration_Enumeration_Item,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Enumeration, .Enumeration, position, access_modifier, name, {});
  d.type = type;
  d.items = items;
  return d;
}

/**
* Makes a new struct declaration.
* 
* @param position           The position of the struct.
* @param access_modifier    The access modifier of the struct.
* @param name               The name of the struct.
* @param flags              The flags of the struct.
* @param fields             The fields of the struct.
* @param generic_type_names The generic type names of the struct.
* @return The new struct declaration.
*/
ast_declaration_make_struct :: proc(
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  flags: Declaration_Flags,
  fields: #soa [dynamic]Declaration_Struct_Field,
  generic_type_names: [dynamic]string,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Struct, .Struct, position, access_modifier, name, flags);
  d.fields = fields;
  d.generic_type_names = generic_type_names;
  return d;
}

/**
* Makes a new union declaration.
* 
* @param position        The position of the union.
* @param access_modifier The access modifier of the union.
* @param name            The name of the union.
* @param types           The types of the union.
* @return The new union declaration.
*/
ast_declaration_make_union :: proc(position: Source_Position, access_modifier: Access_Modifier, name: string, types: [dynamic]^Type_Specification) -> ^Declaration {
  d := ast_declaration_make(Declaration_Union, .Union, position, access_modifier, name, {});
  d.types = types;
  return d;
}

/**
* Makes a new routine declaration.
* 
* @param kind                The kind of the routine.
* @param position            The position of the routine.
* @param access_modifier     The access modifier of the routine.
* @param name                The name of the routine.
* @param flags               The flags of the routine.
* @param calling_convetion   The calling convention of the routine.
* @param generic_type_names  The generic type names of the routine.
* @param parameters          The parameters of the routine.
* @param return_type         The return type of the routine.
* @param generic_constraints The generic constraints of the routine.
* @param block               The block of the routine.
* @return The new routine declaration.
*/
ast_declaration_make_routine :: proc(
  kind: Declaration_Kind,
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  flags: Declaration_Flags,
  calling_convention: Calling_Convention,
  generic_type_names: [dynamic]string,
  parameters: #soa [dynamic]Declaration_Routine_Parameter,
  return_type: ^Type_Specification,
  generic_constraints: #soa [dynamic]Declaration_Routine_Generic_Constraint,
  block: Statement_Block,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Routine, kind, position, access_modifier, name, flags);
  d.calling_convention = calling_convention;
  d.generic_type_names = generic_type_names;
  d.parameters = parameters;
  d.return_type = return_type;
  d.generic_constraints = generic_constraints;
  d.block = block;
  return d;
}

/**
* Makes a new interface declaration.
* 
* @param position        The position of the interface.
* @param access_modifier The access modifier of the interface.
* @param name            The name of the interface.
* @param methods         The methods of the interface.
* @return The new interface declaration.
*/
ast_declaration_make_interface :: proc(position: Source_Position, access_modifier: Access_Modifier, name: string, methods: [dynamic]^Declaration) -> ^Declaration {
  d := ast_declaration_make(Declaration_Interface, .Interface, position, access_modifier, name, {});
  d.methods = methods;
  return d;
}

/**
* Makes a new implementation declaration.
* 
* @param position            The position of the implementation.
* @param interface_type      The interface type of the implementation.
* @param implementation_type The implementation type of the implementation.
* @param methods             The methods of the implementation.
* @return The new implementation declaration.
*/
ast_declaration_make_implementation :: proc(
  position: Source_Position,
  interface_type: ^Type_Specification,
  implementation_type: ^Type_Specification,
  methods: [dynamic]^Declaration,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Implementation, .Implementation, position, .None, "", {});
  d.interface_type = interface_type;
  d.implementation_type = implementation_type;
  d.methods = methods;
  return d;
}

/**
* Makes a new type alias declaration.
* 
* @param position           The position of the type alias.
* @param access_modifier    The access modifier of the type alias.
* @param name               The name of the type alias.
* @param flags              The flags of the type alias.
* @param type               The type of the type alias.
* @param generic_type_names The generic type names of the type alias.
* @return The new type alias declaration.
*/
ast_declaration_make_type_alias :: proc(
  position: Source_Position,
  access_modifier: Access_Modifier,
  name: string,
  flags: Declaration_Flags,
  type: ^Type_Specification,
  generic_type_names: [dynamic]string,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Type_Alias, .Type_Alias, position, access_modifier, name, flags);
  d.type = type;
  d.generic_type_names = generic_type_names;
  return d;
}

/**
* Makes a new type define declaration.
* 
* @param position        The position of the type define.
* @param access_modifier The access modifier of the type define.
* @param name            The name of the type define.
* @param type            The type of the type define.
* @return The new type define declaration.
*/
ast_declaration_make_type_define :: proc(position: Source_Position, access_modifier: Access_Modifier, name: string, type: ^Type_Specification) -> ^Declaration {
  d := ast_declaration_make(Declaration_Type_Define, .Type_Define, position, access_modifier, name, {});
  d.type = type;
  return d;
}

/**
* Makes a new assert directive declaration.
* 
* @param position   The position of the assert directive.
* @param expression The expression of the assert directive.
* @return The new assert directive declaration.
*/
ast_declaration_make_directive_assert :: proc(position: Source_Position, expression: ^Expression) -> ^Declaration {
  d := ast_declaration_make(Declaration_Directive_Assert, .Directive, position, .None, "", {});
  d.directive_kind = .Assert;
  d.expression = expression;
  return d;
}

/**
* Makes a new if directive declaration.
* 
* @param position          The position of the if directive.
* @param condition         The condition of the if directive.
* @param then_declarations The then declarations of the if directive.
* @param else_ifs          The else ifs of the if directive.
* @param else_declarations The else declarations of the if directive.
* @return The new if directive declaration.
*/
ast_declaration_make_directive_if :: proc(
  position: Source_Position,
  condition: ^Expression,
  then_declarations: [dynamic]^Declaration,
  else_ifs: #soa [dynamic] Declaration_Directive_If_Else,
  else_declarations: [dynamic]^Declaration,
) -> ^Declaration {
  d := ast_declaration_make(Declaration_Directive_If, .Directive, position, .None, "", {});
  d.directive_kind = .If;
  d.condition = condition;
  d.then_declarations = then_declarations;
  d.else_ifs = else_ifs;
  d.else_declarations = else_declarations;
  return d;
}

/**
* Makes a new expand context directive declaration.
* 
* @param position   The position of the expand context directive.
* @param field_name The field name of the expand context directive.
* @param field_type The field type of the expand context directive.
* @return The new expand context directive declaration.
*/
ast_declaration_make_directive_expand_context :: proc(position: Source_Position, field_name: string, field_type: ^Type_Specification) -> ^Declaration {
  d := ast_declaration_make(Declaration_Directive_Expand_Context, .Directive, position, .None, "", {});
  d.directive_kind = .Expand_Context;
  d.field_name = field_name;
  d.field_type = field_type;
  return d;
}

/**
* Represents all possible type specifications.
*/
Type_Specification_Kind :: enum {
  None,      // No/Invalid type.

  Name,      // A named type.
  Pointer,   // A pointer type (absolute, self-relative, offset-relative or dynamic).
  Array,     // An array type (fixed or dynamic).
  Slice,     // A slice type.
  Map,       // A 'map' type.
  Tuple,     // A tuple type.
  Procedure, // A 'proc' type.
  Function,  // A 'func' type.
}

/**
* Represents a type specification inside the AST.
*/ 
Type_Specification :: struct {
  kind: Type_Specification_Kind, // The kind of the type specification.
  position: Source_Position,     // The source position of the type specification.
}

/**
* Represents a named type.
*/
Type_Specification_Name :: struct {
  using spec: Type_Specification,              // The base type specification info.
  package_name: string,                        // The package name the type is contained in (Can be emty).
  name: string,                                // The name of the type.
  generic_types: [dynamic]^Type_Specification, // The generic type parameters of the type.

  overwrite_type: ^Type,                       // The overwrite type to be used in generic contexts (Can be nil).
}

/**
* Represents the kind of a pointer type.
*/
Type_Specification_Pointer_Kind :: enum {
  Absolute,        // An absolute pointer (*).
  Self_Relative,   // A self-relative pointer (~*).
  Offset_Relative, // An offset-relative pointer (^*).
  SoA_Layout,      // An SoA layout pointer (soa*).
  AoSoA_Layout,    // An AoSoA layout pointer (aosoa*).
  Dynamic,         // A dynamic pointer (dynamic*).
}

/**
* Represents a pointer type (absolute, self-relative, offset-relative or dynamic).
*/
Type_Specification_Pointer :: struct {
  using spec: Type_Specification,                // The base type specification info.
  pointer_kind: Type_Specification_Pointer_Kind, // The kind of the pointer.
  base: ^Type_Specification,                     // The base type of the pointer.
  relative_base: ^Type_Specification,            // The relative base of the pointer (Only applicable for relative pointers an Can be nil).
}

/**
* Represents the kind of an array type.
*/
Type_Specification_Array_Kind :: enum {
  Fixed,   // A fixed array.
  Dynamic, // An dynamic array.
}

/**
* Represents an array type (fixed or dynamic).
*/
Type_Specification_Array :: struct {
  using spec: Type_Specification,            // The base type specification info.
  array_kind: Type_Specification_Array_Kind, // The kind of the array.
  layout_modifier: Layout_Modifier,          // The layout modifier of the array.
  base: ^Type_Specification,                 // The base type of the array.
  size: ^Expression,                         // The size expression of the array (Only applicable for fixed arrays and Can be nil).
}

/**
* Represents a slice type.
*/
Type_Specification_Slice :: struct {
  using spec: Type_Specification,   // The base type specification info.
  layout_modifier: Layout_Modifier, // The layout modifier of the slice.
  base: ^Type_Specification,        // The base type of the slice.
}

/**
* Represents a 'map' type.
*/
Type_Specification_Map :: struct {
  using spec: Type_Specification, // The base type specification info.
  key: ^Type_Specification,       // The key type of the map.
  value: ^Type_Specification,     // The value type of the map.
}

/**
* Represents a tuple type.
*/
Type_Specification_Tuple :: struct {
  using spec: Type_Specification,         // The base type specification info.
  elements: [dynamic]^Type_Specification, // The elements of the tuple.
}

/**
* Represents a routine ('proc' or 'func') type.
*/
Type_Specification_Routine :: struct {
  using spec: Type_Specification,           // The base type specification info.
  calling_convention: Calling_Convention,   // The calling convention of the routine.
  parameters: [dynamic]^Type_Specification, // The parameters of the routine.
  return_type: ^Type_Specification,         // The return type of the routine.
  has_params: bool,                         // Does the routine have an 'params' parameter?
  has_c_varargs: bool,                      // Does the routine have an '...' parameter?
  is_pure: bool,                            // Is the routine a pure function?
}

/**
* Makes a new type specification of a given type.
* 
* @param T        The type of the type specification.
* @param kind     The kind of the type specification.
* @param position The position of the type specification.
* @return The new type specification.
*/
ast_type_specification_make :: proc($T: typeid, kind: Type_Specification_Kind, position: Source_Position) -> ^T {
  t := new(T);
  t.kind = kind;
  t.position = position;
  return t;
}

/**
* Makes a new name type specification.
* 
* @param position      The position of the name.
* @param name          The name of the name.
* @param generic_types The generic types of the name.
* @return The new name type specification.
*/
ast_type_specification_make_name :: proc(
  position: Source_Position,
  package_name: string,
  name: string,
  generic_types: [dynamic]^Type_Specification,
) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Name, .Name, position);
  t.package_name = package_name;
  t.name = name;
  t.generic_types = generic_types;
  return t;
}

/**
* Makes a new pointer type specification.
* 
* @param position      The position of the pointer.
* @param pointer_kind  The pointer kind of the pointer.
* @param base          The base of the pointer.
* @param relative_base The relative base of the pointer.
* @return The new pointer type specification.
*/
ast_type_specification_make_pointer :: proc(
  position: Source_Position,
  pointer_kind: Type_Specification_Pointer_Kind,
  base: ^Type_Specification,
  relative_base: ^Type_Specification,
) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Pointer, .Pointer, position);
  t.pointer_kind = pointer_kind;
  t.base = base;
  t.relative_base = relative_base;
  return t;
}

/**
* Makes a new array type specification.
* 
* @param position        The position of the array.
* @param array_kind      The array kind of the array.
* @param layout_modifier The SoA modifier of the array.
* @param base            The base of the array.
* @param size            The size of the array.
* @return The new array type specification.
*/
ast_type_specification_make_array :: proc(
  position: Source_Position,
  array_kind: Type_Specification_Array_Kind,
  layout_modifier: Layout_Modifier,
  base: ^Type_Specification,
  size: ^Expression,
) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Array, .Array, position);
  t.array_kind = array_kind;
  t.layout_modifier = layout_modifier;
  t.base = base;
  t.size = size;
  return t;
}

/**
* Makes a new slice type specification.
* 
* @param position        The position of the slice.
* @param layout_modifier The SoA modifier of the slice.
* @param base            The base of the slice.
* @return The new slice type specification.
*/
ast_type_specification_make_slice :: proc(position: Source_Position, layout_modifier: Layout_Modifier, base: ^Type_Specification) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Slice, .Slice, position);
  t.layout_modifier = layout_modifier;
  t.base = base;
  return t;
}

/**
* Makes a new map type specification.
* 
* @param position     The position of the map.
* @param key          The key of the map.
* @param value        The value of the map.
* @return The new map type specification.
*/
ast_type_specification_make_map :: proc(position: Source_Position, key: ^Type_Specification, value: ^Type_Specification) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Map, .Map, position);
  t.key = key;
  t.value = value;
  return t;
}

/**
* Makes a new tuple type specification.
* 
* @param position     The position of the tuple.
* @param elements     The elements of the tuple.
* @return The new tuple type specification.
*/
ast_type_specification_make_tuple :: proc(position: Source_Position, elements: [dynamic]^Type_Specification) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Tuple, .Tuple, position);
  t.elements = elements;
  return t;
}

/**
* Makes a new routine type specification.
* 
* @param kind              The kind of the routine.
* @param position          The position of the routine.
* @param calling_convetion The calling_convetion of the routine.
* @param parameters        The parameters of the routine.
* @param has_params        Whether or not the routine has params.
* @param has_c_varargs     Whether or not the routine has c varargs.
* @param return_type       The return_type of the routine.
* @param is_pure           Whether or not the routine is pure.
* @return The new routine type specification.
*/
ast_type_specification_make_routine :: proc(
  kind: Type_Specification_Kind,
  position: Source_Position,
  calling_convention: Calling_Convention,
  parameters: [dynamic]^Type_Specification,
  has_params: bool,
  has_c_varargs: bool,
  return_type: ^Type_Specification,
  is_pure: bool,
) -> ^Type_Specification {
  t := ast_type_specification_make(Type_Specification_Routine, kind, position);
  t.calling_convention = calling_convention;
  t.parameters = parameters;
  t.return_type = return_type;
  t.has_params = has_params;
  t.has_c_varargs = has_c_varargs;
  t.is_pure = is_pure;
  return t;
}

/**
* Represents all possible statements.
*/
Statement_Kind :: enum {
  None,         // No/Invalid statement.

  Return,       // A 'return' statement.
  Break,        // A 'break' statement.
  Continue,     // A 'continue' statement.
  Fallthrough,  // A 'fallthrough' statement.
  Defer,        // A 'defer' statement.
  Push_Context, // A 'push_context' statement.

  Scope,        // A scope '{}' statement.
  If,           // An 'if' statement.
  Static_If,    // An '#if' statement.
  For,          // A 'for' statement.
  Foreach,      // A 'foreach' statement.
  Switch,       // A switch statement.

  Assign,       // An assign statement.
  Initialize,   // An initialize statement.
  Expression,   // An expression statement.
}

/**
* Represents a statement inside the AST.
*/
Statement :: struct {
  kind: Statement_Kind,      // The kind of the statement.
  position: Source_Position, // The source position of the statement.
}

/**
* Represents a simple block '{}'.
*/
Statement_Block :: struct {
  start_position: Source_Position, // The start source position of the block.
  end_position: Source_Position,   // The end source position of the block.
  statements: [dynamic]^Statement, // The statements inside the block.
}

/**
* Represents a 'return' statement.
*/
Statement_Return :: struct {
  using stmt: Statement,   // The base statement info.
  expression: ^Expression, // The return expression (Can be nil).
}

/**
* Represents a 'defer' statement.
*/
Statement_Defer :: struct {
  using stmt: Statement, // The base statement info.
  statement: ^Statement, // The statement to defer.
}

/**
* Represents a 'push context' statement.
*/
Statement_Push_Context :: struct {
  using stmt: Statement,   // The base statement info.
  expression: ^Expression, // The context expression to push.
  block: Statement_Block,  // The statement block of the pushed context.
}

/**
* Represents a scope '{}' statement.
*/
Statement_Scope :: struct {
  using stmt: Statement,  // The base statement info.
  block: Statement_Block, // The block of the scope.
}

/**
* Represents an 'if else' block inside an 'if' or '#if' statement.
*/
Statement_If_Else :: struct {
  condition: ^Expression, // The condition of the block.
  block: Statement_Block, // The statement block of the block.
}

/**
* Represents an 'if' or '#if' statement.
*/
Statement_If :: struct {
  using stmt: Statement,                     // The base statement info.
  condition: ^Expression,                    // The condition of the statement.
  then_block: Statement_Block,               // The statement block of the 'if' block.
  else_ifs: #soa [dynamic]Statement_If_Else, // The 'else if' blocks of the statement.
  else_block: Statement_Block,               // The statement block of the 'else' block.
}

/**
* Represents a 'for' statement.
*/
Statement_For :: struct {
  using stmt: Statement,   // The base statement info.
  initializer: ^Statement, // The initialize statement of the loop (Can be nil).
  condition: ^Expression,  // The condition expression of the loop (Can be nil).
  next: ^Statement,        // The next statement of the loop (Can be nil).
  block: Statement_Block,  // The statement block of the loop.
}

/**
* Represents a 'foreach' statement.
*/
Statement_Foreach :: struct {
  using stmt: Statement,   // The base statement info.
  element_name: string,    // The name of the element variable.
  index_name: string,      // The name of the index variable (Can be empty).
  collection: ^Expression, // The collection expression to iterate over.
  block: Statement_Block,  // The statement block of the loop.
}

/**
* Represents a case block inside a 'switch' statement.
*/
Statement_Switch_Case :: struct {
  patterns: [dynamic]^Expression, // The pattern expressions of the case block.
  block: Statement_Block,         // The statement block of the case block.
}

/**
* Represents a 'switch' statement.
*/
Statement_Switch :: struct {
  using stmt: Statement,                      // The base statement info.
  expression: ^Expression,                    // The expression to switch on.
  cases: #soa [dynamic]Statement_Switch_Case, // The cases of the statement.
}

/**
* Represents an assign statement.
*/
Statement_Assign :: struct {
  using stmt: Statement,                  // The base statement info.
  operator: Token_Kind,                   // The operator of the assignment.
  left_expressions: [dynamic]^Expression, // The left-hand side expressions of the assignment.
  right_expression: ^Expression,          // The right-hand side expresion of the assignment.
}

/**
* Represents an initialize statement.
*/
Statement_Initialize :: struct {
  using stmt: Statement,     // The base statement info.
  names: [dynamic]string,    // The names of the variables.
  type: ^Type_Specification, // The type of the statement (Can be nil).
  expression: ^Expression,   // The initialize expression of the statement.
}

/**
* Represents an expression statement.
*/
Statement_Expression :: struct {
  using stmt: Statement,   // The base statement info.
  expression: ^Expression, // The expression of the statement.
}

/**
* Makes a new statement of a given type.
* 
* @param T        The type of the statement.
* @param kind     The kind of the statement.
* @param position The position of the statement.
* @return The new statement.
*/
ast_statement_make :: proc($T: typeid, kind: Statement_Kind, position: Source_Position) -> ^T {
  s := new(T);
  s.kind = kind;
  s.position = position;
  return s;
}

/**
* Makes a new return statement.
* 
* @param position   The position of the return statement.
* @param expression The expression of the return statement.
* @return The new return statement.
*/
ast_statement_make_return :: proc(position: Source_Position, expression: ^Expression) -> ^Statement {
  s := ast_statement_make(Statement_Return, .Return, position);
  s.expression = expression;
  return s;
}

/**
* Makes a new break statement.
* 
* @param position The position of the break statement.
* @return The new break statement.
*/
ast_statement_make_break :: proc(position: Source_Position) -> ^Statement {
  return ast_statement_make(Statement, .Break, position);
}

/**
* Makes a new continue statement.
* 
* @param position The position of the continue statement.
* @return The new continue statement.
*/
ast_statement_make_continue :: proc(position: Source_Position) -> ^Statement {
  return ast_statement_make(Statement, .Continue, position);
}

/**
* Makes a new fallthrough statement.
* 
* @param position The position of the fallthrough statement.
* @return The new fallthrough statement.
*/
ast_statement_make_fallthrough :: proc(position: Source_Position) -> ^Statement {
  return ast_statement_make(Statement, .Fallthrough, position);
}

/**
* Makes a new defer statement.
* 
* @param position  The position of the defer statement.
* @param statement The statement of the defer statement.
* @return The new defer statement.
*/
ast_statement_make_defer :: proc(position: Source_Position, statement: ^Statement) -> ^Statement {
  s := ast_statement_make(Statement_Defer, .Defer, position);
  s.statement = statement;
  return s;
}

/**
* Makes a new push context statement.
* 
* @param position   The position of the push context statement.
* @param expression The expression of the push context statement.
* @param block      The block of the push context statement.
* @return The new push context statement.
*/
ast_statement_make_push_context :: proc(position: Source_Position, expression: ^Expression, block: Statement_Block) -> ^Statement {
  s := ast_statement_make(Statement_Push_Context, .Push_Context, position);
  s.expression = expression;
  s.block = block;
  return s;
}

/**
* Makes a new scope statement.
* 
* @param position The position of the scope statement.
* @param block    The block of the scope statement.
* @return The new scope statement.
*/
ast_statement_make_scope :: proc(position: Source_Position, block: Statement_Block) -> ^Statement {
  s := ast_statement_make(Statement_Scope, .Scope, position);
  s.block = block;
  return s;
}

/**
* Makes a new if statement.
* 
* @param position   The position of the if statement.
* @param kind       The kind of the if statement.
* @param condition  The condition of the if statement.
* @param then_block The then block of the if statement.
* @param else_ifs   The else ifs of the if statement.
* @param else_block The else block of the if statement.
* @return The new if statement.
*/
ast_statement_make_if :: proc(
  position: Source_Position,
  kind: Statement_Kind,
  condition: ^Expression,
  then_block: Statement_Block,
  else_ifs: #soa [dynamic]Statement_If_Else,
  else_block: Statement_Block,
) -> ^Statement {
  s := ast_statement_make(Statement_If, kind, position);
  s.condition = condition;
  s.then_block = then_block;
  s.else_ifs = else_ifs;
  s.else_block = else_block;
  return s;
}

/**
* Makes a new for statement.
* 
* @param position    The position of the for statement.
* @param initializer The initializer statement of the for statement.
* @param condition   The conidtion of the for statement.
* @param next        The next statement of the for statement.
* @param block       The block of the for statement.
* @return The new for statement.
*/
ast_statement_make_for :: proc(position: Source_Position, initializer: ^Statement, condition: ^Expression, next: ^Statement, block: Statement_Block) -> ^Statement {
  s := ast_statement_make(Statement_For, .For, position);
  s.initializer = initializer;
  s.condition = condition;
  s.next = next;
  s.block = block;
  return s;
}

/**
* Makes a new foreach statement.
* 
* @param position     The position of the foreach statement.
* @param element_name The element name of the foreach statement.
* @param index_name   The index name of the foreach statement.
* @param collection   The collection of the foreach statement.
* @param block        The block of the foreach statement.
* @return The new foreach statement.
*/
ast_statement_make_foreach :: proc(
  position: Source_Position,
  element_name: string,
  index_name: string,
  collection: ^Expression,
  block: Statement_Block) -> ^Statement {
  s := ast_statement_make(Statement_Foreach, .Foreach, position);
  s.element_name = element_name;
  s.index_name = index_name;
  s.collection = collection;
  s.block = block;
  return s;
}

/**
* Makes a new switch statement.
* 
* @param position     The position of the switch statement.
* @param expressions  The expression of the switch statement.
* @param cases        The cases of the switch statement.
* @return The new switch statement.
*/
ast_statement_make_switch :: proc(position: Source_Position, expression: ^Expression, cases: #soa [dynamic]Statement_Switch_Case) -> ^Statement {
  s := ast_statement_make(Statement_Switch, .Switch, position);
  s.expression = expression;
  s.cases = cases;
  return s;
}

/**
* Makes a new assign statement.
* 
* @param position         The position of the assign statement.
* @param operator         The operator of the assign statement.
* @param left_expression  The left expression of the assign statement.
* @param right_expression The right expression of the assign statement.
* @return The new assign statement.
*/
ast_statement_make_assign :: proc(
  position: Source_Position,
  operator: Token_Kind,
  left_expressions: [dynamic]^Expression,
  right_expression: ^Expression,
) -> ^Statement {
  s := ast_statement_make(Statement_Assign, .Assign, position);
  s.operator = operator;
  s.left_expressions = left_expressions;
  s.right_expression = right_expression;
  return s;
}

/**
* Makes a new initialize statement.
* 
* @param position   The position of the initialize statement.
* @param names      The names of the initialize statement.
* @param type       The type of the initialize statement.
* @param expression The expression of the initialize statement.
* @return The new initialize statement.
*/
ast_statement_make_initialize :: proc(position: Source_Position, names: [dynamic]string, type: ^Type_Specification, expression: ^Expression) -> ^Statement {
  s := ast_statement_make(Statement_Initialize, .Initialize, position);
  s.names = names;
  s.type = type;
  s.expression = expression;
  return s;
}

/**
* Makes a new expression statement.
* 
* @param position   The position of the expression statement.
* @param expression The expression of the expression statement.
* @return The new expression statement.
*/
ast_statement_make_expression :: proc(position: Source_Position, expression: ^Expression) -> ^Statement {
  s := ast_statement_make(Statement_Expression, .Expression, position);
  s.expression = expression;
  return s;
}

/**
* Represents all possible expressions.
*/
Expression_Kind :: enum {
  None,              // No/Invalid expression.

  Parenthesized,     // A parenthesized expression ((foo)).

  Boolean,           // A boolean literal expression (true).
  Integer,           // An integer literal expression (123).
  Float,             // A float literal expression (12.3).
  Character,         // A character literal expression ('f').
  String,            // A string literal expression ("foo").
  Name,              // A name expression (foo).

  Cast,              // A cast expression (cast(Foo) bar).
  Call,              // A call expression (foo(bar)).
  Index,             // An index expression (foo[index]).
  Slice,             // A slice expression (foo[lower:higher]).
  Member,            // A member expression (foo.bar).
  Compound,          // A compound expression ({Foo}).
  Selector,          // A selector expression (foo.(Bar)).
  Implicit_Selector, // An implicit selector expression (.Foo).

  Unary,             // A unary expression (-foo).
  Binary,            // A binary expression (foo + bar).
  Ternary,           // A ternary expression (foo ? bar : baz).
  Modify,            // A modify expression (foo++).

  Query,             // A query expression (size_of(foo)).
  Directive,         // A directive expression (#foo).
}

/**
* Represents an expression inside the AST.
*/
Expression :: struct {
  kind: Expression_Kind,     // The kind of the expression.
  position: Source_Position, // The source position of the expression.
}

/**
* Represents a parenthesized expression.
*/
Expression_Parenthesized :: struct {
  using expr: Expression,  // The base expression info.
  expression: ^Expression, // The parenthesized expression.
}

/**
* An expression literal value.
*/
Expression_Literal_Value :: union {
  bool,
  u64,
  f64,
  rune,
  string,
}

/**
* Represents a literal expression (booleans, integers, floats, characters and strings).
*/
Expression_Literal :: struct {
  using expr: Expression,          // The base expression info.
  value: Expression_Literal_Value, // The value of the expression.
}

/**
* Represents a name expression.
*/
Expression_Name :: struct {
  using expr: Expression,                      // The base expression info.
  name: string,                                // The name of the expression.
  generic_types: [dynamic]^Type_Specification, // The generic type parameters of the expression.
}

/**
* Represents a cast expression.
*/
Expression_Cast :: struct {
  using expr: Expression,    // The base expression info.
  type: ^Type_Specification, // The type of the cast expression.
  expression: ^Expression,   // The expression to cast.
}

/**
* Represents a call expression.
*/
Expression_Call :: struct {
  using expr: Expression,          // The base expression info.
  expression: ^Expression,         // The expression to call.
  arguments: [dynamic]^Expression, // The arguments of the call expression.
}

/**
* Represents an index expression.
*/
Expression_Index :: struct {
  using expr: Expression,  // The base expression info.
  expression: ^Expression, // The expression to index.
  index: ^Expression,      // The index expression.
}

/**
* Represents a slice expression.
*/
Expression_Slice :: struct {
  using expr: Expression,  // The base expression info.
  expression: ^Expression, // The expression to slice.
  lower: ^Expression,      // The lower slice expression (Can be nil).
  higher: ^Expression,     // The higher slice expression (Can be nil).
}

/**
* Represents a member expression.
*/
Expression_Member :: struct {
  using expr: Expression,                      // The base expression info.
  expression: ^Expression,                     // The expression to access the member of.
  name: string,                                // The name of the member.
  generic_types: [dynamic]^Type_Specification, // The generic type parameters of the expression.
}

/**
* Represents the kind of a compound field.
*/
Expression_Compound_Field_Kind :: enum {
  Default, // The default.
  Name,    // The field is assigned by name.
  Index,   // The field is assigned by index.
}

/**
* Represents the value of a compound field.
*/
Expression_Compound_Field_Value :: union {
  string,
  ^Expression,
}

/**
* Represents a single field inside a compound expression.
*/
Expression_Compound_Field :: struct {
  kind: Expression_Compound_Field_Kind,   // The kind of the field.
  position: Source_Position,              // The source position of the field.
  initializer: ^Expression,               // The initializer expression of the field.
  value: Expression_Compound_Field_Value, // The actual value of the field (Only applicable for name and index fields).
}

/**
* Represents a compound expression.
*/
Expression_Compound :: struct {
  using expr: Expression,                          // The base expression info.
  type: ^Type_Specification,                       // The type of the compound expression (Can be nil).
  fields: #soa [dynamic]Expression_Compound_Field, // The fields of the compound expression.
}

/**
* Represents a selector expression.
*/
Expression_Selector :: struct {
  using expr: Expression,    // The base expression info.
  expression: ^Expression,   // The expression to select the type of.
  type: ^Type_Specification, // The type of to select.
}

/**
* Represents an implicit selector expression.
*/
Expression_Implicit_Selector :: struct {
  using expr: Expression, // The base expression info.
  name: string,           // The name to implicitly select.
}

/**
* Represents a unary expression.
*/
Expression_Unary :: struct {
  using expr: Expression,  // The base expression info.
  operator: Token_Kind,    // The unary operator of the expression.
  expression: ^Expression, // The expression to operate on.
}

/**
* Represents a binary expression.
*/
Expression_Binary :: struct {
  using expr: Expression, // The base expression info.
  operator: Token_Kind,   // The binary operator of the expression.
  left: ^Expression,      // The left-hand side of the expression.
  right: ^Expression,     // The right-hand side of the expression.
}

/**
* Represents a ternary expression.
*/
Expression_Ternary :: struct {
  using expr: Expression,       // The base expression info.
  condition: ^Expression,       // The condition expression.
  then_expression: ^Expression, // The expression to evaluate when the condition is true.
  else_expression: ^Expression, // The expression to evaluate when the condition is false.
}

/**
* Represents a modify expression.
*/
Expression_Modify :: struct {
  using expr: Expression,  // The base expression info.
  operator: Token_Kind,    // The modify operator of the expression.
  is_post: bool,           // Should the operator be applied after or before the expression?
  expression: ^Expression, // The expression to modify.
}

/**
* Represents the kind of a query expression.
*/
Expression_Query_Kind :: enum {
  Size_Of_Expression,      // A 'size_of(foo)' query expression.
  Size_Of_Type,            // A 'size_of(:Foo)' query expression.
  Typeid_Of_Expression,    // A 'type_id(foo)' query expression.
  Typeid_Of_Type,          // A 'type_id(:Foo)' query expression.
  Type_Info_Of_Expression, // A 'type_info_of(foo)' query expression.
  Type_Info_Of_Type,       // A 'type_info_of(:Foo)' query expression.
}

/**
* Represents the value of a query expression.
*/
Expression_Query_Value :: union {
  ^Expression,
  ^Type_Specification,
}

/**
* Represents a query expression.
*/
Expression_Query :: struct {
  using expr: Expression,            // The base expression info.
  query_kind: Expression_Query_Kind, // The kind of the query expression.
  value: Expression_Query_Value,     // The actual value of the query expression.
}

/**
* Represents the kind of a directive expression.
*/
Expression_Directive_Kind :: enum {
  Line,     // A '#line' directive.
  Routine,  // A '#routine' directive.
  File,     // A '#file' directive.
  Location, // A '#location' directive.
}

/**
* Represents a directive expression.
*/
Expression_Directive :: struct {
  using expr: Expression,                    // The base expression info.
  directive_kind: Expression_Directive_Kind, // The kind of the directive expression.
}

/**
* Makes a new expression of a given type.
* 
* @param T        The type of the expression.
* @param kind     The kind of the expression.
* @param position The position of the expression.
* @return The new expression.
*/
ast_expression_make :: proc($T: typeid, kind: Expression_Kind, position: Source_Position) -> ^T {
  e := new(T);
  e.kind = kind;
  e.position = position;
  return e;
}

/**
* Makes a new parenthesized expression.
* 
* @param position   The position of the parenthesized expression.
* @param expression The parenthesized expression.
* @return The new parenthesized expression.
*/
ast_expression_make_parenthesized :: proc(position: Source_Position, expression: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Parenthesized, .Parenthesized, position);
  e.expression = expression;
  return e;
}

/**
* Makes a new boolean expression.
* 
* @param position The position of the boolean expression.
* @param value    The value of the boolean expression.
* @return The new boolean expression.
*/
ast_expression_make_boolean :: proc(position: Source_Position, value: bool) -> ^Expression {
  e := ast_expression_make(Expression_Literal, .Boolean, position);
  e.value = value;
  return e;
}

/**
* Makes a new integer expression.
* 
* @param position The position of the integer expression.
* @param value    The value of the integer expression.
* @return The new integer expression.
*/
ast_expression_make_integer :: proc(position: Source_Position, value: u64) -> ^Expression {
  e := ast_expression_make(Expression_Literal, .Integer, position);
  e.value = value;
  return e;
}

/**
* Makes a new float expression.
* 
* @param position The position of the float expression.
* @param value    The value of the float expression.
* @return The new float expression.
*/
ast_expression_make_float :: proc(position: Source_Position, value: f64) -> ^Expression {
  e := ast_expression_make(Expression_Literal, .Float, position);
  e.value = value;
  return e;
}

/**
* Makes a new character expression.
* 
* @param position The position of the character expression.
* @param value    The value of the character expression.
* @return The new character expression.
*/
ast_expression_make_character :: proc(position: Source_Position, value: rune) -> ^Expression {
  e := ast_expression_make(Expression_Literal, .Character, position);
  e.value = value;
  return e;
}

/**
* Makes a new string expression.
* 
* @param position The position of the string expression.
* @param value    The value of the string expression.
* @return The new string expression.
*/
ast_expression_make_string :: proc(position: Source_Position, value: string) -> ^Expression {
  e := ast_expression_make(Expression_Literal, .String, position);
  e.value = value;
  return e;
}

/**
* Makes a new name expression.
* 
* @param position      The position of the name expression.
* @param name          The value of the name expression.
* @param generic_types The generic types of the name expression.
* @return The new name expression.
*/
ast_expression_make_name :: proc(position: Source_Position, name: string, generic_types: [dynamic]^Type_Specification) -> ^Expression {
  e := ast_expression_make(Expression_Name, .Name, position);
  e.name = name;
  e.generic_types = generic_types;
  return e;
}

/**
* Makes a new cast expression.
* 
* @param position   The position of the cast expression.
* @param type       The type of the cast expression.
* @param expression The expression of the cast expression.
* @return The new cast expression.
*/
ast_expression_make_cast :: proc(position: Source_Position, type: ^Type_Specification, expression: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Cast, .Cast, position);
  e.type = type;
  e.expression = expression;
  return e;
}

/**
* Makes a new call expression.
* 
* @param position   The position of the call expression.
* @param expression The expression of the call expression.
* @param arguments  The arguments of the call expression.
* @return The new call expression.
*/
ast_expression_make_call :: proc(position: Source_Position, expression: ^Expression, arguments: [dynamic]^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Call, .Call, position);
  e.expression = expression;
  e.arguments = arguments;
  return e;
}

/**
* Makes a new index expression.
* 
* @param position   The position of the index expression.
* @param expression The expression of the index expression.
* @param index      The index of the index expression.
* @return The new index expression.
*/
ast_expression_make_index :: proc(position: Source_Position, expression: ^Expression, index: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Index, .Index, position);
  e.expression = expression;
  e.index = index;
  return e;
}

/**
* Makes a new slice expression.
* 
* @param position   The position of the slice expression.
* @param expression The expression of the slice expression.
* @param lower      The lower bound of the slice expression.
* @param higher     The higher bound of the slice expression.
* @return The new slice expression.
*/
ast_expression_make_slice :: proc(position: Source_Position, expression: ^Expression, lower: ^Expression, higher: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Slice, .Slice, position);
  e.expression = expression;
  e.lower = lower;
  e.higher = higher;
  return e;
}

/**
* Makes a new member expression.
* 
* @param position      The position of the member expression.
* @param expression    The expression of the member expression.
* @param name          The value of the member expression.
* @param generic_types The generic types of the member expression.
* @return The new member expression.
*/
ast_expression_make_member :: proc(position: Source_Position, expression: ^Expression, name: string, generic_types: [dynamic]^Type_Specification) -> ^Expression {
  e := ast_expression_make(Expression_Member, .Member, position);
  e.expression = expression;
  e.name = name;
  e.generic_types = generic_types;
  return e;
}

/**
* Makes a new compound expression.
* 
* @param position The position of the compound expression.
* @param type     The type of the compound expression.
* @param fields   The fields of the compound expression.
* @return The new compound expression.
*/
ast_expression_make_compound :: proc(position: Source_Position, type: ^Type_Specification, fields: #soa [dynamic]Expression_Compound_Field) -> ^Expression {
  e := ast_expression_make(Expression_Compound, .Compound, position);
  e.type = type;
  e.fields = fields;
  return e;
}

/**
* Makes a new selector expression.
* 
* @param position   The position of the selector expression.
* @param expression The expression of the selector expression.
* @param type       The type of the selector expression.
* @return The new selector expression.
*/
ast_expression_make_selector :: proc(position: Source_Position, expression: ^Expression, type: ^Type_Specification) -> ^Expression {
  e := ast_expression_make(Expression_Selector, .Selector, position);
  e.expression = expression;
  e.type = type;
  return e;
}

/**
* Makes a new implicit selector expression.
* 
* @param position The position of the implicit selector expression.
* @param name     The name of the implicit selector expression.
* @return The new implicit selector expression.
*/
ast_expression_make_implicit_selector :: proc(position: Source_Position, name: string) -> ^Expression {
  e := ast_expression_make(Expression_Implicit_Selector, .Implicit_Selector, position);
  e.name = name;
  return e;
}

/**
* Makes a new unary expression.
* 
* @param position   The position of the unary expression.
* @param operator   The operator of the unary expression.
* @param expression The expression of the unary expression.
* @return The new unary expression.
*/
ast_expression_make_unary :: proc(position: Source_Position, operator: Token_Kind, expression: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Unary, .Unary, position);
  e.operator = operator;
  e.expression = expression;
  return e;
}

/**
* Makes a new binary expression.
* 
* @param position The position of the binary expression.
* @param operator The operator of the binary expression.
* @param left     The left expression of the binary expression.
* @param right    The right expression of the binary expression.
* @return The new binary expression.
*/
ast_expression_make_binary :: proc(position: Source_Position, operator: Token_Kind, left: ^Expression, right: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Binary, .Binary, position);
  e.operator = operator;
  e.left = left;
  e.right = right;
  return e;
}

/**
* Makes a new ternary expression.
* 
* @param position        The position of the ternary expression.
* @param condition       The condition of the ternary expression.
* @param then_expression The then expression of the ternary expression.
* @param else_expression The else expression of the ternary expression.
* @return The new ternary expression.
*/
ast_expression_make_ternary :: proc(position: Source_Position, condition: ^Expression, then_expression: ^Expression, else_expression: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Ternary, .Ternary, position);
  e.condition = condition;
  e.then_expression = then_expression;
  e.else_expression = else_expression;
  return e;
}

/**
* Makes a new modify expression.
* 
* @param position   The position of the modify expression.
* @param operator   The operator of the modify expression.
* @param is_post    Whether or not the expression is a post modify.
* @param expression The expression of the modify expression.
* @return The new modify expression.
*/
ast_expression_make_modify :: proc(position: Source_Position, operator: Token_Kind, is_post: bool, expression: ^Expression) -> ^Expression {
  e := ast_expression_make(Expression_Modify, .Modify, position);
  e.operator = operator;
  e.is_post = is_post;
  e.expression = expression;
  return e;
}

/**
* Makes a new query expression.
* 
* @param position   The position of the query expression.
* @param query_kind The kind of the query expression.
* @param value      The value of the query expression.
* @return The new query expression.
*/
ast_expression_make_query :: proc(position: Source_Position, query_kind: Expression_Query_Kind, value: Expression_Query_Value) -> ^Expression {
  e := ast_expression_make(Expression_Query, .Query, position);
  e.query_kind = query_kind;
  e.value = value;
  return e;
}

/**
* Makes a new directive expression.
* 
* @param position       The position of the directive expression.
* @param directive_kind The kind of the directive expression.
* @return The new directive expression.
*/
ast_expression_make_directive :: proc(position: Source_Position, directive_kind: Expression_Directive_Kind) -> ^Expression {
  e := ast_expression_make(Expression_Directive, .Directive, position);
  e.directive_kind = directive_kind;
  return e;
}
