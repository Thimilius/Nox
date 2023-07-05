package nox

import "core:mem"
import "core:strings"
import "tracy"

/**
* Represents the kind of a type.
* (NOTE: The order of this is not arbitrary as it gets used in '<' and '>' comparisons.)
*/
Type_Kind :: enum {
  None,

  Void,

  B8,
  B16,
  B32,
  B64,
  Bool,
  Untyped_Boolean,

  I8,
  I16,
  I32,
  I64,
  Int,
  U8,
  U16,
  U32,
  U64,
  UInt,
  Untyped_Integer,

  F32,
  F64,
  Untyped_Float,

  Char,
  Untyped_Char,

  Enumeration,
 
  String,
  CString,
  Untyped_String,

  Rawptr,
  Pointer,
  Self_Relative_Pointer,
  Offset_Relative_Pointer,
  SoA_Layout_Pointer,
  AoSoA_Layout_Pointer,
  Dynamic_Pointer,
  
  Constant,
  Array,
  Dynamic_Array,
  Slice,
  Map,
  Tuple,

  Struct,
  Union,

  Procedure,
  Function,

  Interface,

  Any,
  Typeid,

  Untyped_Null,

  Generic,
}

/**
* Represents the kind for a type info.
* (Must match with the enumeration in the 'runtime' package.)
*/
Type_Info_Kind :: enum {
  Invalid,
  Void,
  Boolean,
  Integer,
  Float,
  Char,
  Enumeration,
  String,
  Pointer,
  Relative_Pointer,
  Layout_Pointer,
  Dynamic_Pointer,
  Array,
  Dynamic_Array,
  Slice,
  Map,
  Tuple,
  Struct,
  Union,
  Routine,
  Interface,
  Any,
  Typeid,
}

/**
* The state for a type.
*/
Type_State :: enum {
  Incomplete, // The type is incomplete (Only possible for structs and unions).
  Completing, // The type is currently completing (Only possible for structs and unions).
  Completed,  // The type is completed.
}

Type_Id :: uint;

/**
* Represents possible flags for a type.
*/
Type_Flag :: enum {
  Non_Modifiable,                            // For types that cannot be modified through '++' or '--'.
  
  Not_Allowed_As_Pointer,                    // For types that are not allowed to be pointers.
  Not_Allowed_As_Base,                       // For types that are not allowed to be a base of arrays, slices and tuples.
  Not_Allowed_As_Field,                      // For types that are not allowed as struct fields.
  Not_Allowed_As_Tuple,                      // For types that are not allowed as tuple elements.
  Not_Allowed_As_Union,                      // For types that are not allowed to be part of unions.
  Not_Allowed_As_Variable,                   // For types that are not allowed as local/global variables.
  Not_Allowed_As_Parameter_Or_Return_Type,   // For types that are not allowed as parameters or return type.
  Not_Allowed_As_Parameter_In_Pure_Function, // For types that are not allowed as parameters in pure functions.
  
  Has_Hash_Function,                         // The type has a simple function that can generate a hash from a value of the type.
  Has_Compare_Function,                      // The type has a simple function that can determine if two values from the type are equal.

  Uninstantiated_Generic,                    // For uninstantiated generic types.
  Defined_Distinct,                          // For types that are created through 'type_define' declarations.
  Interface_Used_In_Dynamic_Pointer,         // For interfaces that are used as the base of a dynamic pointer.

  // Those are pretty much only relevant inside the generator.
  Generate_RTTR,                             // Generate runtime type information for this type in the type table.
  Contains_Self_Relative_Pointer_In_Value,   // For types that contain self-relative pointers inside their actual value (pointers or slices don't for example).
  Passed_As_Pointer,                         // For types that are passed as pointers to routines.
  Requires_Address_Load,                     // For types that require the 'Loading_Address' loading mode when generating.
}

Type_Flags :: bit_set[Type_Flag];

/**
* The type for a collection.
*/
Collection_Type :: enum {
  None,
  Slice,
  Array,
  Dynamic_Array,
}

/**
* Layout information for a collection.
*/
Layout_Info :: struct {
  modifier: Layout_Modifier, // The layout modifier.
  items_in_aosoa_chunk: int, // The number of items in an AoSoA chunk (Only relevant for an AoSoA layout).
}

/**
* Default layout info with no special layout.
*/
LAYOUT_INFO_NONE :: Layout_Info{.None, 0}

/**
* Represents the size for the L1 cache line in bytes.
* This gets set programmatically at startup (Should be 64 bytes on most CPUs).
*/
L1_CACHE_LINE_SIZE: uint;

/**
* Base structure for representing a type.
*/
Type :: struct {
  kind: Type_Kind,                    // The kind of the type.
  state: Type_State,                  // The state of the type.
  flags: Type_Flags,                  // The flags of the type.
  id: Type_Id,                        // The id of the type.

  size: uint,                         // The size of the type in bytes.
  alignment: uint,                    // The alignment of the type in bytes.

  name: string,                       // The name of the type.
  symbol: ^Symbol,                    // The symbol that defines the type (Not present for built-in types).

  base: ^Type,                        // The base type for the type (Used for example in all pointer kinds and collections).
  methods: #soa [dynamic]Type_Method, // All methods implemented for the type.
  interfaces: [dynamic]^Type,         // All interfaces the type implements.
}

/**
* Represents a method implemented on a type.
*/
Type_Method :: struct {
  access_modifier: Access_Modifier, // The access modifier of the method.

  // Because methods for a type can currently be implemented in ANY package/file, we have to store that position information here.
  // That way we can properly do the access check for the method unrelated to the position where the type was defined.
  pack: ^Package,                   // The package where the method is implemented.
  file: string,                     // The file where the method is implemented.
  
  name: string,                     // The name of the method.
  type: ^Type,                      // The type of the method.
}

/**
* Represents an enumeration type.
*/
Type_Enumeration :: struct {
  using type: Type,                           // The base type info.
  items: #soa [dynamic]Type_Enumeration_Item, // The items of the enum.
  is_flags: bool,                             // Is the enumeration defined with the '@flags' attribute?
}

/**
* Represents a single item for an enumeration type.
*/
Type_Enumeration_Item :: struct {
  name: string, // The name of the item.
  value: Value, // The constant value of the item (Will always be an integer type).
}

/**
* Represents a relative pointer type.
*/
Type_Relative_Pointer :: struct {
  using type: Type,     // The base type info.
  relative_base: ^Type, // The relative base type used to represent the relative offset.
}

/**
* Represents an array type.
*/
Type_Array :: struct {
  using type: Type,              // The base type info.
  layout_info: Layout_Info,      // The layout info of the array.
  number_of_elements: int,       // The number of elements of the array (Will be 0 for incomplete arrays).
  has_incomplete_elements: bool, // Is the array incomplete (Has not yet a defined size)?
}

/**
* Represents a dynamic array type.
*/
Type_Dynamic_Array :: struct {
  using type: Type,         // The base type info.
  layout_info: Layout_Info, // The layout info of the dynamic array.
}

/**
* Represents a slice type.
*/
Type_Slice :: struct {
  using type: Type,         // The base type info.
  layout_info: Layout_Info, // The layout info of the slice.
}

/**
* Represents a map type.
*/
Type_Map :: struct {
  using type: Type, // The base type info.
  key: ^Type,       // The key type of the map.
  value: ^Type,     // The value type of the map.
}

/**
* Represents a tuple type.
*/
Type_Tuple :: struct {
  using type: Type,                           // The base type info.
  elements: #soa [dynamic]Type_Tuple_Element, // The elements of the tuple.
}

/**
* Represents a single element of a tuple type.
*/
Type_Tuple_Element :: struct {
  type: ^Type,  // The type of the element.
  offset: uint, // The offset of the element.
}

/**
* Represents a struct type.
*/
Type_Struct :: struct {
  using type: Type,                        // The base type info.
  fields: #soa [dynamic]Type_Struct_Field, // The fields of the struct.
  layout_info: Layout_Info,                // The layout info of the struct if it is used in an SoA/AoSoA collection.
  generic_types: [dynamic]^Type,           // The generic types of the struct (This is just bookkeeping that we need because structs are initially incomplete).
}

/**
* Represents a single field in a struct type.
*/
Type_Struct_Field :: struct {
  access_modifier: Access_Modifier, // The access modifier of the field.
  is_composite: bool,               // Is the field marked with the 'composite' keyword?
  name: string,                     // The name of the field.
  type: ^Type,                      // The type of the field.
  offset: uint,                     // The offset of the field.
}

/**
* Represents a union type.
*/
Type_Union :: struct {
  using type: Type,         // The base type info.
  variants: [dynamic]^Type, // The variants in the union.
  biggest_type: ^Type,      // The biggest type of all variants in the union.
}

/**
* Represents flags for a routine type.
*/
Type_Routine_Flag :: enum {
  Has_Params,       // Has the routine a 'params' parameter?
  Has_C_Varargs,    // Has the routine an c varargs parameter?
  Is_Intrinsic,     // Is the routine an intrinsic?
  Is_Method,        // Is the routine a method?
  Is_Pure_Function, // Is the routine a pure function?
}
Type_Routine_Flags :: bit_set[Type_Routine_Flag];

/**
* Represents a routine type.
*/
Type_Routine :: struct {
  using type: Type,                                 // The base type info.
  calling_convention: Calling_Convention,           // The calling convention of the routine.
  parameters: #soa [dynamic]Type_Routine_Parameter, // The parameters of the routine.
  return_type: ^Type,                               // The return type of the routine.
  routine_flags: Type_Routine_Flags,                // The flags of the routine type.
}

/**
* Represents a single parameter in a routine type.
*/
Type_Routine_Parameter :: struct {
  type: ^Type,      // The type of the parameter.
  is_default: bool, // Is the parameter a default parameter?
}

/**
* Represents a cached constant type.
*/
Type_Cached_Constant :: struct {
  base: ^Type,                     // The base type of the constant type.
  is_uninstantiated_generic: bool, // Is the type an uninstantied generic?
}

/**
* Represents a cached pointer type.
*/
Type_Cached_Pointer :: struct {
  base: ^Type,                     // The base type of the pointer type.
  is_uninstantiated_generic: bool, // Is the type an uninstantied generic?
}

/**
* Represents a cached relative pointer type.
*/
Type_Cached_Relative_Pointer :: struct {
  base: ^Type,                     // The base type of the relative pointer type.
  relative_base: ^Type,            // The relative base of the relative pointer type.
  is_uninstantiated_generic: bool, // Is the type an uninstantied generic?
}

/**
* Represents a cached dynamic pointer type.
*/
Type_Cached_Dynamic_Pointer :: struct {
  base: ^Type,                     // The base type of the dynamic pointer type.
  is_uninstantiated_generic: bool, // Is the type an uninstantied generic?
}

/**
* Represents a cached array type.
*/
Type_Cached_Array :: struct {
  layout_modifier: Layout_Modifier, // The layout modifier of the array type.
  base: ^Type,                      // The base type of the array type.
  number_of_elements: int,          // The number of elements of the array.
  is_uninstantiated_generic: bool,  // Is the type an uninstantied generic?
}

/**
* Represents a cached dynamic array type.
*/
Type_Cached_Dynamic_Array :: struct {
  layout_modifier: Layout_Modifier, // The layout modifier of the dynamic array type.
  base: ^Type,                      // The base type of the dynamic array type.
  is_uninstantiated_generic: bool,  // Is the type an uninstantied generic?
}

/**
* Represents a cached slice type.
*/
Type_Cached_Slice :: struct {
  layout_modifier: Layout_Modifier, // The layout modifier of the slice type.
  base: ^Type,                      // The base type of the slice type.
  is_uninstantiated_generic: bool,  // Is the type an uninstantied generic?
}

/**
* Represents a cached map type.
*/
Type_Cached_Map :: struct {
  key: ^Type,                      // The key type of the map type.
  value: ^Type,                    // The value type of the map type.
  is_uninstantiated_generic: bool, // Is the type an uninstantied generic?
}

/**
* Represents a cached tuple type.
*/
Type_Cached_Tuple :: struct {
  elements: #soa [dynamic]Type_Tuple_Element, // The elements of the tuple type.
  is_uninstantiated_generic: bool,            // Is the type an uninstantied generic?

  tuple: ^Type,                               // The cached tuple type.
}

/**
* Represents a cached routine type.
*/
Type_Cached_Routine :: struct {
  calling_convention: Calling_Convention,           // The calling convention of the routine type.
  parameters: #soa [dynamic]Type_Routine_Parameter, // The parameters of the routine type.
  return_type: ^Type,                               // The return type of the routine type.
  routine_flags: Type_Routine_Flags,                // The flags of the routine type.
  is_uninstantiated_generic: bool,                  // Is the type an uninstantied generic?

  routine: ^Type,                                   // The cached routine type.
}

/**
* Represents a cached generic type.
*/
Type_Cached_Generic :: struct {
  name: string, // The name of the generic type.
}

/**
* Represents a cached instantied type.
*/
Type_Cached_Instantiated :: struct {
  generic_types: [dynamic]^Type, // The generic types of the instantiated type.
  
  instantiated_type: ^Type,      // The cached instantiated type.
}

/**
* Represents a cached layout struct type.
*/
Type_Cached_Layout_Struct :: struct {
  collection_type: Collection_Type, // The collection type the layout struct is used in.
  base_struct: ^Type,               // The struct (in AoS form) the layout struct is based on.
  // For now we don't have the full 'Layout_Info' here as it is not needed.
  // The 'items_in_aosoa_chunk' are constant based on 'base_struct' and 'number_of_items'.
  layout_modifier: Layout_Modifier, // The layout modifier of the layout struct.
  number_of_elements: int,          // The number of elements (In case of a fixed array collection).
}

/**
* The cache for all types.
*/
Type_Cache :: struct {
  constants: map[Type_Cached_Constant]^Type,                        // Cached constant types.
  pointers: map[Type_Cached_Pointer]^Type,                          // Cached pointer types.
  self_relative_pointers: map[Type_Cached_Relative_Pointer]^Type,   // Cached self-relative pointer types.
  offset_relative_pointers: map[Type_Cached_Relative_Pointer]^Type, // Cached offset-relative pointer types.
  soa_layout_pointers: map[Type_Cached_Pointer]^Type,               // Cached SoA layout pointers.
  aosoa_layout_pointers: map[Type_Cached_Pointer]^Type,             // Cached AoSoA layout pointers.
  dynamic_pointers: map[Type_Cached_Dynamic_Pointer]^Type,          // Cached dynamic pointer types.
  arrays: map[Type_Cached_Array]^Type,                              // Cached array types.
  dynamic_arrays: map[Type_Cached_Dynamic_Array]^Type,              // Cached dynamic array types.
  slices: map[Type_Cached_Slice]^Type,                              // Cached slice types.
  maps: map[Type_Cached_Map]^Type,                                  // Cached map types.
  generics: map[Type_Cached_Generic]^Type,                          // Cached generic types.
  layout_structs: map[Type_Cached_Layout_Struct]^Type,              // Cached layout structs.

  // The following can't be maps as the cached structs contain a dynamic array which forbids being used as a map key.
  tuples: [dynamic]Type_Cached_Tuple,                               // Cached tuple types.
  procedures: [dynamic]Type_Cached_Routine,                         // Cached procedure type.
  functions: [dynamic]Type_Cached_Routine,                          // Cached function types.
  instantiated: map[^Type][dynamic]Type_Cached_Instantiated,        // Cached instantied type.
}

/**
* Cached types of the 'runtime' package.
*/
Type_Cached_Runtime_Types :: struct {
  operating_system_kind: ^Type,      // The 'Operating_System_Kind' enum.
  source_location: ^Type,            // The 'Source_Location' struct.
  context_struct: ^Type,             // The 'Context' struct.
  allocator: ^Type,                  // The 'Allocator' struct.

  type_info: ^Type,                  // The 'Type_Info' struct.
  type_info_variant: ^Type,          // The 'Type_Info_Variant' union.
  type_info_integer: ^Type,          // The 'Type_Info_Integer' struct.
  type_info_enumeration: ^Type,      // The 'Type_Info_Enumeration' struct.
  type_info_string: ^Type,           // The 'Type_Info_String' struct.
  type_info_pointer: ^Type,          // The 'Type_Info_Pointer' struct.
  type_info_relative_pointer: ^Type, // The 'Type_Info_Relative_Pointer' struct.
  type_info_layout_pointer: ^Type,   // The 'Type_Info_Layout_Pointer' struct.
  type_info_dynamic_pointer: ^Type,  // The 'Type_Info_Dynamic_Pointer' struct.
  type_info_array: ^Type,            // The 'Type_Info_Array' struct.
  type_info_dynamic_array: ^Type,    // The 'Type_Info_Dynamic_Array' struct.
  type_info_slice: ^Type,            // The 'Type_Info_Slice' struct.
  type_info_map: ^Type,              // The 'Type_Info_Map' struct.
  type_info_tuple: ^Type,            // The 'Type_Info_Tuple' struct.
  type_info_struct: ^Type,           // The 'Type_Info_Struct' struct.
  type_info_union: ^Type,            // The 'Type_Info_Union' struct.
  type_info_routine: ^Type,          // The 'Type_Info_Routine' struct.
  type_info_interface: ^Type,        // The 'Type_Info_Interface' struct.
}

/**
* Represents all stored types in a program.
*/
Type_Storage :: struct {
  types: [dynamic]^Type,                           // Holds all allocated types.
  typeid_counter: Type_Id,                         // The type id counter.
  
  type_pool: mem.Dynamic_Pool,                     // The backing pool for the type allocator.
  type_allocator: mem.Allocator,                   // The allocator used for types.

  name_pool: mem.Dynamic_Pool,                     // The backing pool for the name allocator.
  name_allocator: mem.Allocator,                   // The allocator used for type names.

  type_void: ^Type,                                // The 'void' type.
  type_b8: ^Type,                                  // The 'b8' type.
  type_b16: ^Type,                                 // The 'b16' type.
  type_b32: ^Type,                                 // The 'b32' type.
  type_b64: ^Type,                                 // The 'b64' type.
  type_bool: ^Type,                                // The 'bool' type.
  type_i8: ^Type,                                  // The 'i8' type.
  type_i16: ^Type,                                 // The 'i16' type.
  type_i32: ^Type,                                 // The 'i32' type.
  type_i64: ^Type,                                 // The 'i64' type.
  type_int: ^Type,                                 // The 'int' type.
  type_u8: ^Type,                                  // The 'u8' type.
  type_u16: ^Type,                                 // The 'u16' type.
  type_u32: ^Type,                                 // The 'u32' type.
  type_u64: ^Type,                                 // The 'u64' type.
  type_uint: ^Type,                                // The 'uint' type.
  type_f32: ^Type,                                 // The 'f32' type.
  type_f64: ^Type,                                 // The 'f64' type.
  type_char: ^Type,                                // The 'char' type.
  type_string: ^Type,                              // The 'string' type.
  type_cstring: ^Type,                             // The 'cstring' type.
  type_rawptr: ^Type,                              // The 'rawptr' type.
  type_any: ^Type,                                 // The 'any' type.
  type_typeid: ^Type,                              // The 'typeid' type.
  type_untyped_boolean: ^Type,                     // The untyped 'boolean' type.
  type_untyped_integer: ^Type,                     // The untyped 'integer' type.
  type_untyped_float: ^Type,                       // The untyped 'float' type.
  type_untyped_char: ^Type,                        // The untyped 'char' type.
  type_untyped_string: ^Type,                      // The untyped 'string' type.
  type_untyped_null: ^Type,                        // The untyped 'null' type.

  cache: Type_Cache,                               // All cached types in the storage.
  cached_runtime_types: Type_Cached_Runtime_Types, // The cached types in the 'runtime' package.
}

/**
* Represents the metric for a built-in type.
*/
Type_Metric :: struct {
  size: uint,      // The size of the type in bytes.
  alignment: uint, // The alignment of the type in bytes.
  unsigned: bool,  // Is the type unsigned? (Only applicable for integers)
}

/**
* Holds the type metrics for all built-in types.
* (NOTE: This is only for 64-bit architectures as this is the only one we support.)
*/
TYPE_METRICS: [64]Type_Metric = {
  Type_Kind.B8                   = {1, 1, true},
  Type_Kind.B16                  = {2, 2, true},
  Type_Kind.B32                  = {4, 4, true},
  Type_Kind.B64                  = {8, 8, true},
  Type_Kind.Bool                 = {1, 1, true},
  
  Type_Kind.I8                   = {1, 1, false},
  Type_Kind.I16                  = {2, 2, false},
  Type_Kind.I32                  = {4, 4, false},
  Type_Kind.I64                  = {8, 8, false},
  Type_Kind.Int                  = {8, 8, false},
  Type_Kind.U8                   = {1, 1, true},
  Type_Kind.U16                  = {2, 2, true},
  Type_Kind.U32                  = {4, 4, true},
  Type_Kind.U64                  = {8, 8, true},
  Type_Kind.UInt                 = {8, 8, true},
  
  Type_Kind.F32                  = {4, 4, false},
  Type_Kind.F64                  = {8, 8, false},

  Type_Kind.Char                 = {4, 4, true},

  Type_Kind.String               = {16, 8, false}, // Internal structure: (rawptr, int).
  Type_Kind.CString              = {8, 8, true},

  Type_Kind.Rawptr               = {8, 8, true},
  Type_Kind.Pointer              = {8, 8, true},
  Type_Kind.SoA_Layout_Pointer   = {16, 8, false}, // Internal structure: (rawptr, index).
  Type_Kind.AoSoA_Layout_Pointer = {16, 8, false}, // Internal structure: (rawptr, index).
  Type_Kind.Dynamic_Pointer      = {16, 8, false}, // Internal structure: (rawptr, rawptr).

  Type_Kind.Dynamic_Array        = {40, 8, false}, // Internal structure: (rawptr, int, int, Allocator).
  Type_Kind.Slice                = {16, 8, false}, // Internal structure: (rawptr, int).
  Type_Kind.Map                  = {56, 8, false}, // Internal structure: (rawptr, int, int, Allocator, rawptr, rawptr).

  Type_Kind.Any                  = {16, 8, false}, // Internal structure: (rawptr, int).
  Type_Kind.Typeid               = {8, 8, false},
}

/**
* Makes a new type storage.
*
* @return The new type storage.
*/
type_storage_make :: proc() -> ^Type_Storage {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  storage := new(Type_Storage);
  type_storage_init(storage);
  return storage;
}

/**
* Initializes a type storage.
* 
* @param storage The type storage to initialize.
*/
type_storage_init :: proc(storage: ^Type_Storage) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  L1_CACHE_LINE_SIZE = os_get_l1_cache_line_size();

  // We start the counter at 1, so that 0 can denote an invalid id.
  storage.typeid_counter = 1;

  mem.dynamic_pool_init(&storage.type_pool);
  storage.type_allocator = mem.dynamic_pool_allocator(&storage.type_pool);

  mem.dynamic_pool_init(&storage.name_pool);
  storage.name_allocator = mem.dynamic_pool_allocator(&storage.name_pool);

  void_flags: Type_Flags = {
    .Not_Allowed_As_Base,
    .Not_Allowed_As_Parameter_Or_Return_Type,
    .Not_Allowed_As_Variable,
    .Not_Allowed_As_Field,
    .Not_Allowed_As_Tuple,
    .Not_Allowed_As_Union,
    .Not_Allowed_As_Parameter_In_Pure_Function,
  };
  common_flags: Type_Flags = {.Has_Hash_Function, .Has_Compare_Function};
  type_storage_init_builtin_type(storage, &storage.type_void, .Void, "void", void_flags);
  type_storage_init_builtin_type(storage, &storage.type_b8, .B8, "b8", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_b16, .B16, "b16", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_b32, .B32, "b32", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_b64, .B64, "b64", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_bool, .Bool, "bool", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_i8, .I8, "i8", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_i16, .I16, "i16", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_i32, .I32, "i32", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_i64, .I64, "i64", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_int, .Int, "int", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_u8, .U8, "u8", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_u16, .U16, "u16", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_u32, .U32, "u32", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_u64, .U64, "u64", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_uint, .UInt, "uint", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_f32, .F32, "f32", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_f64, .F64, "f64", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_char, .Char, "char", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_string, .String, "string", common_flags + {.Passed_As_Pointer});
  type_storage_init_builtin_type(storage, &storage.type_cstring, .CString, "cstring", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_rawptr, .Rawptr, "rawptr", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_any, .Any, "any", common_flags + {.Passed_As_Pointer, .Requires_Address_Load});
  type_storage_init_builtin_type(storage, &storage.type_typeid, .Typeid, "typeid", common_flags);
  type_storage_init_builtin_type(storage, &storage.type_untyped_boolean, .Untyped_Boolean, "untyped boolean");
  type_storage_init_builtin_type(storage, &storage.type_untyped_integer, .Untyped_Integer, "untyped integer");
  type_storage_init_builtin_type(storage, &storage.type_untyped_float, .Untyped_Float, "untyped float");
  type_storage_init_builtin_type(storage, &storage.type_untyped_char, .Untyped_Char, "untyped char");
  type_storage_init_builtin_type(storage, &storage.type_untyped_string, .Untyped_String, "untyped_string");
  type_storage_init_builtin_type(storage, &storage.type_untyped_null, .Untyped_Null, "untyped null");

  storage.type_rawptr.base = storage.type_void;
}

/**
* Initializes a built-in type.
*
* @param storage  The reference to the type storage.
* @param out_type Pointer to the type that will be set as the built-in type.
* @param kind     The kind of the built-in type.
* @param name     The name of the built-in type.
* @param flags    The flags of the built-in type.
*/
type_storage_init_builtin_type :: proc(storage: ^Type_Storage, out_type: ^^Type, kind: Type_Kind, name: string, flags: Type_Flags = {}) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type := type_storage_make_type(storage, Type, kind, name);
  type.flags = flags;
  type.size = TYPE_METRICS[kind].size;
  type.alignment = TYPE_METRICS[kind].alignment;
  // We will always generate RTTR for basic types (which are not untyped).
  if !type_is_untyped(type) do type.flags += {.Generate_RTTR};
  out_type^ = type;
}

/**
* Destroys the resources associated with a type storage.
*
* @param storage The type storage to destroy.
*/
type_storage_destroy :: proc(storage: ^Type_Storage) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  for type in storage.types {
    if type_is_enumeration(type) {
      delete_soa((cast(^Type_Enumeration) type).items);
    } else if type_is_struct(type) {
      type_struct := cast(^Type_Struct) type;
      delete_soa(type_struct.fields);
      delete(type_struct.generic_types);  
    } else if type_is_union(type) {
      delete((cast(^Type_Union) type).variants);
    } else if type_is_tuple(type) {
      delete_soa((cast(^Type_Tuple) type).elements);
    } else if type_is_routine(type) {
      delete_soa((cast(^Type_Routine) type).parameters);
    }

    delete_soa(type.methods);
    delete(type.interfaces);
  }
  delete(storage.types);

  mem.dynamic_pool_destroy(&storage.type_pool);
  mem.dynamic_pool_destroy(&storage.name_pool);

  delete(storage.cache.constants);
  delete(storage.cache.pointers);
  delete(storage.cache.self_relative_pointers);
  delete(storage.cache.offset_relative_pointers);
  delete(storage.cache.soa_layout_pointers);
  delete(storage.cache.aosoa_layout_pointers);
  delete(storage.cache.dynamic_pointers);
  delete(storage.cache.arrays);
  delete(storage.cache.dynamic_arrays);
  delete(storage.cache.slices);
  delete(storage.cache.maps);
  delete(storage.cache.tuples);
  delete(storage.cache.generics);
  delete(storage.cache.procedures);
  delete(storage.cache.functions);
  for _, cached_types_instantiated in storage.cache.instantiated {
    delete(cached_types_instantiated);
  }
  delete(storage.cache.instantiated);
  delete(storage.cache.layout_structs);

  free(storage);
}

/**
* Makes a new type.
*
* @param storage The reference to the type storage.
* @param T       The type of the type to make.
* @param kind    The kind of the type to make.
* @param name    The name of the type to make.
* @return The new type.
*/
type_storage_make_type :: proc(storage: ^Type_Storage, $T: typeid, kind: Type_Kind, name: string) -> ^T {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type := new(T, storage.type_allocator);
  type.id = storage.typeid_counter;
  type.kind = kind;
  type.state = .Completed;
  type.name = name;

  storage.typeid_counter += 1;
  append(&storage.types, type);

  return type;
}

/**
* Copies relevant data from a source type to a destination type.
* 
* @param destination_type The destination type.
* @param source_type      The source type.
*/
type_storage_copy_type :: proc(destination_type: ^$T, source_type: ^Type) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  id := destination_type.id;
  name := destination_type.name;
  destination_type^ = (cast(^T) source_type)^;
  destination_type.id = id;
  destination_type.name = name;
  destination_type.methods = {}; // The methods a type implements are unique for any given type (even instantiated structs).
}

/**
* Makes a new incomplete type (Can only ever be a struct or union).
*
* @param storage The reference to the type storage.
* @param kind    The kind of the incomplete type.
* @param symbol  The symbol that defines the incomplete type.
* @return The new incomplete type.
*/
type_storage_make_type_incomplete :: proc(storage: ^Type_Storage, kind: Type_Kind, symbol: ^Symbol) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(kind == .Struct || kind == .Union);

  type: ^Type;
  if kind == .Struct {
    type = type_storage_make_type(storage, Type_Struct, kind, symbol.name);
  } else {
    type = type_storage_make_type(storage, Type_Union, kind, symbol.name);
  }
  
  type.flags += .Uninstantiated_Generic in symbol.flags ? {.Uninstantiated_Generic} : {};
  type.state = .Incomplete;
  type.symbol = symbol;

  return type;
}

/**
* Makes a new distinct type.
*
* @param storage The reference to the type storage.
* @param name    The name of the distinct type.
* @param base    The base type of the distinct type.
* @return The new distinct type.
*/
type_storage_make_type_distinct :: proc(storage: ^Type_Storage, name: string, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  kind := base.kind;
  #partial switch kind {
    case .Enumeration:             return type_storage_make_type_distinct_internal(storage, kind, Type_Enumeration, name, base);
    case .Self_Relative_Pointer:   return type_storage_make_type_distinct_internal(storage, kind, Type_Relative_Pointer, name, base);
    case .Offset_Relative_Pointer: return type_storage_make_type_distinct_internal(storage, kind, Type_Relative_Pointer, name, base);
    case .Array:                   return type_storage_make_type_distinct_internal(storage, kind, Type_Array, name, base);
    case .Dynamic_Array:           return type_storage_make_type_distinct_internal(storage, kind, Type_Dynamic_Array, name, base);
    case .Slice:                   return type_storage_make_type_distinct_internal(storage, kind, Type_Slice, name, base);
    case .Map:                     return type_storage_make_type_distinct_internal(storage, kind, Type_Map, name, base);
    case .Tuple:                   return type_storage_make_type_distinct_internal(storage, kind, Type_Tuple, name, base);
    case .Struct:                  return type_storage_make_type_distinct_internal(storage, kind, Type_Struct, name, base);
    case .Union:                   return type_storage_make_type_distinct_internal(storage, kind, Type_Union, name, base);
    case .Procedure, .Function:    return type_storage_make_type_distinct_internal(storage, kind, Type_Routine, name, base);
    case:                          return type_storage_make_type_distinct_internal(storage, kind, Type, name, base);
  }
}

/**
* Makes a new distinct type from a concrete provided type.
*
* @param storage The reference to the type storage.
* @param kind    The kind of the distinct type.
* @param T       The type of the distinct type.
* @param name    The name of the distinct type.
* @param base    The base type of the distinct type.
* @return The new distinct type.
*/
type_storage_make_type_distinct_internal :: proc(storage: ^Type_Storage, kind: Type_Kind, $T: typeid, name: string, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type := type_storage_make_type(storage, T, kind, name);
  type_storage_copy_type(type, base);
  type.flags += {.Defined_Distinct};

  return type;
}

/**
* Makes a new enumeration type.
*
* @param storage  The reference to the type storage.
* @param symbol   The symbol that defines the enumeration type.
* @param base     The base type of the enumeration type.
* @param items    The items of the enumeration type.
* @param is_flags Is the enumeratioon type marked with the '@flags' attribute?
* @return The new enumeration type.
*/
type_storage_make_type_enumeration :: proc(storage: ^Type_Storage, symbol: ^Symbol, base: ^Type, items: #soa [dynamic]Type_Enumeration_Item, is_flags: bool) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type := type_storage_make_type(storage, Type_Enumeration, .Enumeration, symbol.name);
  type.symbol = symbol;
  type.size = base.size;
  type.alignment = base.alignment;
  type.flags = {.Has_Hash_Function, .Has_Compare_Function};
  type.base = base;
  type.items = items;
  type.is_flags = is_flags;

  return type;
}

/**
* Makes a new routine type.
*
* @param storage            The reference to the type storage.
* @param kind               The kind of the routine type.
* @param calling_convention The calling convention of the routine type.
* @param parmaters          The parameters of the routine type.
* @param return_type        The return type of the routine type.
* @param flags              Is the enumeratioon type marked with the '@flags' attribute?
* @return The new routine type.
*/
type_storage_make_type_routine :: proc(
  storage: ^Type_Storage,
  kind: Type_Kind,
  calling_convention: Calling_Convention,
  parameters: #soa [dynamic]Type_Routine_Parameter,
  return_type: ^Type,
  routine_flags: Type_Routine_Flags,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  name_builder := strings.builder_make(storage.name_allocator);
  type_write_routine_name_raw(&name_builder, kind, calling_convention, parameters, return_type, routine_flags, "");
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Routine, kind, name);
  type.flags += {.Not_Allowed_As_Parameter_In_Pure_Function, .Has_Hash_Function, .Has_Compare_Function};
  type.size = TYPE_METRICS[Type_Kind.Pointer].size;
  type.alignment = TYPE_METRICS[Type_Kind.Pointer].alignment;
  type.calling_convention = calling_convention;
  type.parameters = parameters;
  type.return_type = return_type;
  type.routine_flags = routine_flags;

  for parameter in parameters {
    if .Uninstantiated_Generic in parameter.type.flags {
      type.flags += {.Uninstantiated_Generic};
      break;
    }  
  }
  if .Uninstantiated_Generic in return_type.flags do type.flags += {.Uninstantiated_Generic};

  if .Is_Intrinsic in routine_flags || .Is_Method in routine_flags {
    type.flags += {
      .Not_Allowed_As_Pointer,
      .Not_Allowed_As_Base,
      .Not_Allowed_As_Field,
      .Not_Allowed_As_Union,
      .Not_Allowed_As_Tuple,
      .Not_Allowed_As_Variable,
      .Not_Allowed_As_Parameter_Or_Return_Type,
    };
  }

  return type;
}

/**
* Makes a new interface type.
*
* @param storage The reference to the type storage.
* @param symbol  The symbol that defines the interface type.
* @param methods The methods of the interface type.
* @return The new interface type.
*/
type_storage_make_type_interface :: proc(storage: ^Type_Storage, symbol: ^Symbol, methods: #soa [dynamic]Type_Method) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type := type_storage_make_type(storage, Type, .Interface, symbol.name);
  type.flags += {
    .Not_Allowed_As_Pointer,
    .Not_Allowed_As_Base,
    .Not_Allowed_As_Field,
    .Not_Allowed_As_Union,
    .Not_Allowed_As_Tuple,
    .Not_Allowed_As_Variable,
    .Not_Allowed_As_Parameter_Or_Return_Type,
  };
  type.symbol = symbol;
  type.size = 0;
  type.alignment = 0;
  type.methods = methods;

  return type;
}

/**
* Makes a new instantiated type (Can only ever be a struct).
*
* @param storage               The reference to the type storage.
* @param uninstantiated_symbol The symbol that defines the uninstantiated type.
* @param generic_types         The generic types of the instantiated type.
* @return The new instantiated type.
*/
type_storage_make_type_instantiated :: proc(storage: ^Type_Storage, uninstantiated_symbol: ^Symbol, generic_types: [dynamic]^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(type_is_struct(uninstantiated_symbol.type));

  name_builder := strings.builder_make(storage.name_allocator);
  type_write_struct_name(&name_builder, uninstantiated_symbol.name, generic_types);
  name := strings.to_string(name_builder);

  is_uninstantiated_generic := false;
  for generic_type in generic_types {
    if .Uninstantiated_Generic in generic_type.flags {
      is_uninstantiated_generic = true;
      break;
    }
  }

  type := type_storage_make_type(storage, Type_Struct, .Struct, name);
  type_storage_copy_type(type, uninstantiated_symbol.type);
  if is_uninstantiated_generic {
    type.flags += {.Uninstantiated_Generic};
  } else {
    type.flags -= {.Uninstantiated_Generic};
  }
  type.generic_types = generic_types;

  cached_types_instantiated := storage.cache.instantiated[uninstantiated_symbol.type];
  append(&cached_types_instantiated, Type_Cached_Instantiated{generic_types, type});
  storage.cache.instantiated[uninstantiated_symbol.type] = cached_types_instantiated;

  return type;
}

/**
* Calculates the number of items in an AoSoA chunk for a given struct type.
*
* @param struct_type The struct type to calculate the number of items in an AoSoA chunk for.
* @return 1. The number of items in an AoSoA chunk or -1 if th chunk is too big. 2. The field that is too big if the number of items is -1.
*/
type_calculate_items_in_aosoa_chunk :: proc(struct_type: ^Type) -> (int, Type_Struct_Field) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(type_is_struct(struct_type));

  type_struct := cast(^Type_Struct) struct_type;

  // To calculate the maximum number of items we can fit in the cache line we have to find field with the biggest field.
  biggest_field: Type_Struct_Field;
  for field, i in type_struct.fields {
    field_type_size := type_size_of(field.type);
    if biggest_field.type == nil || field_type_size > type_size_of(biggest_field.type) {
      biggest_field = field;
    } 
  }

  // The mimium size will always be at least 1.
  // This is so we can properly handle emtpy structs. 
  size_of_biggest_field: uint = 1;
  if biggest_field.type != nil {
    size_of_biggest_field = type_size_of(biggest_field.type);
  }

  if size_of_biggest_field > L1_CACHE_LINE_SIZE {
    return -1, biggest_field; // Here we know that the struct has a field whose size exceeds the limit of a single cache line.
  } else {
    return cast(int) (L1_CACHE_LINE_SIZE / size_of_biggest_field), biggest_field;
  }
}

/**
* Makes a new layout struct type.
*
* @param storage              The reference to the type storage.
* @param symbol               The symbol that will represent the layout struct.
* @param collection_type      The type of collection the layout struct will be used in.
* @param base_struct          The base struct in AoS form that the layout struct is based on.
* @param layout_modifier      The layout modifier of the layout struct.
* @param number_of_elements   The number of elements (Only applicable for a fixed array collection).
* @param aosoa_chunk_symbol   The AoSoA chunk symbol that will represent a single AoSoA chunk (Only applicable for an AoSoA layout).
* @param items_in_aosoa_chunk The number of items in an AoSoA chunk (Only applicable for an AoSoA layout).
* @return The new layout struct type.
*/
type_storage_make_type_layout_struct :: proc(
  storage: ^Type_Storage,
  symbol: ^Symbol,
  collection_type: Collection_Type,
  base_struct: ^Type,
  layout_modifier: Layout_Modifier,
  number_of_elements: int,
  aosoa_chunk_symbol: ^Symbol,
  items_in_aosoa_chunk: int,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(layout_modifier != .None);

  type := type_storage_make_type_incomplete(storage, .Struct, symbol);
  type_struct := cast(^Type_Struct) base_struct;
  
  aosoa_array_chunk_type: ^Type;
  if layout_modifier == .AoSoA {
    // Example layout for 'aosoa[8]Foo':
    // struct Foo {
    //   i: int,
    //   s: string,
    // }
    // struct Array_AoSoA {
    //   base: [2]Array_AoSoA_Base,
    // }
    // struct Array_AoSoA_Base {
    //   i: int[4],
    //   s: int[4],
    // }

    assert(items_in_aosoa_chunk > 0);

    // We create the arrays on the inner base struct.
    base_fields: #soa [dynamic]Type_Struct_Field;
    for field in type_struct.fields {
      base_field_array_type := type_storage_get_or_make_type_array(storage, LAYOUT_INFO_NONE, field.type, items_in_aosoa_chunk, false);
      append_soa(&base_fields, Type_Struct_Field{.Private, false, field.name, base_field_array_type, 0});
    }

    aosoa_array_chunk_type = type_storage_make_type_incomplete(storage, .Struct, aosoa_chunk_symbol);
    aosoa_array_chunk_type.state = .Completed;
    type_complete_struct(aosoa_array_chunk_type, base_fields, LAYOUT_INFO_NONE);
    aosoa_chunk_symbol.type = aosoa_array_chunk_type;
  }

  fields: #soa [dynamic]Type_Struct_Field;
  switch collection_type {
    case .Array: {
      // For an AoSoA array we have to fully layout the data beforehand.
      // To do that we need to figure out how many items we can find inside a single cache/SIMD chunk.
      // For now this is hardcoded to 64 bytes.
      if layout_modifier == .AoSoA {
        // We have to figure out how arrays we need on the outside.
        // In the case where 'number_of_elements' is not a multiple of 'items_in_aosoa_chunk', we theoretically allocate more memory than necessary.
        number_of_needed_arrays := (number_of_elements + (items_in_aosoa_chunk - 1)) / items_in_aosoa_chunk;
        field_array_type := type_storage_get_or_make_type_array(storage, LAYOUT_INFO_NONE, aosoa_array_chunk_type, number_of_needed_arrays, false);
        append_soa(&fields, Type_Struct_Field{.Private, false, "aosoa", field_array_type, 0});
      } else {
        for field in type_struct.fields {
          field_array_type := type_storage_get_or_make_type_array(storage, LAYOUT_INFO_NONE, field.type, number_of_elements, false);
          append_soa(&fields, Type_Struct_Field{.Private, false, field.name, field_array_type, 0});
        }
      }
    }
    case .Dynamic_Array, .Slice: {
      // For AoSoA we will just have a single data pointer and do all necessary pointer arithmetic inside the code generator.
      // The reason is that we don't have a way to properly divide the single data pointer into multiple.
      // At least not in the "simple" way in the case of SoA (a single data pointer for every field).
      if layout_modifier == .AoSoA {
        aosoa_data_pointer_type := type_storage_get_or_make_type_pointer(storage, aosoa_array_chunk_type);
        append_soa(&fields, Type_Struct_Field{.Private, false, "data", aosoa_data_pointer_type, 0});
      } else {
        for field in type_struct.fields {
          field_pointer_type := type_storage_get_or_make_type_pointer(storage, field.type);
          append_soa(&fields, Type_Struct_Field{.Private, false, field.name, field_pointer_type, 0});
        }
      }
      
      append_soa(&fields, Type_Struct_Field{.Private, false, "length", storage.type_int, 0});
      if collection_type == .Dynamic_Array {
        append_soa(&fields, Type_Struct_Field{.Private, false, "capacity", storage.type_int, 0});
        append_soa(&fields, Type_Struct_Field{.Private, false, "allocator", storage.cached_runtime_types.allocator, 0});
      }
    }
    case .None: fallthrough;
    case: assert(false);
  }
  
  // We link the 'normal' base struct to the layout struct. That way we can refer to it when needed.
  type.base = base_struct;

  layout_info := Layout_Info{layout_modifier, items_in_aosoa_chunk};
  type_complete_struct(type, fields, layout_info);
  type.state = .Completed;

  cached_layout_struct := Type_Cached_Layout_Struct{collection_type, base_struct, layout_modifier, number_of_elements};
  storage.cache.layout_structs[cached_layout_struct] = type;

  return type;
}

/**
* Gets a cached constant type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param base    The base type of the constant.
* @return The constant type.
*/
type_storage_get_or_make_type_constant :: proc(storage: ^Type_Storage, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  if base.kind == .Constant {
    return base;
  }

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_constant := Type_Cached_Constant{base, is_uninstantiated_generic};
  cached_type, found := storage.cache.constants[cached_constant];
  if found do return cached_type;

  type := type_storage_make_type(storage, Type, .Constant, base.name);
  type.flags = base.flags;
  type.size = base.size;
  type.alignment = base.alignment;
  type.flags += {.Non_Modifiable};
  type.base = base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.constants[cached_constant] = type;

  return type;
}

/**
* Gets a cached pointer type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param base    The base type of the pointer.
* @return The pointer type.
*/
type_storage_get_or_make_type_pointer :: proc(storage: ^Type_Storage, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_pointer := Type_Cached_Pointer{base, is_uninstantiated_generic};
  cached_type, found := storage.cache.pointers[cached_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type, .Pointer, name);
  type.flags += {.Not_Allowed_As_Parameter_In_Pure_Function, .Has_Hash_Function, .Has_Compare_Function};
  type.size = TYPE_METRICS[Type_Kind.Pointer].size;
  type.alignment = TYPE_METRICS[Type_Kind.Pointer].alignment;
  type.base = base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.pointers[cached_pointer] = type;

  return type;
}

/**
* Gets a cached self-relative pointer type or makes a new one.
* 
* @param storage       The reference to the type storage.
* @param base          The base type of the self-relative pointer.
* @param relative_base The relative base type of the self-relative pointer.
* @return The self-relative pointer type.
*/
type_storage_get_or_make_type_self_relative_pointer :: proc(storage: ^Type_Storage, base: ^Type, relative_base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_pointer := Type_Cached_Relative_Pointer{base, relative_base, is_uninstantiated_generic};
  cached_type, found := storage.cache.self_relative_pointers[cached_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "~");
  strings.write_string(&name_builder, relative_base.name);
  strings.write_string(&name_builder, "*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Relative_Pointer, .Self_Relative_Pointer, name);
  type.flags += {
    .Not_Allowed_As_Union,
    .Not_Allowed_As_Parameter_Or_Return_Type,
    .Not_Allowed_As_Parameter_In_Pure_Function,
    .Contains_Self_Relative_Pointer_In_Value,
    .Requires_Address_Load,
  };
  type.size = type_size_of(relative_base);
  type.alignment = type_alignment_of(relative_base);
  type.base = base;
  type.relative_base = relative_base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.self_relative_pointers[cached_pointer] = type;

  return type;
}

/**
* Gets a cached offset-relative pointer type or makes a new one.
* 
* @param storage       The reference to the type storage.
* @param base          The base type of the offset-relative pointer.
* @param relative_base The relative base type of the offset-relative pointer.
* @return The offset-relative pointer type.
*/
type_storage_get_or_make_type_offset_relative_pointer :: proc(storage: ^Type_Storage, base: ^Type, relative_base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_pointer := Type_Cached_Relative_Pointer{base, relative_base, is_uninstantiated_generic};
  cached_type, found := storage.cache.offset_relative_pointers[cached_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "^");
  strings.write_string(&name_builder, relative_base.name);
  strings.write_string(&name_builder, "*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Relative_Pointer, .Offset_Relative_Pointer, name);
  type.flags += {.Has_Hash_Function, .Has_Compare_Function};
  type.size = type_size_of(relative_base);
  type.alignment = type_alignment_of(relative_base);
  type.base = base;
  type.relative_base = relative_base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.offset_relative_pointers[cached_pointer] = type;

  return type;
}

/**
* Gets a cached SoA pointer type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param base    The base type of the SoA pointer.
* @return The SoA pointer type.
*/
type_storage_get_or_make_type_soa_pointer :: proc(storage: ^Type_Storage, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(type_is_soa(base));

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_pointer := Type_Cached_Pointer{base, is_uninstantiated_generic};
  cached_type, found := storage.cache.soa_layout_pointers[cached_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "soa*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type, .SoA_Layout_Pointer, name);
  type.flags += {.Not_Allowed_As_Parameter_In_Pure_Function, .Passed_As_Pointer};
  type.size = TYPE_METRICS[Type_Kind.SoA_Layout_Pointer].size;
  type.alignment = TYPE_METRICS[Type_Kind.SoA_Layout_Pointer].alignment;
  type.base = base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.soa_layout_pointers[cached_pointer] = type;

  return type;
}

/**
* Gets a cached AoSoA pointer type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param base    The base type of the AoSoA pointer.
* @return The AoSoA pointer type.
*/
type_storage_get_or_make_type_aosoa_pointer :: proc(storage: ^Type_Storage, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(type_is_aosoa(base));

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_pointer := Type_Cached_Pointer{base, is_uninstantiated_generic};
  cached_type, found := storage.cache.aosoa_layout_pointers[cached_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "aosoa*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type, .AoSoA_Layout_Pointer, name);
  type.flags += {.Not_Allowed_As_Parameter_In_Pure_Function, .Passed_As_Pointer};
  type.size = TYPE_METRICS[Type_Kind.AoSoA_Layout_Pointer].size;
  type.alignment = TYPE_METRICS[Type_Kind.AoSoA_Layout_Pointer].alignment;
  type.base = base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.aosoa_layout_pointers[cached_pointer] = type;

  return type;
}

/**
* Gets a cached dynamic pointer type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param base    The base type of the dynamic pointer.
* @return The dynamic pointer type.
*/
type_storage_get_or_make_type_dynamic_pointer :: proc(storage: ^Type_Storage, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;

  cached_dynamic_pointer := Type_Cached_Dynamic_Pointer{base, is_uninstantiated_generic};
  cached_type, found := storage.cache.dynamic_pointers[cached_dynamic_pointer];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "dynamic*");
  strings.write_string(&name_builder, base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type, .Dynamic_Pointer, name);
  type.flags += {.Not_Allowed_As_Parameter_In_Pure_Function, .Has_Hash_Function, .Has_Compare_Function, .Passed_As_Pointer};
  type.size = TYPE_METRICS[Type_Kind.Dynamic_Pointer].size;
  type.alignment = TYPE_METRICS[Type_Kind.Dynamic_Pointer].alignment;
  type.base = base;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  base.flags += {.Interface_Used_In_Dynamic_Pointer};

  storage.cache.dynamic_pointers[cached_dynamic_pointer] = type;

  return type;
}

/**
* Gets a cached array type or makes a new one.
* 
* @param storage                 The reference to the type storage.
* @param layout_info             The layout info of the array.
* @param base                    The base type of the array.
* @param number_of_elements      The number of items in the array.
* @param has_incomplete_elements Is the array incomplete?
* @return The array type.
*/
type_storage_get_or_make_type_array :: proc(
  storage: ^Type_Storage,
  layout_info: Layout_Info,
  base: ^Type,
  number_of_elements: int,
  has_incomplete_elements: bool,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;
  layout_modifier := layout_info.modifier;

  cached_array := Type_Cached_Array{layout_modifier, base, number_of_elements, is_uninstantiated_generic};
  cached_type, found := storage.cache.arrays[cached_array];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  if layout_modifier == .SoA {
    strings.write_string(&name_builder, "soa");
  } else if layout_modifier == .AoSoA {
    strings.write_string(&name_builder, "aosoa");
  }
  strings.write_string(&name_builder, "[");
  if has_incomplete_elements {
    strings.write_string(&name_builder, "...");
  } else {
    strings.write_int(&name_builder, number_of_elements);
  }
  strings.write_string(&name_builder, "]");
  strings.write_string(&name_builder, layout_modifier == .None || type_is_generic(base) ? base.name : base.base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Array, .Array, name);
  type.flags += {.Passed_As_Pointer};
  // The size calculation depends on the number of elements we have.
  // If we are an soa array we also have to take account that our 'base' is an layout struct.
  // That means our actual base is the 'regular struct' stored in the layout struct as a base.
  size_base := layout_modifier == .None ? base : base.base;
  type.size = cast(uint) number_of_elements * type_size_of(size_base);
  type.alignment = type_alignment_of(size_base);
  type.base = base;
  type.layout_info = layout_info;
  type.number_of_elements = number_of_elements;
  type.has_incomplete_elements = has_incomplete_elements;

  if has_incomplete_elements {
    type.flags += {.Not_Allowed_As_Field, .Not_Allowed_As_Union, .Not_Allowed_As_Parameter_Or_Return_Type};
  }
  if type_contains_self_relative_pointer_in_value(base) {
    type.flags += {.Not_Allowed_As_Union, .Contains_Self_Relative_Pointer_In_Value, .Requires_Address_Load};
  }
  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  if !has_incomplete_elements {
    storage.cache.arrays[cached_array] = type;
  }
  
  return type;
}

/**
* Gets a cached dynamic array type or makes a new one.
* 
* @param storage     The reference to the type storage.
* @param layout_info The layout info of the dynamic array.
* @param base        The base type of the dynamic array.
* @return The dynamic array type.
*/
type_storage_get_or_make_type_dynamic_array :: proc(storage: ^Type_Storage, layout_info: Layout_Info, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;
  layout_modifier := layout_info.modifier;

  cached_dynamic_array := Type_Cached_Dynamic_Array{layout_modifier, base, is_uninstantiated_generic};
  cached_type, found := storage.cache.dynamic_arrays[cached_dynamic_array];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  if layout_modifier == .SoA {
    strings.write_string(&name_builder, "soa");
  } else if layout_modifier == .AoSoA {
    strings.write_string(&name_builder, "aosoa");
  }
  strings.write_string(&name_builder, "[dynamic]");
  strings.write_string(&name_builder, layout_modifier == .None || type_is_generic(base) ? base.name : base.base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Dynamic_Array, .Dynamic_Array, name);
  type.flags += {.Passed_As_Pointer};
  type.size = layout_modifier == .None ? TYPE_METRICS[Type_Kind.Dynamic_Array].size : type_size_of(base);
  type.alignment = layout_modifier == .None ? TYPE_METRICS[Type_Kind.Dynamic_Array].alignment : type_alignment_of(base);
  type.base = base;
  type.layout_info = layout_info;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.dynamic_arrays[cached_dynamic_array] = type;

  return type;
}

/**
* Gets a cached slice type or makes a new one.
* 
* @param storage     The reference to the type storage.
* @param layout_info The layout info of the slice.
* @param base        The base type of the slice.
* @return The slice type.
*/
type_storage_get_or_make_type_slice :: proc(storage: ^Type_Storage, layout_info: Layout_Info, base: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in base.flags;
  layout_modifier := layout_info.modifier;

  cached_slice := Type_Cached_Slice{layout_modifier, base, is_uninstantiated_generic};
  cached_type, found := storage.cache.slices[cached_slice];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  if layout_modifier == .SoA {
    strings.write_string(&name_builder, "soa");
  } else if layout_modifier == .AoSoA {
    strings.write_string(&name_builder, "aosoa");
  }
  strings.write_string(&name_builder, "[]");
  strings.write_string(&name_builder, layout_modifier == .None || type_is_generic(base) ? base.name : base.base.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Slice, .Slice, name);
  type.flags += {.Passed_As_Pointer};
  type.size = layout_modifier == .None ? TYPE_METRICS[Type_Kind.Slice].size : type_size_of(base);
  type.alignment = layout_modifier == .None ? TYPE_METRICS[Type_Kind.Slice].alignment : type_alignment_of(base);
  type.base = base;
  type.layout_info = layout_info;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.slices[cached_slice] = type;

  return type;
}

/**
* Gets a cached map type or makes a new one.
* 
* @param storage The reference to the type storage.
* @param key     The key type of the map.
* @param value   The value type of the map.
* @return The map type.
*/
type_storage_get_or_make_type_map :: proc(storage: ^Type_Storage, key: ^Type, value: ^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := .Uninstantiated_Generic in key.flags || .Uninstantiated_Generic in value.flags;

  cached_map := Type_Cached_Map{key, value, is_uninstantiated_generic};
  cached_type, found := storage.cache.maps[cached_map];
  if found do return cached_type;

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "map[");
  strings.write_string(&name_builder, key.name);
  strings.write_string(&name_builder, "]");
  strings.write_string(&name_builder, value.name);
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Map, .Map, name);
  type.flags += {.Passed_As_Pointer};
  type.size = TYPE_METRICS[Type_Kind.Map].size;
  type.alignment = TYPE_METRICS[Type_Kind.Map].alignment;
  type.key = key;
  type.value = value;

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  storage.cache.maps[cached_map] = type;

  return type;
}

/**
* Gets a cached tuple type or makes a new one.
* 
* @param storage  The reference to the type storage.
* @param elements The elements of the tuple.
* @return The tuple type.
*/
type_storage_get_or_make_type_tuple :: proc(storage: ^Type_Storage, elements: #soa [dynamic]Type_Tuple_Element) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  is_uninstantiated_generic := false;
  for element in elements {
    if .Uninstantiated_Generic in element.type.flags {
      is_uninstantiated_generic = true;
      break;
    }
  }

  outer: for cached_tuple in storage.cache.tuples {
    if cached_tuple.is_uninstantiated_generic != is_uninstantiated_generic do continue;
    if len(cached_tuple.elements) != len(elements) do continue;
    for cached_element, i in cached_tuple.elements {
      if cached_element.type != elements[i].type do continue outer;
    }
    delete_soa(elements);
    return cached_tuple.tuple;
  }

  name_builder := strings.builder_make(storage.name_allocator);
  strings.write_string(&name_builder, "(");
  for element, i in elements {
    strings.write_string(&name_builder, element.type.name);
    if i < len(elements) - 1{
      strings.write_string(&name_builder, ", ");
    }
  }
  strings.write_string(&name_builder, ")");
  name := strings.to_string(name_builder);

  type := type_storage_make_type(storage, Type_Tuple, .Tuple, name);
  type_complete_tuple(type, elements);

  if is_uninstantiated_generic do type.flags += {.Uninstantiated_Generic};

  append(&storage.cache.tuples, Type_Cached_Tuple{elements, is_uninstantiated_generic, type});

  return type;
}

/**
* Gets a cached routine type or makes a new one.
* 
* @param storage            The reference to the type storage.
* @param kind               The kind of the routine.
* @param calling_convention The calling convention of the routine.
* @param parameters         The parameters of the routine.
* @param return_type        The return type of the routine.
* @param routine_flags      The flags of the routine.
* @return The routine type.
*/
type_storage_get_or_make_type_routine :: proc(
  storage: ^Type_Storage,
  kind: Type_Kind,
  calling_convention: Calling_Convention,
  parameters: #soa [dynamic]Type_Routine_Parameter,
  return_type: ^Type,
  routine_flags: Type_Routine_Flags,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(kind == .Procedure || kind == .Function);

  is_uninstantiated_generic := false;
  for parameter in parameters {
    if .Uninstantiated_Generic in parameter.type.flags {
      is_uninstantiated_generic = true;
      break;
    }  
  }
  if .Uninstantiated_Generic in return_type.flags {
    is_uninstantiated_generic = true;
  }

  cache_collection := kind == .Procedure ? &storage.cache.procedures : &storage.cache.functions;

  cached_type := type_storage_get_cached_type_routine(
    cache_collection,
    calling_convention,
    parameters,
    return_type,
    routine_flags,
    is_uninstantiated_generic,
  );
  if cached_type != nil {
    // If we have found a cached function, we need to free the parameters being passed in as we do not need them anymore.
    delete_soa(parameters);
    return cached_type;
  }

  type := type_storage_make_type_routine(storage, kind, calling_convention, parameters, return_type, routine_flags);
  append(cache_collection, Type_Cached_Routine{
    calling_convention,
    parameters,
    return_type,
    routine_flags,
    is_uninstantiated_generic,
    type,
  });
  
  return type;
}

/**
* Gets a cached routine type.
* 
* @param collection                The collection holding the cached types.
* @param calling_convention        The calling convention of the routine.
* @param parameters                The parameters of the routine.
* @param return_type               The return type of the routine.
* @param routine_flags             The flags of the routine.
* @param is_uninstantiated_generic Is the routine an uninstanted generic?
* @return The cached routine type (or null if not cached).
*/
type_storage_get_cached_type_routine :: proc(
  collection: ^[dynamic]Type_Cached_Routine,
  calling_convention: Calling_Convention,
  parameters: #soa [dynamic]Type_Routine_Parameter,
  return_type: ^Type,
  routine_flags: Type_Routine_Flags,
  is_uninstantiated_generic: bool,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  outer: for cached_routine in collection {
    if cached_routine.calling_convention != calling_convention do continue;
    if cached_routine.return_type != return_type do continue;
    if cached_routine.routine_flags != routine_flags do continue;
    if cached_routine.is_uninstantiated_generic != is_uninstantiated_generic do continue;

    if len(cached_routine.parameters) != len(parameters) do continue;
    for i in 0..<len(parameters) {
      if cached_routine.parameters[i] != parameters[i] do continue outer;
    }

    return cached_routine.routine;
  }

  return nil;
}

/**
* Gets a cached instantiated type.
* 
* @param storage               The reference to the type storage.
* @param uninstantiated_symbol The symbol defining the uninstantiated type.
* @param generic_types         The generic types of the instantiated type.
* @return The cached instantiated type (or null if not cached).
*/
type_storage_get_cached_type_instantiated :: proc(storage: ^Type_Storage, uninstantiated_symbol: ^Symbol, generic_types: [dynamic]^Type) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  cached_instantiated_types, found := storage.cache.instantiated[uninstantiated_symbol.type];
  if found {
    outer: for cached_instantiated_type in cached_instantiated_types {
      for generic_type, i in cached_instantiated_type.generic_types {
        if generic_type != generic_types[i] do continue outer;
      }
      return cached_instantiated_type.instantiated_type;
    } 
  }
  return nil;
}

/**
* Gets a cached layout struct type.
* 
* @param storage            The reference to the type storage.
* @param collection_type    The collection type the layout struct is used in.
* @param base_struct        The base struct of the layout struct.
* @param layout_modifier    The layout modifier of the layout struct.
* @param number_of_elements The number of elements (Only applicable for a fixed array collection).
* @return The cached layout struct type (or null if not cached).
*/
type_storage_get_cached_type_layout_struct :: proc(
  storage: ^Type_Storage,
  collection_type: Collection_Type,
  base_struct: ^Type,
  layout_modifier: Layout_Modifier,
  number_of_elements: int,
) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);
  
  cached_layout_struct := Type_Cached_Layout_Struct{collection_type, base_struct, layout_modifier, number_of_elements};
  return storage.cache.layout_structs[cached_layout_struct];
}

/**
* Gets or makes a new generic type.
* 
* @param storage The reference to the type storage.
* @param name    The name of the generic.
* @return The generic type.
*/
type_storage_get_or_make_type_generic :: proc(storage: ^Type_Storage, name: string) -> ^Type {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  cached_generic_type := Type_Cached_Generic{name};
  cached_type, found := storage.cache.generics[cached_generic_type];
  if found do return cached_type;

  type := type_storage_make_type(storage, Type, .Generic, name);
  type.flags += {.Uninstantiated_Generic};

  storage.cache.generics[cached_generic_type] = type;

  return type;
}

/**
* Completes a struct type.
* 
* @param type        The struct type to complete.
* @param fields      The fields of the struct.
* @param layout_info The layout info of the struct.
*/
type_complete_struct :: proc(type: ^Type, fields: #soa [dynamic]Type_Struct_Field, layout_info: Layout_Info) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type_struct := cast(^Type_Struct) type;

  type_struct.size = 0;
  type_struct.alignment = 1;
  field_size_sum: uint = 0;

  // The structs starts of with having a hasher and comparer function.
  flags: Type_Flags = {.Passed_As_Pointer, .Has_Hash_Function, .Has_Compare_Function};

  fields_sliced := fields[:];
  for field, i in fields {
    field_type := field.type;
    field_type_size := type_size_of(field_type);
    field_type_alignment := type_alignment_of(field_type);

    fields_sliced[i].offset = type_align_up(type_struct.size, field_type_alignment);

    type_struct.alignment = max(type_struct.alignment, field_type_alignment);
    type_struct.size = field_type_size + type_align_up(type_struct.size, field_type_alignment);

    field_size_sum += field_type_size;

    if type_contains_self_relative_pointer_in_value(field_type) {
      flags += {.Not_Allowed_As_Union, .Contains_Self_Relative_Pointer_In_Value, .Requires_Address_Load};
    }
    if .Has_Hash_Function not_in field_type.flags {
      flags -= {.Has_Hash_Function};
    }
    if .Has_Compare_Function not_in field_type.flags {
      flags -= {.Has_Compare_Function};
    }
  }

  type_struct.size = type_align_up(type_struct.size, type_struct.alignment);

  type_struct.fields = fields;
  type_struct.layout_info = layout_info;
  type_struct.flags = flags;
}

/**
* Completes a union type.
* 
* @param type     The union type to complete.
* @param variants The variants of the union.
*/
type_complete_union :: proc(type: ^Type, variants: [dynamic]^Type) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type_union := cast(^Type_Union) type;

  biggest_type: ^Type;
  for variant in variants {
    if biggest_type == nil || biggest_type.size < type_size_of(variant) {
      biggest_type = variant;
    }
  }
  
  // A union always has a 'tag' of type 'int' as an additional first member.
  type_union.size = TYPE_METRICS[Type_Kind.Int].size;
  type_union.alignment = TYPE_METRICS[Type_Kind.Int].alignment;
  if biggest_type != nil {
    type_union.size += type_size_of(biggest_type);
    type_union.alignment = max(type_union.alignment, type_alignment_of(biggest_type));
  }

  type_union.variants = variants;
  type_union.biggest_type = biggest_type;
  type_union.flags += {.Passed_As_Pointer, .Requires_Address_Load};
}

/**
* Completes a tuple type.
* 
* @param type     The tuple type to complete.
* @param elements The elements of the tuple.
*/
type_complete_tuple :: proc(type: ^Type, elements: #soa [dynamic]Type_Tuple_Element) {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  type_tuple := cast(^Type_Tuple) type;

  type_tuple.size = 0;
  type_tuple.alignment = 1;
  element_size_sum: uint = 0;

  elements_sliced := elements[:];
  for element, i in elements {
    elements_sliced[i].offset = type_tuple.size;

    element_type := element.type;
    element_type_size := type_size_of(element_type);
    element_type_alignment := type_alignment_of(element_type);

    type_tuple.alignment = max(type_tuple.alignment, element_type_alignment);
    type_tuple.size = element_type_size + type_align_up(type_tuple.size, element_type_alignment);

    element_size_sum += element_type_size;

    if type_contains_self_relative_pointer_in_value(element_type) {
      type_tuple.flags += {.Not_Allowed_As_Union, .Contains_Self_Relative_Pointer_In_Value, .Requires_Address_Load};
    }
  }

  type_tuple.size = type_align_up(type_tuple.size, type_tuple.alignment);

  type_tuple.elements = elements;
  type_tuple.flags += {.Passed_As_Pointer};
}

/**
* Gets the index of a struct field.
* 
* @param type The struct type to search in.
* @param name The name of the field to get the index of.
* @return The index of the struct field or -1 if not found.
*/
type_get_struct_field_index :: proc(type: ^Type, name: string) -> int {
  tracy.ZoneC(ZONE_COLOR_TYPE);

  assert(type_is_struct(type));

  type_struct := cast(^Type_Struct) type;
  for i := 0; i < len(type_struct.fields); i += 1 {
    field := type_struct.fields[i];
    if field.name == name do return i;
  }

  return -1;
}

/**
* Align size up based on current size and given alignment.
*
* @param size      The current size.
* @param alignment The alignement to use.
* @return The aligned size.
*/
type_align_up :: proc(size: uint, alignment: uint) -> uint {
  return type_align_down(size + alignment - 1, alignment);
}

/**
* Align size down based on current size and given alignment.
*
* @param size      The current size.
* @param alignment The alignement to use.
* @return The aligned size.
*/
type_align_down :: proc(size: uint, alignment: uint) -> uint {
  return size & ~(alignment - 1);
}

/**
* Get the type without qualifiers.
* 
* @param type The type to get without qualifiers.
* @return The type without qualifiers.
*/
type_unqualify :: proc(type: ^Type) -> ^Type {
  return type_is_constant(type) ? type.base : type;
}

/**
* Get the type info kind for a type.
*
* @param type The type to get the type info kind of.
* @return The type info kind corresponding to the type.
*/
type_info_kind :: proc(type: ^Type) -> Type_Info_Kind {
  switch type.kind {
    case .Void: return .Void;

    case .B8, .B16, .B32, .B64, .Bool, .Untyped_Boolean: return .Boolean;
    case .I8, .I16, .I32, .I64, .Int, .U8, .U16, .U32, .U64, .UInt, .Untyped_Integer: return .Integer;
    case .F32, .F64, .Untyped_Float: return .Float;
    case .Char, .Untyped_Char: return .Char;
    case .Enumeration: return .Enumeration;

    case .String, .CString, .Untyped_String: return .String;
    case .Rawptr, .Pointer: return .Pointer;
    case .Self_Relative_Pointer, .Offset_Relative_Pointer: return .Relative_Pointer;
    case .SoA_Layout_Pointer, .AoSoA_Layout_Pointer: return .Layout_Pointer;
    case .Dynamic_Pointer: return .Dynamic_Pointer;
    case .Constant: type_info_kind(type.base);

    case .Array: return .Array;
    case .Dynamic_Array: return .Dynamic_Array;
    case .Slice: return .Slice;
    case .Map: return .Map;
    case .Tuple: return .Tuple;

    case .Struct: return .Struct;
    case .Union: return .Union;

    case .Procedure, .Function: return .Routine;

    case .Interface: return .Interface;
    case .Any: return .Any;
    case .Typeid: return .Typeid;
    
    case .None, .Untyped_Null, .Generic: return .Invalid;
  }

  return .Invalid;
}

/**
* Gets the size of a type.
* 
* @param type The type to get the size of.
* @return The size of the type.
*/
type_size_of :: proc(type: ^Type) -> uint {
  assert(type.state == .Completed);
  return type.size;
}

/**
* Gets the alignment of a type.
* 
* @param type The type to get the alignment of.
* @return The alignment of the type.
*/
type_alignment_of :: proc(type: ^Type) -> uint {
  assert(type.state == .Completed);
  return type.alignment;
}

/**
* Check whether or not a given type is the void type.
*
* @param type The type to check.
* @return True if the type is the void type otherwise false.
*/
type_is_void :: proc(type: ^Type) -> bool {
  return type.kind == .Void;
}

/**
* Check whether or not a given type is a boolean type.
*
* @param type The type to check.
* @return True if the type is a boolean type otherwise false.
*/
type_is_boolean :: proc(type: ^Type) -> bool {
  return type.kind >= .B8 && type.kind <= .Untyped_Boolean;
}

/**
* Check whether or not a given type is an integer type.
*
* @param type The type to check.
* @return True if the type is an integer type otherwise false.
*/
type_is_integer :: proc(type: ^Type) -> bool {
  return type.kind >= .I8 && type.kind <= .Untyped_Integer;
}

/**
* Check whether or not a given type is an unsigned (integer) type.
*
* @param type The type to check.
* @return True if the type is an unsigned (integer) type otherwise false.
*/
type_is_unsigned :: proc(type: ^Type) -> bool {
  return type.kind >= .U8 && type.kind <= .UInt;
}

/**
* Check whether or not a given type is a float type.
*
* @param type The type to check.
* @return True if the type is a float type otherwise false.
*/
type_is_float :: proc(type: ^Type) -> bool {
  return type.kind >= .F32 && type.kind <= .Untyped_Float;
}

/**
* Check whether or not a given type is a char type.
*
* @param type The type to check.
* @return True if the type is a char type otherwise false.
*/
type_is_char :: proc(type: ^Type) -> bool {
  return type.kind >= .Char && type.kind <= .Untyped_Char;
}

/**
* Check whether or not a given type is an enumeration type.
*
* @param type The type to check.
* @return True if the type is an enumeration type otherwise false.
*/
type_is_enumeration :: proc(type: ^Type) -> bool {
  return type.kind == .Enumeration;
}

/**
* Check whether or not a given type is an arithmetic type.
*
* @param type The type to check.
* @return True if the type is an arithmetic type otherwise false.
*/
type_is_arithmetic :: proc(type: ^Type) -> bool {
  return type.kind >= .I8 && type.kind <= .Untyped_Float || type_is_offset_relative_pointer(type);
}

/**
* Check whether or not a given type is a scalar type.
*
* @param type The type to check.
* @return True if the type is a scalar type otherwise false.
*/
type_is_scalar :: proc(type: ^Type) -> bool {
  return type.kind >= .B8 && type.kind <= .Enumeration;
}

/**
* Check whether or not a given type is a string type.
*
* @param type The type to check.
* @return True if the type is a string type otherwise false.
*/
type_is_string :: proc(type: ^Type) -> bool {
  return type.kind == .String || type.kind == .Untyped_String;
}

/**
* Check whether or not a given type is a cstring type.
*
* @param type The type to check.
* @return True if the type is a cstring type otherwise false.
*/
type_is_cstring :: proc(type: ^Type) -> bool {
  return type.kind == .CString;
}

/**
* Check whether or not a given type is a string like type.
*
* @param type The type to check.
* @return True if the type is a string like type otherwise false.
*/
type_is_string_like :: proc(type: ^Type) -> bool {
  return type_is_string(type) || type_is_cstring(type);
}

/**
* Check whether or not a given type is a raw pointer type.
*
* @param type The type to check.
* @return True if the type is a raw pointer type otherwise false.
*/
type_is_raw_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .Rawptr;
}

/**
* Check whether or not a given type is an absolute pointer type.
*
* @param type The type to check.
* @return True if the type is an absolute pointer type otherwise false.
*/
type_is_absolute_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .Pointer || type.kind == .Rawptr;
}

/**
* Check whether or not a given type is an absolute pointer like type.
*
* @param type The type to check.
* @return True if the type is an absolute pointer like type otherwise false.
*/
type_is_absolute_pointer_like :: proc(type: ^Type) -> bool {
  return type_is_absolute_pointer(type) || (type_is_routine(type) && !type_is_routine_intrinsic(type)) || type_is_untyped_null(type);
}

/**
* Check whether or not a given type is a self-relative pointer type.
*
* @param type The type to check.
* @return True if the type is a self-relative pointer type otherwise false.
*/
type_is_self_relative_pointer :: proc(type: ^Type) -> bool  {
  return type.kind == .Self_Relative_Pointer;
}

/**
* Check whether or not a given type is an offset-relative pointer type.
*
* @param type The type to check.
* @return True if the type is an offset-relative pointer type otherwise false.
*/
type_is_offset_relative_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .Offset_Relative_Pointer;
}

/**
* Check whether or not a given type is a relative pointer type.
*
* @param type The type to check.
* @return True if the type is a relative pointer type otherwise false.
*/
type_is_relative_pointer :: proc(type: ^Type) -> bool {
  return type_is_self_relative_pointer(type) || type_is_offset_relative_pointer(type);
}

/**
* Check whether or not a given type is an absolute or relative pointer like type.
*
* @param type The type to check.
* @return True if the type is an absolute or relative pointer like type otherwise false.
*/
type_is_absolute_or_relative_pointer_like :: proc(type: ^Type) -> bool {
  return type_is_absolute_pointer_like(type) || type_is_relative_pointer(type);
}

/**
* Check whether or not a given type is an SoA layout pointer type.
*
* @param type The type to check.
* @return True if the type is an SoA layout pointer type otherwise false.
*/
type_is_soa_layout_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .SoA_Layout_Pointer;
}

/**
* Check whether or not a given type is an AoSoA layout pointer type.
*
* @param type The type to check.
* @return True if the type is an AoSoA layout pointer type otherwise false.
*/
type_is_aosoa_layout_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .AoSoA_Layout_Pointer;
}

/**
* Check whether or not a given type is a layout pointer type.
*
* @param type The type to check.
* @return True if the type is a layout pointer type otherwise false.
*/
type_is_layout_pointer :: proc(type: ^Type) -> bool {
  return type_is_soa_layout_pointer(type) || type_is_aosoa_layout_pointer(type);
}

/**
* Check whether or not a given type is a dynamic pointer type.
*
* @param type The type to check.
* @return True if the type is a dynamic pointer type otherwise false.
*/
type_is_dynamic_pointer :: proc(type: ^Type) -> bool {
  return type.kind == .Dynamic_Pointer;
}

/**
* Check whether or not a given type is a constant type.
*
* @param type The type to check.
* @return True if the type is a constant type otherwise false.
*/
type_is_constant :: proc(type: ^Type) -> bool {
  return type.kind == .Constant;
}

/**
* Check whether or not a given type is an array type.
*
* @param type The type to check.
* @return True if the type is an array type otherwise false.
*/
type_is_array :: proc(type: ^Type) -> bool {
  return type.kind == .Array;
}

/**
* Check whether or not a given type is an array type and incomplete.
*
* @param type The type to check.
* @return True if the type is an array type and incomplete otherwise false.
*/
type_is_array_and_incomplete :: proc(type: ^Type) -> bool {
  return type.kind == .Array && (cast(^Type_Array) type).has_incomplete_elements;
}

/**
* Check whether or not a given type is an array type with SoA layout.
*
* @param type The type to check.
* @return True if the type is an array type with SoA layout otherwise false.
*/
type_is_array_soa :: proc(type: ^Type) -> bool {
  return type.kind == .Array && (cast(^Type_Array) type).layout_info.modifier == .SoA;
}

/**
* Check whether or not a given type is an array type with AoSoA layout.
*
* @param type The type to check.
* @return True if the type is an array type with AoSoA layout otherwise false.
*/
type_is_array_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Array && (cast(^Type_Array) type).layout_info.modifier == .AoSoA;
}

/**
* Check whether or not a given type is an array type with SoA or AosoA layout.
*
* @param type The type to check.
* @return True if the type is an array type with SoA or AosoA layout otherwise false.
*/
type_is_array_soa_or_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Array && (cast(^Type_Array) type).layout_info.modifier != .None;
}

/**
* Check whether or not a given type is the a dynamic array type.
*
* @param type The type to check.
* @return True if the type is the a dynamic array type otherwise false.
*/
type_is_dynamic_array :: proc(type: ^Type) -> bool {
  return type.kind == .Dynamic_Array;
}

/**
* Check whether or not a given type is the a dynamic array type with SoA layout.
*
* @param type The type to check.
* @return True if the type is the a dynamic array type with SoA layout otherwise false.
*/
type_is_dynamic_array_soa :: proc(type: ^Type) -> bool {
  return type.kind == .Dynamic_Array && (cast(^Type_Dynamic_Array) type).layout_info.modifier == .SoA;
}

/**
* Check whether or not a given type is the a dynamic array type with AoSoA layout.
*
* @param type The type to check.
* @return True if the type is the a dynamic array type with AoSoA layout otherwise false.
*/
type_is_dynamic_array_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Dynamic_Array && (cast(^Type_Dynamic_Array) type).layout_info.modifier == .AoSoA;
}

/**
* Check whether or not a given type is the a dynamic array type with SoA or AoSoA layout.
*
* @param type The type to check.
* @return True if the type is the a dynamic array type with SoA or AoSoA layout otherwise false.
*/
type_is_dynamic_array_soa_or_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Dynamic_Array && (cast(^Type_Dynamic_Array) type).layout_info.modifier != .None;
}

/**
* Check whether or not a given type is the a slice type.
*
* @param type The type to check.
* @return True if the type is the a slice type otherwise false.
*/
type_is_slice :: proc(type: ^Type) -> bool {
  return type.kind == .Slice;
}

/**
* Check whether or not a given type is the a slice type with SoA layout.
*
* @param type The type to check.
* @return True if the type is the a slice type with SoA layout otherwise false.
*/
type_is_slice_soa :: proc(type: ^Type) -> bool {
  return type.kind == .Slice && (cast(^Type_Slice) type).layout_info.modifier == .SoA;
}

/**
* Check whether or not a given type is the a slice type with AoSoA layout.
*
* @param type The type to check.
* @return True if the type is the a slice type with AoSoA layout otherwise false.
*/
type_is_slice_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Slice && (cast(^Type_Slice) type).layout_info.modifier == .AoSoA;
}

/**
* Check whether or not a given type is the a slice type with SoA or AoSoA layout.
*
* @param type The type to check.
* @return True if the type is the a slice type with SoA or AoSoA layout otherwise false.
*/
type_is_slice_soa_or_aosoa :: proc(type: ^Type) -> bool {
  return type.kind == .Slice && (cast(^Type_Slice) type).layout_info.modifier != .None;
}

/**
* Check whether or not a given type is an SoA collection.
*
* @param type The type to check.
* @return True if the type is an SoA collection otherwise false.
*/
type_is_soa :: proc(type: ^Type) -> bool {
  return type_is_array_soa(type) || type_is_dynamic_array_soa(type) || type_is_slice_soa(type);
}

/**
* Check whether or not a given type is an AoSoA collection.
*
* @param type The type to check.
* @return True if the type is an AoSoA collection otherwise false.
*/
type_is_aosoa :: proc(type: ^Type) -> bool {
  return type_is_array_aosoa(type) || type_is_dynamic_array_aosoa(type) || type_is_slice_aosoa(type);
}

/**
* Check whether or not a given type is an SoA or AoSoA collection.
*
* @param type The type to check.
* @return True if the type is an SoA or AoSoA collection otherwise false.
*/
type_is_soa_or_aosoa :: proc(type: ^Type) -> bool {
  return type_is_array_soa_or_aosoa(type) || type_is_dynamic_array_soa_or_aosoa(type) || type_is_slice_soa_or_aosoa(type);
}

/**
* Check whether or not a given type is a map type.
*
* @param type The type to check.
* @return True if the type is a map type otherwise false.
*/
type_is_map :: proc(type: ^Type) -> bool {
  return type.kind == .Map;
}

/**
* Check whether or not a given type is a tuple type.
*
* @param type The type to check.
* @return True if the type is a tuple type otherwise false.
*/
type_is_tuple :: proc(type: ^Type) -> bool {
  return type.kind == .Tuple;
}

/**
* Check whether or not a given type is a struct type.
*
* @param type The type to check.
* @return True if the type is a struct type otherwise false.
*/
type_is_struct :: proc(type: ^Type) -> bool {
  return type.kind == .Struct;
}

/**
* Check whether or not a given type is a union type.
*
* @param type The type to check.
* @return True if the type is a union type otherwise false.
*/
type_is_union :: proc(type: ^Type) -> bool {
  return type.kind == .Union;
}

/**
* Check whether or not a given type is a procedure type.
*
* @param type The type to check.
* @return True if the type is a procedure type otherwise false.
*/
type_is_procedure :: proc(type: ^Type) -> bool {
  return type.kind == .Procedure;
}

/**
* Check whether or not a given type is a function type.
*
* @param type The type to check.
* @return True if the type is a function type otherwise false.
*/
type_is_function :: proc(type: ^Type) -> bool {
  return type.kind == .Function;
}

/**
* Check whether or not a given type is a routine type.
*
* @param type The type to check.
* @return True if the type is a routine type otherwise false.
*/
type_is_routine :: proc(type: ^Type) -> bool {
  return type_is_procedure(type) || type_is_function(type);
}

/**
* Check whether or not a given type is an intrinsic routine type.
*
* @param type The type to check.
* @return True if the type is an intrinsic routine type otherwise false.
*/
type_is_routine_intrinsic :: proc(type: ^Type) -> bool {
  return type_is_routine(type) && .Is_Intrinsic in (cast(^Type_Routine) type).routine_flags;
}

/**
* Check whether or not a given type is an interface type.
*
* @param type The type to check.
* @return True if the type is an interface type otherwise false.
*/
type_is_interface :: proc(type: ^Type) -> bool {
  return type.kind == .Interface;
}

/**
* Check whether or not a given type is the any type.
*
* @param type The type to check.
* @return True if the type is the any type otherwise false.
*/
type_is_any :: proc(type: ^Type) -> bool {
  return type.kind == .Any;
}

/**
* Check whether or not a given type is a tuple type.
*
* @param type The type to check.
* @return True if the type is a tuple type otherwise false.
*/
type_is_typeid :: proc(type: ^Type) -> bool {
  return type.kind == .Typeid;
}

/**
* Check whether or not a given type is an untyped boolean type.
*
* @param type The type to check.
* @return True if the type is an untyped boolean type otherwise false.
*/
type_is_untyped_boolean :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_Boolean;
}

/**
* Check whether or not a given type is an untyped integer type.
*
* @param type The type to check.
* @return True if the type is an untyped integer type otherwise false.
*/
type_is_untyped_integer :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_Integer;
}

/**
* Check whether or not a given type is an untyped float type.
*
* @param type The type to check.
* @return True if the type is an untyped float type otherwise false.
*/
type_is_untyped_float :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_Float;
}

/**
* Check whether or not a given type is an untyped char type.
*
* @param type The type to check.
* @return True if the type is an untyped char type otherwise false.
*/
type_is_untyped_char :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_Char;
}

/**
* Check whether or not a given type is an untyped string type.
*
* @param type The type to check.
* @return True if the type is an untyped string type otherwise false.
*/
type_is_untyped_string :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_String;
}

/**
* Check whether or not a given type is an untyped null type.
*
* @param type The type to check.
* @return True if the type is an untyped null type otherwise false.
*/
type_is_untyped_null :: proc(type: ^Type) -> bool {
  return type.kind == .Untyped_Null;
}

/**
* Check whether or not a given type is an untyped type.
*
* @param type The type to check.
* @return True if the type is an untyped type otherwise false.
*/
type_is_untyped :: proc(type: ^Type) -> bool {
  return type_is_untyped_boolean(type) ||
  type_is_untyped_integer(type) ||
  type_is_untyped_float(type) ||
  type_is_untyped_char(type) ||
  type_is_untyped_string(type) ||
  type_is_untyped_null(type);
}

/**
* Check whether or not a given type is a generic type.
*
* @param type The type to check.
* @return True if the type is a generic type otherwise false.
*/
type_is_generic :: proc(type: ^Type) -> bool {
  return type.kind == .Generic;
}

/**
* Check if a type constains a self-relative pointer in its value.
* 
* @param type The type to check.
* @return True if the type constains a self-relative pointer in its value otherwise false.
*/
type_contains_self_relative_pointer_in_value :: proc(type: ^Type) -> bool {
  return .Contains_Self_Relative_Pointer_In_Value in type.flags;
}

/**
* Check if a routine has an implicit context parameter.
* 
* @param type The type to check.
* @return True if the routine has an implicit context parameter otherwise false.
*/
type_routine_has_context :: proc(type_routine: ^Type_Routine) -> bool {
  return type_routine.calling_convention == .Nox && .Is_Pure_Function not_in type_routine.routine_flags;
}

/**
* Makes routine flags for a routine type.
* @param has_params    Has the routine a 'params' parameter?
* @param has_c_varargs Has the routine an c varargs parameter?
* @param is_intrinsic  Is the routine an intrinsic?
* @param is_method     Is the routine a method?
* @param is_pure       Is the routine a pure function?
* @return The routine flags.
*/
type_make_routine_flags :: proc(has_params: bool, has_c_varargs: bool, is_intrinsic: bool, is_method: bool, is_pure: bool) -> Type_Routine_Flags {
  routine_flags: Type_Routine_Flags;
  routine_flags += has_params ? {.Has_Params} : {};
  routine_flags += has_c_varargs ? {.Has_C_Varargs} : {};
  routine_flags += is_intrinsic ? {.Is_Intrinsic} : {};
  routine_flags += is_method ? {.Is_Method} : {};
  routine_flags += is_pure ? {.Is_Pure_Function} : {};
  return routine_flags;
}

/**
* Writes a routine type with a name to a string builder.
*
* @param builder The string builder to write to.
* @param type    The routine type to write.
* @param name    The name of the routine to include.
*/
type_write_routine_name :: proc(builder: ^strings.Builder, type: ^Type, name: string) {
  type_routine := cast(^Type_Routine) type;

  type_write_routine_name_raw(
    builder,
    type.kind,
    type_routine.calling_convention,
    type_routine.parameters,
    type_routine.return_type,
    type_routine.routine_flags,
    name,
  );
}

/**
* Writes a struct with a name to a string builder.
*
* @param builder       The string builder to write to.
* @param name          The name of the struct.
* @param generic_types The generic types of the struct.
*/
type_write_struct_name :: proc(
  builder: ^strings.Builder,
  name: string,
  generic_types: [dynamic]^Type,
) {
  strings.write_string(builder, name);
  if len(generic_types) > 0 {
    strings.write_string(builder, "!(");
    for generic_type, i in generic_types {
      strings.write_string(builder, generic_type.name);
      if i < len(generic_types) - 1 {
        strings.write_string(builder, ", ");
      }
    }
    strings.write_string(builder, ")");
  }
}

/**
* Writes a routine type with a name to a string builder.
*
* @param builder            The string builder to write to.
* @param kind               The kind of the routine.
* @param calling_convention The calling convention of the routine.
* @param parameters         The parameters of the routine.
* @param return_type        The return type of the routine.
* @param flags              The flags of the routine.
* @param name               The name of the routine to include.
*/
type_write_routine_name_raw :: proc(
  builder: ^strings.Builder,
  kind: Type_Kind,
  calling_convention: Calling_Convention,
  parameters: #soa [dynamic]Type_Routine_Parameter,
  return_type: ^Type,
  routine_flags: Type_Routine_Flags,
  name: string,
) {
  if .Is_Intrinsic in routine_flags {
    strings.write_string(builder, "@intrinsic ");
  } else if .Is_Method in routine_flags {
    strings.write_string(builder, "@method ");
  }

  if .Is_Pure_Function in routine_flags {
    strings.write_string(builder, "pure ");
  }
  strings.write_string(builder, kind == .Procedure ? "proc" : "func");

  switch calling_convention {
    case .Nox: // We don't print the 'Nox' calling convention as it is the standard.
    case .No_Context: strings.write_string(builder, " \"nocontext\" ");
    case .C: strings.write_string(builder, " \"c\" ");
    case .Std_Call: strings.write_string(builder, " \"stdcall\" ");
    case .Fast_Call: strings.write_string(builder, " \"fastcall\" ");
    case .Win64: strings.write_string(builder, " \"win64\" ");
  }

  if name != "" {
    strings.write_string(builder, " ");
    strings.write_string(builder, name);
  }

  strings.write_string(builder, "(");
  for parameter, i in parameters {
    if .Has_Params in routine_flags && i == len(parameters) - 1 {
      strings.write_string(builder, "params ");
    }
    strings.write_string(builder, parameter.type.name);
    if i < len(parameters) - 1 || .Has_C_Varargs in routine_flags {
      strings.write_string(builder, ", ");
    }
  }
  if .Has_C_Varargs in routine_flags {
    strings.write_string(builder, "...");  
  }
  strings.write_string(builder, ")");

  if return_type != nil && !type_is_void(return_type) {
    strings.write_string(builder, " -> ");
    strings.write_string(builder, return_type.name);
  }
}

/**
* Writes a all names of given types as a list.
*
* @param types The types whose names to write.
* @return The final list of names.
*/
type_get_type_names :: proc(types: [dynamic]^Type) -> string {
  builder := strings.builder_make_len(0, context.temp_allocator);
  for type, i in types {
    strings.write_string(&builder, "    ");
    strings.write_string(&builder, type.name);
    if i < len(types) - 1 {
      strings.write_string(&builder, "\n");
    }
  }
  return strings.to_string(builder);
}
