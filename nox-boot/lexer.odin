package nox

import "core:c/libc"
import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"

/**
* Represents a position in source code.
*/
Source_Position :: struct {
  file: string, // The file of the position.
  line: u32,    // The line of the position.
  column: u32,  // The column of the position.
}

/**
* Represents the special builtin source position
*/
SOURCE_POSITION_BUILTIN :: Source_Position{"<builtin>", 0, 0 };

/**
* The kind of a token.
*/
Token_Kind :: enum {
  Invalid,

  Eof,

  // Single/Misc.
  Colon,
  Dot,
  Comma,
  Semicolon,
  Negate,
  Not,
  Question,
  At,
  Pound,
  Arrow,
  Ellipsis,
  Left_Parentheses,
  Right_Parentheses,
  Left_Brace,
  Right_Brace,
  Left_Bracket,
  Right_Bracket,

  // Literals.
  Boolean,
  Integer,
  Float,
  Character,
  String,
  Name,

  // Multiplicative precedence.
  First_Multiplicative,
  Multiply = First_Multiplicative,
  Divide,
  Modulo,
  And,
  Left_Shift,
  Right_Shift,
  Last_Multiplicative = Right_Shift,

  // Additive precedence.
  First_Additive,
  Add = First_Additive,
  Subtract,
  Xor,
  Or,
  Last_Additive = Or,

  // Comperative precedence.
  First_Comparison,
  Equal = First_Comparison,
  Not_Equal,
  Less_Than,
  Greater_Than,
  Less_Than_Equal,
  Greater_Than_Equal,
  Last_Comparison = Greater_Than_Equal,

  // Special comparison.
  And_And,
  Or_Or,

  // Assignment.
  First_Assign,
  Assign = First_Assign,
  Add_Assign,
  Subtract_Assign,
  Or_Assign,
  And_Assign,
  Xor_Assign,
  Left_Shift_Assign,
  Right_Shift_Assign,
  Multiply_Assign,
  Divide_Assign,
  Modulo_Assign,
  Last_Assign = Modulo_Assign,

  // Special assignment.
  Increment,
  Decrement,
  Colon_Assign,

  // Keywords.
  Keyword_True,
  Keyword_False,
  Keyword_Import,
  Keyword_Public,
  Keyword_Internal,
  Keyword_Private,
  Keyword_Extern,
  Keyword_Const,
  Keyword_Global,
  Keyword_Enum,
  Keyword_Struct,
  Keyword_Union,
  Keyword_Proc,
  Keyword_Func,
  Keyword_Pure,
  Keyword_Interface,
  Keyword_Implement,
  Keyword_Type_Alias,
  Keyword_Type_Define,
  Keyword_Composite,
  Keyword_Dynamic,
  Keyword_Map,
  Keyword_SoA,
  Keyword_AoSoA,
  Keyword_Params,
  Keyword_Where,
  Keyword_Return,
  Keyword_Break,
  Keyword_Continue,
  Keyword_Fallthrough,
  Keyword_Defer,
  Keyword_Push_Context,
  Keyword_If,
  Keyword_Then,
  Keyword_Else,
  Keyword_For,
  Keyword_Foreach,
  Keyword_In,
  Keyword_Switch,
  Keyword_Case,
  Keyword_Cast,
  Keyword_Size_Of,
  Keyword_Typeid_Of,
  Keyword_Type_Info_Of,
}

KEYWORD_TRUE :: "true";
KEYWORD_FALSE :: "false";
KEYWORD_IMPORT :: "import";
KEYWORD_PUBLIC :: "public";
KEYWORD_INTERNAL :: "internal";
KEYWORD_PRIVATE :: "private";
KEYWORD_EXTERN :: "extern";
KEYWORD_CONST :: "const";
KEYWORD_GLOBAL :: "global";
KEYWORD_ENUM :: "enum";
KEYWORD_STRUCT :: "struct";
KEYWORD_UNION :: "union";
KEYWORD_PROC :: "proc";
KEYWORD_FUNC :: "func";
KEYWORD_PURE :: "pure";
KEYWORD_INTERFACE :: "interface";
KEYWORD_IMPLEMENT :: "implement";
KEYWORD_TYPE_ALIAS :: "type_alias";
KEYWORD_TYPE_DEFINE :: "type_define";
KEYWORD_COMPOSITE :: "composite";
KEYWORD_DYNAMIC :: "dynamic";
KEYWORD_MAP :: "map";
KEYWORD_SOA :: "soa";
KEYWORD_AOSOA :: "aosoa";
KEYWORD_PARAMS :: "params";
KEYWORD_WHERE :: "where";
KEYWORD_RETURN :: "return";
KEYWORD_BREAK :: "break";
KEYWORD_CONTINUE :: "continue";
KEYWORD_FALLTHROUGH :: "fallthrough";
KEYWORD_DEFER :: "defer";
KEYWORD_PUSH_CONTEXT :: "push_context";
KEYWORD_IF :: "if";
KEYWORD_THEN :: "then";
KEYWORD_ELSE :: "else";
KEYWORD_FOR :: "for";
KEYWORD_FOREACH :: "foreach";
KEYWORD_IN :: "in";
KEYWORD_SWITCH :: "switch";
KEYWORD_CASE :: "case";
KEYWORD_CAST :: "cast";
KEYWORD_SIZE_OF :: "size_of";
KEYWORD_TYPEID_OF :: "typeid_of";
KEYWORD_TYPE_INFO_OF :: "type_info_of";

KEYWORDS: map[string]Token_Kind = {
  KEYWORD_TRUE = .Keyword_True,
  KEYWORD_FALSE = .Keyword_False,
  KEYWORD_IMPORT = .Keyword_Import,
  KEYWORD_PUBLIC = .Keyword_Public,
  KEYWORD_INTERNAL = .Keyword_Internal,
  KEYWORD_PRIVATE = .Keyword_Private,
  KEYWORD_EXTERN = .Keyword_Extern,
  KEYWORD_CONST = .Keyword_Const,
  KEYWORD_GLOBAL = .Keyword_Global,
  KEYWORD_ENUM = .Keyword_Enum,
  KEYWORD_STRUCT = .Keyword_Struct,
  KEYWORD_UNION = .Keyword_Union,
  KEYWORD_PROC = .Keyword_Proc,
  KEYWORD_FUNC = .Keyword_Func,
  KEYWORD_PURE = .Keyword_Pure,
  KEYWORD_INTERFACE = .Keyword_Interface,
  KEYWORD_IMPLEMENT = .Keyword_Implement,
  KEYWORD_TYPE_ALIAS = .Keyword_Type_Alias,
  KEYWORD_TYPE_DEFINE = .Keyword_Type_Define,
  KEYWORD_COMPOSITE = .Keyword_Composite,
  KEYWORD_DYNAMIC = .Keyword_Dynamic,
  KEYWORD_MAP = .Keyword_Map,
  KEYWORD_SOA = .Keyword_SoA,
  KEYWORD_AOSOA = .Keyword_AoSoA,
  KEYWORD_PARAMS = .Keyword_Params,
  KEYWORD_WHERE = .Keyword_Where,
  KEYWORD_RETURN = .Keyword_Return,
  KEYWORD_BREAK = .Keyword_Break,
  KEYWORD_CONTINUE = .Keyword_Continue,
  KEYWORD_FALLTHROUGH = .Keyword_Fallthrough,
  KEYWORD_DEFER = .Keyword_Defer,
  KEYWORD_PUSH_CONTEXT = .Keyword_Push_Context,
  KEYWORD_IF = .Keyword_If,
  KEYWORD_THEN = .Keyword_Then,
  KEYWORD_ELSE = .Keyword_Else,
  KEYWORD_FOR = .Keyword_For,
  KEYWORD_FOREACH = .Keyword_Foreach,
  KEYWORD_IN = .Keyword_In,
  KEYWORD_SWITCH = .Keyword_Switch,
  KEYWORD_CASE = .Keyword_Case,
  KEYWORD_CAST = .Keyword_Cast,
  KEYWORD_SIZE_OF = .Keyword_Size_Of,
  KEYWORD_TYPEID_OF = .Keyword_Typeid_Of,
  KEYWORD_TYPE_INFO_OF = .Keyword_Type_Info_Of,
}

/**
* Holds the names corresponding to a token kind.
*/
TOKEN_KIND_NAMES := [?]string{
  Token_Kind.Invalid = "Invalid",

  Token_Kind.Eof = "Eof",

  Token_Kind.Colon = ":",
  Token_Kind.Dot = ".",
  Token_Kind.Comma = ",",
  Token_Kind.Semicolon = ";",
  Token_Kind.Negate = "~",
  Token_Kind.Not = "!",
  Token_Kind.Question = "?",
  Token_Kind.At = "@",
  Token_Kind.Pound = "#",
  Token_Kind.Arrow = "->",
  Token_Kind.Ellipsis = "...",
  Token_Kind.Left_Parentheses = "(",
  Token_Kind.Right_Parentheses = ")",
  Token_Kind.Left_Brace = "{",
  Token_Kind.Right_Brace = "}",
  Token_Kind.Left_Bracket = "[",
  Token_Kind.Right_Bracket = "]",

  Token_Kind.Boolean = "boolean",
  Token_Kind.Integer = "integer",
  Token_Kind.Float = "float",
  Token_Kind.Character = "character",
  Token_Kind.String = "string",
  Token_Kind.Name = "name",

  Token_Kind.Multiply = "*",
  Token_Kind.Divide = "/",
  Token_Kind.Modulo = "%",
  Token_Kind.And = "&",
  Token_Kind.Left_Shift = "<<",
  Token_Kind.Right_Shift = ">>",

  Token_Kind.Add = "+",
  Token_Kind.Subtract = "-",
  Token_Kind.Xor = "^",
  Token_Kind.Or = "|",

  Token_Kind.Equal = "==",
  Token_Kind.Not_Equal = "!=",
  Token_Kind.Less_Than = "<",
  Token_Kind.Greater_Than = ">",
  Token_Kind.Less_Than_Equal = "<=",
  Token_Kind.Greater_Than_Equal = ">=",

  Token_Kind.And_And = "&&",
  Token_Kind.Or_Or = "||",

  Token_Kind.Assign = "=",
  Token_Kind.Add_Assign = "+=",
  Token_Kind.Subtract_Assign = "-=",
  Token_Kind.Or_Assign = "|=",
  Token_Kind.And_Assign = "&=",
  Token_Kind.Xor_Assign = "^=",
  Token_Kind.Left_Shift_Assign = "<<=",
  Token_Kind.Right_Shift_Assign = ">>=",
  Token_Kind.Multiply_Assign = "*=",
  Token_Kind.Divide_Assign = "/=",
  Token_Kind.Modulo_Assign = "%=",

  Token_Kind.Increment = "++",
  Token_Kind.Decrement = "--",
  Token_Kind.Colon_Assign = ":=",

  Token_Kind.Keyword_True = KEYWORD_TRUE,
  Token_Kind.Keyword_False = KEYWORD_FALSE,
  Token_Kind.Keyword_Import = KEYWORD_IMPORT,
  Token_Kind.Keyword_Public = KEYWORD_PUBLIC,
  Token_Kind.Keyword_Internal = KEYWORD_INTERNAL,
  Token_Kind.Keyword_Private = KEYWORD_PRIVATE,
  Token_Kind.Keyword_Extern = KEYWORD_EXTERN,
  Token_Kind.Keyword_Const = KEYWORD_CONST,
  Token_Kind.Keyword_Global = KEYWORD_GLOBAL,
  Token_Kind.Keyword_Enum = KEYWORD_ENUM,
  Token_Kind.Keyword_Struct = KEYWORD_STRUCT,
  Token_Kind.Keyword_Union = KEYWORD_UNION,
  Token_Kind.Keyword_Proc = KEYWORD_PROC,
  Token_Kind.Keyword_Func = KEYWORD_FUNC,
  Token_Kind.Keyword_Pure = KEYWORD_PURE,
  Token_Kind.Keyword_Interface = KEYWORD_INTERFACE,
  Token_Kind.Keyword_Implement = KEYWORD_IMPLEMENT,
  Token_Kind.Keyword_Type_Alias = KEYWORD_TYPE_ALIAS,
  Token_Kind.Keyword_Type_Define = KEYWORD_TYPE_DEFINE,
  Token_Kind.Keyword_Composite = KEYWORD_COMPOSITE,
  Token_Kind.Keyword_Dynamic = KEYWORD_DYNAMIC,
  Token_Kind.Keyword_Map = KEYWORD_MAP,
  Token_Kind.Keyword_SoA = KEYWORD_SOA,
  Token_Kind.Keyword_AoSoA = KEYWORD_AOSOA,
  Token_Kind.Keyword_Params = KEYWORD_PARAMS,
  Token_Kind.Keyword_Where = KEYWORD_WHERE,
  Token_Kind.Keyword_Return = KEYWORD_RETURN,
  Token_Kind.Keyword_Break = KEYWORD_BREAK,
  Token_Kind.Keyword_Continue = KEYWORD_CONTINUE,
  Token_Kind.Keyword_Fallthrough = KEYWORD_FALLTHROUGH,
  Token_Kind.Keyword_Defer = KEYWORD_DEFER,
  Token_Kind.Keyword_Push_Context = KEYWORD_PUSH_CONTEXT,
  Token_Kind.Keyword_If = KEYWORD_IF,
  Token_Kind.Keyword_Then = KEYWORD_THEN,
  Token_Kind.Keyword_Else = KEYWORD_ELSE,
  Token_Kind.Keyword_For = KEYWORD_FOR,
  Token_Kind.Keyword_Foreach = KEYWORD_FOREACH,
  Token_Kind.Keyword_In = KEYWORD_IN,
  Token_Kind.Keyword_Switch = KEYWORD_SWITCH,
  Token_Kind.Keyword_Case = KEYWORD_CASE,
  Token_Kind.Keyword_Cast = KEYWORD_CAST,
  Token_Kind.Keyword_Size_Of = KEYWORD_SIZE_OF,
  Token_Kind.Keyword_Typeid_Of = KEYWORD_TYPEID_OF,
  Token_Kind.Keyword_Type_Info_Of = KEYWORD_TYPE_INFO_OF,
}

/**
* Holds the corresponding binary token kinds for binary assignments.
*/
ASSIGN_TOKEN_TO_BINARY_TOKEN := [?]Token_Kind{
  Token_Kind.Add_Assign = .Add,
  Token_Kind.Subtract_Assign = .Subtract,
  Token_Kind.Or_Assign = .Or,
  Token_Kind.And_Assign = .And,
  Token_Kind.Xor_Assign = .Xor,
  Token_Kind.Left_Shift_Assign = .Left_Shift,
  Token_Kind.Right_Shift_Assign = .Right_Shift,
  Token_Kind.Multiply_Assign = .Multiply,
  Token_Kind.Divide_Assign = .Divide,
  Token_Kind.Modulo_Assign = .Modulo,
}

/**
* Represents the value of a token.
*/
Token_Value :: union {
  bool,
  u64,
  f64,
  rune,
  string,
}

/**
* Represents a single token in a token stream.
*/
Token :: struct {
  kind: Token_Kind,          // The kind of the token.
  value: Token_Value,        // The value of the token.
  position: Source_Position, // The source position of the token.
}

/**
* Represents the state of the Lexer.
*/
Lexer :: struct {
  stream: string,            // The source stream.
  stream_byte_position: int, // The byte position inside the source stream.
  token: Token,              // The current token inside the stream.
}

/*
* Maps a char to their numerical digit.
*/
CHAR_TO_DIGIT := [256]u64{
  '0' = 0,
  '1' = 1,
  '2' = 2,
  '3' = 3,
  '4' = 4,
  '5' = 5,
  '6' = 6,
  '7' = 7,
  '8' = 8,
  '9' = 9,
  'a' = 10, 'A' = 10,
  'b' = 11, 'B' = 11,
  'c' = 12, 'C' = 12,
  'd' = 13, 'D' = 13,
  'e' = 14, 'E' = 14,
  'f' = 15, 'F' = 15,
}

/*
* Maps a char to their corresponding escape character.
* Currently our support for espace sequences is very limited.
*/
ESCAPE_TO_CHAR := [256]rune{
  '0' = 0,
  'a' = '\a',
  'b' = '\b',
  'e' = '\e',
  'f' = '\f',
  'n' = '\n',
  'r' = '\r',
  't' = '\t',
  'v' = '\v',
  '\'' = '\'',
  '"' = '"',
  '\\' = '\\',
}

/**
* Makes a new lexer.
*
* @param file   The file the stream came from.
* @param stream The stream for the lexer.
* @return The new lexer.
*/
lexer_make :: proc(file: string, stream: string) -> Lexer {
  lexer: Lexer;
  lexer_init(&lexer, file, stream);
  return lexer;
}

/**
* Initializes a lexer with given properties.
*
* @param lexer  The lexer to initialize.
* @param file   The file the stream came from.
* @param stream The stream for the lexer.
*/
lexer_init :: proc(lexer: ^Lexer, file: string, stream: string) {
  lexer.stream = stream;
  lexer.stream_byte_position = 0;
  lexer.token = { };
  lexer.token.position.file = file;
  lexer.token.position.line = 1;
  lexer.token.position.column = 1;
}

/**
* Gets a string representation for a token kind.
*
* @param kind The token kind to get the string representation for.
* @return The string representation of the token kind.
*/
lexer_token_kind_name :: proc(kind: Token_Kind) -> string {
  return TOKEN_KIND_NAMES[kind];
}

/**
* Gets a string containing information about the current token in a lexer.
*
* @param lexer The lexer to get the token info of.
* @return The token info.
*/
lexer_token_info :: proc(lexer: ^Lexer) -> string {
  if lexer.token.kind == .Name {
    return lexer.token.value.(string);
  } else {
    return lexer_token_kind_name(lexer.token.kind);
  }
}

/**
* Checks whether or not the current lexer token is of a specific kind.
* 
* @param lexer The lexer to check the current token of.
* @param kind  The kind to check for.
* @return True when the current lexer token is of the given kind otherwise false.
*/
lexer_is_token :: proc(lexer: ^Lexer, kind: Token_Kind) -> bool {
  return lexer.token.kind == kind;
}

/**
* Checks whether or not the current lexer token is an assignment operator.
* 
* @param lexer The lexer to check the current token of.
* @return True when the current lexer token is an assignment operator otherwise false.
*/
lexer_is_assign_operator :: proc(lexer: ^Lexer) -> bool {
  return lexer.token.kind >= .First_Assign && lexer.token.kind <= .Last_Assign;
}

/**
* Checks whether or not the current lexer token is a comparison operator.
* 
* @param lexer The lexer to check the current token of.
* @return True when the current lexer token is a comparison operator otherwise false.
*/
lexer_is_comparison_operator :: proc(lexer: ^Lexer) -> bool {
  return lexer.token.kind >= .First_Comparison && lexer.token.kind <= .Last_Comparison;
}

/**
* Checks whether or not the current lexer token is an additive operator.
* 
* @param lexer The lexer to check the current token of.
* @return True when the current lexer token is an additive operator otherwise false.
*/
lexer_is_additive_operator :: proc(lexer: ^Lexer) -> bool {
  return lexer.token.kind >= .First_Additive && lexer.token.kind <= .Last_Additive;
}

/**
* Checks whether or not the current lexer token is a multiplicative operator.
* 
* @param lexer The lexer to check the current token of.
* @return True when the current lexer token is a multiplicative operator otherwise false.
*/
lexer_is_multiplicative_operator :: proc(lexer: ^Lexer) -> bool {
  return lexer.token.kind >= .First_Multiplicative && lexer.token.kind <= .Last_Multiplicative;
}

/**
* Checks whether or not the current lexer token is a unary operator.
* 
* @param lexer The lexer to check the current token of.
* @return True when the current lexer token is a unary operator otherwise false.
*/
lexer_is_unary_operator :: proc(lexer: ^Lexer) -> bool {
  return lexer_is_token(lexer, .Add) ||
    lexer_is_token(lexer, .Subtract) ||
    lexer_is_token(lexer, .Multiply) ||
    lexer_is_token(lexer, .And) ||
    lexer_is_token(lexer, .Negate) ||
    lexer_is_token(lexer, .Not) ||
    lexer_is_token(lexer, .Increment) ||
    lexer_is_token(lexer, .Decrement);
}

/**
* Trys to match a given token kind and lexes the next one on success.
* 
* @param lexer The lexer to match the current token of.
* @param kind  The token kind to match.
* @return True when the current lexer token is of the given kind otherwise false.
*/
lexer_match_token :: proc(lexer: ^Lexer, kind: Token_Kind) -> bool {
  if lexer_is_token(lexer, kind) {
    lexer_get_next_token(lexer);
    return true;
  } else {
    return false;
  }
}

/**
* Expects a given token kind and lexes the next one on success.
* 
* @param lexer The lexer to expect the current token of.
* @param kind  The token kind to expect.
* @return True when the current lexer token is of the given kind otherwise false.
*/
lexer_expect_token :: proc(lexer: ^Lexer, kind: Token_Kind) -> bool {
  if lexer_is_token(lexer, kind) {
   lexer_get_next_token(lexer);
    return true;
  } else {
    report_error_fatal(lexer.token.position, "Expected token '%v'. Got '%v'", lexer_token_kind_name(kind), lexer_token_info(lexer));
    return false;
  }
}

/**
* Gets/Lexes the next token in the stream of a lexer.
* 
* @param lexer The lexer to get the next token of.
* @return The newly lexed token.
*/
lexer_get_next_token :: proc(lexer: ^Lexer) -> Token {
  lexer.token.value = nil;
  lexer.token.kind = .Invalid;

  lexer_skip_whitespace_and_comments(lexer);

  // Check for end.
  if lexer.stream_byte_position >= len(lexer.stream) {
    lexer.token.kind = .Eof;
    return lexer.token;
  }

  start_column := lexer.token.position.column;
  token_start := lexer.stream_byte_position;
  character := lexer_consume_character(lexer);

  switch (character) {
    case '0'..='9': {
      for unicode.is_digit(character) {
        character = lexer_consume_character(lexer);
      }
      
      is_float := character == '.' || unicode.to_lower(character) == 'e';
      // If we encounter a alpha character after the '.', we know we have a regular member access on a literal like: 123.foo().
      if unicode.is_alpha(lexer_peek_next_character(lexer)) {
        is_float = false;
      }

      // We reset the stream position to the start for proper scanning.
      lexer.stream_byte_position = token_start;

      if is_float {
        lexer_scan_float(lexer);
      } else {
        lexer_scan_integer(lexer);
      }
    }

    case '\'': {
      lexer_scan_character(lexer);
    }
    case '"': {
      lexer_scan_string(lexer, false);
    }
    case '`': {
      lexer_scan_string(lexer, true);
    }

    case '.': {
      lexer.token.kind = .Dot;
      character = lexer_peek_next_character(lexer);
      second_character := lexer_peek_second_next_character(lexer);
      if unicode.is_digit(character) {
        // We have to "reset" the stream to the start position because the "." we have consumed is needed for properly scanning the float.
        lexer.stream_byte_position = token_start;
        lexer_scan_float(lexer);
      } else if character == '.' && second_character == '.' {
        lexer.token.kind = .Ellipsis;
        lexer_consume_character(lexer);
        lexer_consume_character(lexer);
      }
    }

    case ',': { lexer.token.kind = .Comma; }
    case ';': { lexer.token.kind = .Semicolon; }
    case '~': { lexer.token.kind = .Negate; }
    case '?': { lexer.token.kind = .Question; }
    case '@': { lexer.token.kind = .At; }
    case '#': { lexer.token.kind = .Pound; }
    case '(': { lexer.token.kind = .Left_Parentheses; }
    case ')': { lexer.token.kind = .Right_Parentheses; }
    case '[': { lexer.token.kind = .Left_Bracket; }
    case ']': { lexer.token.kind = .Right_Bracket; }
    case '{': { lexer.token.kind = .Left_Brace; }
    case '}': { lexer.token.kind = .Right_Brace; }
    case '!': {
      lexer.token.kind = .Not;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Not_Equal;
      }
    }
    case '=': {
      lexer.token.kind = .Assign;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Equal;
      }
    }
    case ':': {
      lexer.token.kind = .Colon;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Colon_Assign;
      }
    }
    case '*': {
      lexer.token.kind = .Multiply;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Multiply_Assign;
      }
    }
    case '/': {
      lexer.token.kind = .Divide;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Divide_Assign;
      }
    }
    case '%': {
      lexer.token.kind = .Modulo;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Modulo_Assign;
      }
    }
    case '^': {
      lexer.token.kind = .Xor;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Xor_Assign;
      }
    }
    case '+': {
      lexer.token.kind = .Add;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Add_Assign;
      } else if lexer_peek_next_character(lexer) == '+' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Increment;
      }
    }
    case '-': {
      lexer.token.kind = .Subtract;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Subtract_Assign;
      } else if lexer_peek_next_character(lexer) == '-' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Decrement;
      } else if lexer_peek_next_character(lexer) == '>' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Arrow;
      }
    }
    case '&': {
      lexer.token.kind = .And;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .And_Assign;
      } else if lexer_peek_next_character(lexer) == '&' {
        lexer_consume_character(lexer);
        lexer.token.kind = .And_And;
      }
    }
    case '|': {
      lexer.token.kind = .Or;
      if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Or_Assign;
      } else if lexer_peek_next_character(lexer) == '|' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Or_Or;
      }
    }
    case '<': {
      lexer.token.kind = .Less_Than;
      if lexer_peek_next_character(lexer) == '<' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Left_Shift;
        if lexer_peek_next_character(lexer) == '=' {
          lexer_consume_character(lexer);
          lexer.token.kind = .Left_Shift_Assign;
        }
      } else if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Less_Than_Equal;
      }
    }
    case '>': {
      lexer.token.kind = .Greater_Than;
      if lexer_peek_next_character(lexer) == '>' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Right_Shift;
        if lexer_peek_next_character(lexer) == '=' {
          lexer_consume_character(lexer);
          lexer.token.kind = .Right_Shift_Assign;
        }
      } else if lexer_peek_next_character(lexer) == '=' {
        lexer_consume_character(lexer);
        lexer.token.kind = .Greater_Than_Equal;
      }
    }

    case: {
      // Currently we interpret everything else as a valid character for an identifier/keyword.
      character = lexer_peek_next_character(lexer);
      for unicode.is_letter(character) || unicode.is_number(character) || character == '_' {
        lexer_consume_character(lexer);
        character = lexer_peek_next_character(lexer);
      }
      token_end := lexer.stream_byte_position;

      str := lexer.stream[token_start:token_end];

      lexer.token.value = str;
      lexer.token.position.column = start_column;

      keyword_kind, is_keyword := KEYWORDS[str];
      if is_keyword {
        lexer.token.kind = keyword_kind;
        if keyword_kind == .Keyword_True || keyword_kind == .Keyword_False {
          lexer.token.kind = .Boolean;
          lexer.token.value = cast(bool) (keyword_kind == .Keyword_True);
        }
      } else {
        lexer.token.kind = .Name;
      }

      return lexer.token;
    }
  }

  lexer.token.position.column = start_column;

  return lexer.token;
}

/**
* Scans the current token of a lexer as a float.
* 
* @param lexer The lexer to scan the float of.
*/
lexer_scan_float :: proc(lexer: ^Lexer) {
  lexer.token.kind = .Float;
  lexer.token.value = 0.0;

  start_position := lexer.stream_byte_position;
  character := lexer_peek_next_character(lexer);
  for unicode.is_digit(character) {
    lexer_consume_character(lexer);
    character = lexer_peek_next_character(lexer);
  }

  if character == '.' {
    lexer_consume_character(lexer);
    character = lexer_peek_next_character(lexer);
    for unicode.is_digit(character) {
      lexer_consume_character(lexer);
      character = lexer_peek_next_character(lexer);
    } 
  } else if unicode.to_lower(character) == 'e' {
    lexer_consume_character(lexer);
    character = lexer_peek_next_character(lexer);

    if character == '+' || character == '-' {
      lexer_consume_character(lexer);
      character = lexer_peek_next_character(lexer);
    }

    if !unicode.is_digit(character) {
      report_error_fatal(lexer, "Expected digit after float exponent literal but found '%c'", character);
    }

    for unicode.is_digit(character) {
      lexer_consume_character(lexer);
      character = lexer_peek_next_character(lexer);
    }
  }

  value := libc.strtod(cstring(raw_data(lexer.stream[start_position:])), nil);

  lexer.token.value = value;
}

/**
* Scans the current token of a lexer as an integer.
* 
* @param lexer The lexer to scan the integer of.
*/
lexer_scan_integer :: proc(lexer: ^Lexer) {
  lexer.token.kind = .Integer;
  lexer.token.value = cast(u64) 0;

  character := lexer_peek_next_character(lexer);

  base: u64 = 10;
  if character == '0' {
    lexer_consume_character(lexer);
    character := lexer_peek_next_character(lexer);

    if unicode.is_alpha(character) {
      if unicode.to_lower(character) == 'b' {
        base = 2;
      } else if unicode.to_lower(character) == 'o' {
        base = 8;
      } else if unicode.to_lower(character) == 'x' {
        base = 16;
      } else {
        report_error_fatal(lexer, "Invalid integer literal prefix: '%c'", character);
      }
      lexer_consume_character(lexer);
    }
  }

  character = lexer_peek_next_character(lexer);

  value: u64 = 0;
  for {
    // We allow an unlimited amount of underscores anywhere within an integer literal.
    if character == '_' {
      lexer_consume_character(lexer);
      character = lexer_peek_next_character(lexer);
      continue;
    }

    digit := CHAR_TO_DIGIT[character];
    // Make sure we actually have a valid digit.
    // This currently does an early out for hexadeciaml numbers greater than 'f'.
    if digit == 0 && character != '0' {
      break;
    }

    // Make sure the digit is valid for the base.
    if digit >= base {
      report_error_fatal(lexer, "Digit '%c' out of range for base %v", character, base);
      digit = 0;
    }

    // Make sure the literal is not too big.
    if value > (U64_MAX - digit) / base {
      report_error_fatal(lexer, "Integer literal constant is too big");

      // Skip remaining numbers.
      for unicode.is_digit(character) {
        lexer_consume_character(lexer);
        character = lexer_peek_next_character(lexer);
      }

      return;
    }

    value = value * base + digit;
    lexer_consume_character(lexer);
    character = lexer_peek_next_character(lexer);
  }

  lexer.token.value = value;
}

/**
* Scans the current token of a lexer as a character.
* 
* @param lexer The lexer to scan the character of.
*/
lexer_scan_character :: proc(lexer: ^Lexer) {
  lexer.token.kind = .Character;
  lexer.token.value = rune(0);

  character := lexer_peek_next_character(lexer);

  value: rune;
  if character == '\'' {
    report_error_fatal(lexer, "Character literal cannot be empty");
  } else if character == '\n' {
    report_error_fatal(lexer, "Character literal cannot contain newline");
  } else if character == '\\' {
    lexer_consume_character(lexer);
    character := lexer_peek_next_character(lexer);  

    if character < len(ESCAPE_TO_CHAR) {
      value = ESCAPE_TO_CHAR[character];  

      // Make sure the escaped character is actually valid.
      if value == 0 && character != '0' {
        report_error_fatal(lexer, "Invalid character literal espace: '\\%c'", character);  
      }
    } else {
      report_error_fatal(lexer, "Invalid character literal espace: '\\%c'", character);
    }
  } else {
    value = character;
  }

  lexer_consume_character(lexer);
  character = lexer_peek_next_character(lexer);
  if character != '\'' {
    report_error_fatal(lexer, "Expected closing char quote but found: '%c'", character);
  } else {
    lexer_consume_character(lexer);
  }

  lexer.token.value = value;
}

/**
* Scans the current token of a lexer as a string.
* 
* @param lexer  The lexer to scan the string of.
* @param is_raw Should the string be passed raw?
*/
lexer_scan_string :: proc(lexer: ^Lexer, is_raw: bool) {
  lexer.token.kind = .String;

  character := lexer_peek_next_character(lexer);
  
  builder: strings.Builder;
  // We do not destroy the builder because we return his buffer as the string.

  for character != 0 && ((is_raw && character != '`') || (!is_raw && character != '"')) {
    value := character;
    if (!is_raw) {
      if character == '\n' {
        report_error_fatal(lexer, "String literal cannot contain newline");
      } else if character == '\\' {
        lexer_consume_character(lexer);
        character = lexer_peek_next_character(lexer);  
  
        if character < len(ESCAPE_TO_CHAR) {
          value = ESCAPE_TO_CHAR[character];  
  
          // Make sure the escaped character is actually valid.
          if value == 0 && character != '0' {
            report_error_fatal(lexer, "Invalid string literal espace: '\\%c'", character);  
          }
        } else {
          report_error_fatal(lexer, "Invalid string literal espace: '\\%c'", character);
        }
      }
    }

    strings.write_rune(&builder, value);

    lexer_consume_character(lexer);
    character = lexer_peek_next_character(lexer);
  }

  if (is_raw && character != '`') || (!is_raw && character != '"') {
    report_error_fatal(lexer, "Unexpected end of file within string literal");
    return;
  }
  lexer_consume_character(lexer);

  lexer.token.value = strings.to_string(builder);
}

/**
* Skips all whitespace and comments in the lexer stream.
* 
* @param lexer The lexer to skip the whitespace and comments of.
*/
lexer_skip_whitespace_and_comments :: proc(lexer: ^Lexer) {
  for {
    character := lexer_peek_next_character(lexer);
    if character == '\n' {
      lexer_advance_newline(lexer);
    } 

    is_whitespace := unicode.is_space(character);
    if is_whitespace {
      // We consume the caracter only if it is truly a whitespace.
      lexer_consume_character(lexer);
    } else if character == '/' {
      if lexer_peek_second_next_character(lexer) == '/' {
        // Skip single-line comment.
        lexer_consume_character(lexer);
        lexer_consume_character(lexer);

        character = lexer_peek_next_character(lexer);
        for character != 0 && character != '\n' {
          lexer_consume_character(lexer);
          character = lexer_peek_next_character(lexer);
        }
      } else if lexer_peek_second_next_character(lexer) == '*' {
        // Skip multi-line comment.
        lexer_consume_character(lexer);
        lexer_consume_character(lexer);

        level := 1;

        character = lexer_peek_next_character(lexer);
        for character != 0 && level > 0 {
          // Allow nesting of multi-line comments
          if character == '/' && lexer_peek_second_next_character(lexer) == '*' {
            level += 1;

            lexer_consume_character(lexer);
            lexer_consume_character(lexer);
            character = lexer_peek_next_character(lexer);
          } else if character == '*' && lexer_peek_second_next_character(lexer) == '/' {
            level -= 1;

            lexer_consume_character(lexer);
            lexer_consume_character(lexer);
            character = lexer_peek_next_character(lexer);
          } else {
            if character == '\n' {
              lexer_advance_newline(lexer);
            }

            lexer_consume_character(lexer);
          }
          character = lexer_peek_next_character(lexer);
        }
      } else {
        // Here we know that we have found a single '/' which corresponds to a division token.
        break;
      }
    } else {
      // At this point we have consumed all whitespace characters and can continue on.
      break;
    }
  }
}

/**
* Peeks the next character in the lexer stream.
* 
* @param lexer The lexer to peek the next character of.
* @return The peeked character.
*/
lexer_peek_next_character :: proc(lexer: ^Lexer) -> rune {
  if lexer.stream_byte_position >= len(lexer.stream) {
    return 0;
  }

  character := utf8.rune_at(lexer.stream, lexer.stream_byte_position);
  return character;
}

/**
* Peeks the second next character in the lexer stream.
* 
* @param lexer The lexer to peek the second next character of.
* @return The peeked character.
*/
lexer_peek_second_next_character :: proc(lexer: ^Lexer) -> rune {
  next_character := lexer_peek_next_character(lexer);
  stream_byte_position := lexer.stream_byte_position + utf8.rune_size(next_character);

  if stream_byte_position >= len(lexer.stream) {
    return 0;
  }

  character := utf8.rune_at(lexer.stream, stream_byte_position);
  return character;
}

/**
* Consumes the next character in the lexer stream.
* 
* @param lexer The lexer to consume the next character of.
* @return The consumed character.
*/
lexer_consume_character :: proc(lexer: ^Lexer) -> rune {
  character := utf8.rune_at(lexer.stream, lexer.stream_byte_position);
  lexer.stream_byte_position += utf8.rune_size(character);
  lexer.token.position.column += 1;
  return character;
}

/**
* Advances the lexer state to a new line.
* 
* @param lexer The lexer to advance.
*/
lexer_advance_newline :: proc(lexer: ^Lexer) {
  // We set the column to 0 because it will get incremented to 1 when the \n is consumed.
  lexer.token.position.line += 1;
  lexer.token.position.column = 0;
}
