package nox

import "core:os"

/**
* Reports a simple formatted warning message.
* 
* @param format The format of the message.
* @param args   The format arguments.
*/
report_warning_simple :: proc(format: string, args: ..any) {
  os_print_colored(.Yellow, "Warning: ");
  os_print_colored(.Yellow, format, ..args);
  os_print_colored(.Yellow, "\n");
}

/**
* Reports a formatted warning message for a provided source position.
* 
* @param position The source position.
* @param format   The format of the message.
* @param args     The format arguments.
*/
report_warning_position :: proc(position: Source_Position, format: string, args: ..any) {
  os_print_colored(.Yellow, "%v(%v:%v): ", position.file, position.line, position.column);
  os_print_colored(.Yellow, format, ..args);
  os_print_colored(.Yellow, "\n");
}

report_warning :: proc{report_warning_simple, report_warning_position};

/**
* Reports a simple fatal formatted error message.
* 
* @param format The format of the message.
* @param args   The format arguments.
*/
report_error_fatal_simple :: proc(format: string, args: ..any) {
  os_print_colored(.Red, "Error: ");
  os_print_colored(.Red, format, ..args);
  os_print_colored(.Red, "\n");

  os.exit(-1);
}

/**
* Reports a fatal formatted lexer error message.
* 
* @param lexer  The lexer.
* @param format The format of the message.
* @param args   The format arguments.
*/
report_error_fatal_lexer :: proc(lexer: ^Lexer, format: string, args: ..any) {
  report_error_fatal_position(lexer.token.position, format, ..args);
}

/**
* Reports a fatal formatted parser error message.
* 
* @param parser The parser.
* @param format The format of the message.
* @param args   The format arguments.
*/
report_error_fatal_parser :: proc(parser: ^Parser, format: string, args: ..any) {
  report_error_fatal_lexer(parser.lexer, format, ..args);
}

/**
* Reports a fatal formatted error message for a provided source position.
* 
* @param position The source position.
* @param format   The format of the message.
* @param args     The format arguments.
*/
report_error_fatal_position :: proc(position: Source_Position, format: string, args: ..any) {
  report_error_position(position, format, ..args);

  os.exit(-1);
}

report_error_fatal :: proc{report_error_fatal_simple, report_error_fatal_lexer, report_error_fatal_parser, report_error_fatal_position};

/**
* Reports a formatted error message for a provided source position.
* 
* @param position The source position.
* @param format   The format of the message.
* @param args     The format arguments.
*/
report_error_position :: proc(position: Source_Position, format: string, args: ..any) {
  os_print_colored(.Red, "%v(%v:%v): ", position.file, position.line, position.column);
  os_print_colored(.Red, format, ..args);
  os_print_colored(.Red, "\n");
}
