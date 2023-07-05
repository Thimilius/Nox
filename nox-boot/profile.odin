package nox

import "core:time"

ZONE_COLOR_DEFAULT          : u32 = 0x000000;
ZONE_COLOR_PARSER           : u32 = 0x008800;
ZONE_COLOR_RESOLVER         : u32 = 0x660066;
ZONE_COLOR_RESOLVER_GENERIC : u32 = 0x664466;
ZONE_COLOR_TYPE             : u32 = 0x2266FF;
ZONE_COLOR_GENERATOR        : u32 = 0xBB55AA;
ZONE_COLOR_GENERATOR_DEBUG  : u32 = 0xBB55FF;

/**
* Contains all compiler statistics.
*/
Profiler :: struct {
  global_stopwatch: time.Stopwatch,     // Stopwatch used for the overall execution time of the compiler.
  individual_stopwatch: time.Stopwatch, // Stopwatch used for timing individual phases of the compiler.

  execution_time: f64,                  // The execution time of the whole compiler including frontend, LLVM and linker.

  frontend_time: f64,                   // The execution time of the frontend including all internal phases (Parser, Resolver and Generator).
  parser_time: f64,                     // The execution time of the Parser.
  resolver_time: f64,                   // The execution time of the Resolver.
  generator_time: f64,                  // The execution time of the Generator.

  llvm_time: f64,                       // The execution time of LLVM including optimization phases and native code generation.
  link_time: f64,                       // The execution time of the linker.

  processed_lines: uint,                // The number of overall lines processed by the lexer (including blanks and comments).
}
