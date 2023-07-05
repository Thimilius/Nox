package nox

import "core:container/queue"
import "core:fmt"
import "core:log"
import "core:os"
import "core:runtime"
import "core:strings"
import "core:sync"
import "core:thread"
import "core:time"
import "tracy"

/**
* Contains all the compilation arguments that can be set from the command line.
*/
Compilation_Arguments :: struct {
  name: string,               // The name of the package (or file) to compile.

  output_name: string,        // The name of the output file.
  optimization_level: int,    // The optimization level to apply (-1, 0, 1 or 2).
  generate_llvm_ir: bool,     // Should the LLVM IR (.ll) file be generated?
  keep_intermediate: bool,    // Should the intermediate object file (.obj or .o) be kept?
  build_debug: bool,          // Should debug information be generated? (Windows only)
  rttr_disabled: bool,        // Should RTTR be disabled? (Sets the 'NOX_RTTR_DISABLED' constant to true)
  assert_disabled: bool,      // Should the assert procedure be disabled? (Sets the 'NOX_ASSERT_DISABLED' constant to true)
  extra_linker_flags: string, // Extra arguments that are passed to the linker as is.

  verbose: bool,              // Should verbose logging be active?
  verbose_more_timings: bool, // Should more timings be displayed in verbose logging?
  run_tests: bool,            // Should the test-framework be run?
}

/**
* Contains all data required by the compiler.
*/
Compiler :: struct {
  arguments: Compilation_Arguments,         // Holds the arguments that are used for compilation.

  storage: ^Type_Storage,                   // Holds a reference to the type storage.
  resolver: ^Resolver,                      // Holds a reference to the resolver.

  packages: map[string]^Package,            // Contains all imported packages.
  package_search_paths: map[string]string,  // Maps package sources (like 'std') to their corresponding absolute search path.
  current_package: ^Package,                // Holds the currently active package when resolving.
  runtime_package: ^Package,                // Holds the special 'runtime' package.

  parsing_threads: [dynamic]^thread.Thread, // Holds all parsing threads.
  file_queue: [dynamic]Queued_File,         // Holds the queue for files to parse.
  file_queue_index: int,                    // Holds the current index into the file queue.
  file_lock: sync.Mutex,                    // Holds the lock to synchronize access to the file queue.
  package_lock: sync.Mutex,                 // Holds the lock to synchronize access to the currently parsed package.
  finished_files_count: int,                // Holds the number of files which have finished parsing.
  parsing_threads_should_terminate: bool,   // Whether or not the parsing threads should terminate. 
  terminate_lock: sync.Mutex,               // Holds the lock to synchronize access to the should terminate flag.
  
  libraries_to_link: [dynamic]string,       // Holds all externally imported libraries.

  profiler: Profiler,                       // Holds the profiler for keeping track of stats.
}

/**
* Represents a queued file for parsing by a thread.
*/
Queued_File :: struct {
  pack: ^Package,    // The package the file belongs to.
  file_path: string, // The path to the file.
}

/**
* The major version of the compiler
*/ 
NOX_VERSION_MAJOR :: 1
/**
* The minor version of the compiler
*/ 
NOX_VERSION_MINOR :: 0
/**
* The patch version of the compiler
*/ 
NOX_VERSION_PATCH :: 0

/**
* Main entry point for the compiler.
*/
nox_main :: proc() {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  os_initialize();

  nox_print_header();

  compiler: Compiler;
  defer nox_compiler_destroy(&compiler);
  context.user_ptr = &compiler;

  nox_parse_arguments(&compiler);

  // Setup the correct logger.
  logger := nox_logger(compiler.arguments.verbose);
  context.logger = logger;

  // We map the path to the standard library to the 'std' package source.
  exe_directory := os_get_exe_directory();
  system_packages_search_path := strings.concatenate({exe_directory, "nox-lib", OS_PATH_SEPERATOR});
  compiler.package_search_paths["std"] = system_packages_search_path;
  delete(exe_directory);

  // The current directory gets mapped to the empty package source.
  current_directory := os_get_current_directory();
  compiler.package_search_paths[""] = current_directory;

  time.stopwatch_start(&compiler.profiler.global_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);

  if compiler.arguments.run_tests {
    compiler.arguments.name = "nox-test";
  } else {
    when ODIN_DEBUG {
      if len(compiler.arguments.name) == 0 {
        compiler.arguments.name = "nox-sandbox";
      }
    }

    if compiler.arguments.name == "" {
      nox_print_info();
    }
  }
  import_path := compiler.arguments.name;

  is_compiling_single_file := false;
  if os_has_extension(import_path, ".nox") {
    is_compiling_single_file = true;
    if !os_file_exists(import_path) {
      report_error_fatal("The file '%v' does not exist", import_path);
    }
  }

  // Make sure the directory we got passed is a valid Nox package.
  if !is_compiling_single_file {
    absolute_package_path, path_success := package_find_absolute_path(import_path);
    if !path_success {
      report_error_fatal("The name '%v' is not a Nox package", import_path);
    }
    delete(absolute_package_path);
  }

  compiler.storage = type_storage_make();
  compiler.resolver = resolver_make();

  package_initialize_parsing_threads();

  // The first thing we do is import and compile the 'runtime' package.
  // We set 'compiler.runtime_package' while importing.
  package_import("std:runtime");
  assert(compiler.runtime_package != nil);

  main_package := is_compiling_single_file ? package_import_single_file(compiler.arguments.name) : package_import(import_path);
  if main_package == nil {
    report_error_fatal("Failed to compile package at path: %v", import_path);
  }

  compiler.profiler.parser_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.individual_stopwatch));
  time.stopwatch_reset(&compiler.profiler.individual_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);

  package_shutdown_parsing_threads();

  // We start at the main symbol to figure out which other symbols are actually reachable.
  main_symbol := nox_get_main_symbol(main_package);
  resolver_resolve_symbol(compiler.resolver, main_symbol);
  nox_check_main_symbol(main_symbol);

  // Resolve (sort, type-check, evaluate).
  resolver_resolve(compiler.resolver);

  compiler.profiler.resolver_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.individual_stopwatch));
  time.stopwatch_reset(&compiler.profiler.individual_stopwatch);
  time.stopwatch_start(&compiler.profiler.individual_stopwatch);

  // Finally generate the code.
  generation_arguments: Generation_Arguments;
  generation_arguments.storage = compiler.storage;
  generation_arguments.resolver_output = compiler.resolver.output;
  generation_arguments.main_symbol = main_symbol;
  generation_arguments.compilation_arguments = compiler.arguments;
  generation_arguments.libraries_to_link = compiler.libraries_to_link[:];

  generator := generator_make();
  defer generator_destroy(generator);
  generate(generator, generation_arguments);

  compiler.profiler.execution_time = time.duration_seconds(time.stopwatch_duration(compiler.profiler.global_stopwatch))

  log.logf(.Info, "Processed lines (including blanks and comments): %v", compiler.profiler.processed_lines);
  log.logf(.Info, "Execution time: %vs (100%%)", compiler.profiler.execution_time);
  log.logf(.Info, "      Frontend: %vs (%v%%)", compiler.profiler.frontend_time, (compiler.profiler.frontend_time / compiler.profiler.execution_time) * 100);
  
  if compiler.arguments.verbose_more_timings {
    log.logf(.Info, "   -    Parser: %vs (%v%%)", compiler.profiler.parser_time, (compiler.profiler.parser_time / compiler.profiler.execution_time) * 100);  
    log.logf(.Info, "   -  Resolver: %vs (%v%%)", compiler.profiler.resolver_time, (compiler.profiler.resolver_time / compiler.profiler.execution_time) * 100);    
    log.logf(.Info, "   - Generator: %vs (%v%%)", compiler.profiler.generator_time, (compiler.profiler.generator_time / compiler.profiler.execution_time) * 100);    
  }
  
  log.logf(.Info, "          LLVM: %vs (%v%%)", compiler.profiler.llvm_time, (compiler.profiler.llvm_time / compiler.profiler.execution_time) * 100);
  log.logf(.Info, "        Linker: %vs (%v%%)", compiler.profiler.link_time, (compiler.profiler.link_time / compiler.profiler.execution_time) * 100);

  if compiler.arguments.run_tests {
    when ODIN_OS == .Windows {
      os_run_command("test.exe");
      if !compiler.arguments.keep_intermediate {
        os_remove_file("test.exe");
        os_remove_file("test.pdb");
      }
    } else {
      os_run_command("./test");
      if !compiler.arguments.keep_intermediate {
        os_remove_file("test");
      }
    }
  }

  if compiler.arguments.verbose {
    nox_logger_destroy(&logger);
  }

  free_all(context.temp_allocator);
}

/**
* Parses the command line arguments into the compilation arguments.
*/
nox_parse_arguments :: proc(compiler: ^Compiler) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  arguments: Compilation_Arguments;
  arguments.output_name = "out";
  when ODIN_DEBUG {
    arguments.generate_llvm_ir = true;
    arguments.build_debug = true;
    arguments.verbose = true;
  }

  name_set := false;

  for i := 1; i < len(runtime.args__); i += 1 {
    argument := runtime.cstring_to_string(runtime.args__[i]);

    if argument == "-o-1" {
      arguments.optimization_level = -1;
    } else if argument == "-o0" {
      arguments.optimization_level = 0;
    } else if argument == "-o1" {
      arguments.optimization_level = 1;
    } else if argument == "-o2" {
      arguments.optimization_level = 2;
    } else if argument == "-o3" {
      arguments.optimization_level = 3;
    } else if argument == "-generate-llvm-ir" {
      arguments.generate_llvm_ir = true;
    } else if argument == "-keep-intermediate" {
      arguments.keep_intermediate = true;
    } else if argument == "-debug" {
      arguments.build_debug = true;
    } else if argument == "-disable-rttr" {
      arguments.rttr_disabled = true;
    } else if argument == "-disable-assert" {
      arguments.assert_disabled = true;
    } else if argument == "-verbose" {
      arguments.verbose = true;
    } else if argument == "-verbose-more-timings" {
      arguments.verbose_more_timings = true;
    } else if argument == "-test" {
      arguments.run_tests = true;
    } else {
      colon_index := strings.index(argument, ":");

      prefix_out := "-out:";
      prefix_extra := "-extra-linker-flags:";
      prefix_source := "-package-source:";

      if colon_index > 0 {
        actual_argument := argument[:colon_index + 1];
        if actual_argument == prefix_out {
          arguments.output_name = argument[len(prefix_out):];
          if len(arguments.output_name) == 0 {
            report_error_fatal("No output name provided in 'out' argument");
          }  
        } else if actual_argument == prefix_extra {
          arguments.extra_linker_flags = argument[len(prefix_extra):];
        } else if actual_argument == prefix_source {
          package_source := argument[len(prefix_source):];
          equal_index := strings.index(package_source, "=");
          if equal_index < 0 {
            report_error_fatal("Missing package source path after: '%v'", package_source);
          }
          package_source_name := package_source[:equal_index];
          if len(package_source_name) == 0 {
            report_error_fatal("Empty name for package source");
          }
          package_source_path := package_source[equal_index + 1:];
          if len(package_source_path) == 0 {
            report_error_fatal("Empty path for package source '%v'", package_source_name);
          }
          compiler.package_search_paths[package_source_name] = strings.clone(package_source_path);
        } else {
          report_error_fatal("Unknown argument: %v", actual_argument);
        }
      } else {
        dash_index := strings.index(argument, "-");
        if dash_index == 0 {
          report_error_fatal("Unknown argument: %v", argument[1:]);
        }

        if (name_set) {
          report_warning("Package name already provided. Name '%v' ignored.", argument);
        } else {
          name_set = true;
          arguments.name = argument;
        }
      }
    }
  }

  // The running of tests overwrites configures the arguments as required.
  if arguments.run_tests {
    arguments.optimization_level = -1; // We want no optimization so LLVM does not simplify our actual tests.
    arguments.output_name = "test";  
  }

  compiler.arguments = arguments;
}

/**
* Prints the Nox Compiler information to the console.
*/
nox_print_info :: proc() {
  fmt.printf("Usage:\n");
  fmt.printf("         noxb [arguments] package\n");
  fmt.printf("         noxb [arguments] file\n");
  fmt.printf("Arguments:\n");
  fmt.printf("         -debug                        Generate debug information.\n");
  fmt.printf("         -disable-assert               Disable all calls to 'assert'. Sets the 'NOX_ASSERT_DISABLED' constant to 'true'.\n");
  fmt.printf("         -disable-rttr                 Disable support for RTTR. Sets the 'NOX_RTTR_DISABLED' constant to 'true'.\n");
  fmt.printf("         -extra-linker-flags:[string]  Pass 'string' as additional flags and commands to the linker.\n");
  fmt.printf("         -generate-llvm-ir             Additionally generate the intermediate LLVM IR as an .ll file.\n");
  fmt.printf("         -keep-intermediate            Keep the intermediate object files.\n");
  fmt.printf("         -o[n]                         Set the optimization level 'n'. Allowed values are: -1, 0, 1, 2 and 3.\n");
  fmt.printf("         -out:[name]                   Set 'name' as the output name for the generated executable.\n");
  fmt.printf("         -package-source:[name]=[path] Set 'name' as a package source for 'path'. Can be specified multiple times.\n");
  fmt.printf("         -test                         Execute the compiler tests.\n");
  fmt.printf("         -verbose                      Print compiler information while running.\n");
  fmt.printf("         -verbose-more-timings         Print more timings for the frontend.\n");

  os.exit(0);
}

/**
* Prints the Nox Compiler information header to the console.
*/
nox_print_header :: proc() {
  fmt.printf("Nox Bootstrap Compiler %v.%v.%v\n", NOX_VERSION_MAJOR, NOX_VERSION_MINOR, NOX_VERSION_PATCH);
}

/**
* Gets the main symbol (proc main()) from a provided package.
* 
* @param pack The package to look in.
* @return The main symbol (can be null if not found).
*/
nox_get_main_symbol :: proc(pack: ^Package) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  main_symbol := package_get_symbol_defined_anywhere_in_package(pack, "main", .All);
  if main_symbol == nil {
    report_error_fatal("Missing entry point 'main'");
  }
  return main_symbol;
}

/**
* Checks if a provided main symbol is actually valid.
* 
* @param main_symbol The main symbol to check.
*/
nox_check_main_symbol :: proc(main_symbol: ^Symbol) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  position := main_symbol.declaration.position;

  // This ensures that we can't have multiple 'main' routines in a package.
  if main_symbol.access_modifier == .Private {
    report_error_fatal(position, "Entry point 'main' has to be declared 'internal' or 'public'");
  }

  if main_symbol.kind != .Routine || main_symbol.declaration.kind != .Procedure {
    report_error_fatal(position, "Entry point 'main' is not a procedure");
  }

  type_routine := cast(^Type_Routine) main_symbol.type;
  if type_routine.calling_convention != .Nox {
    report_error_fatal(position, "Entry point 'main' has to have calling convention 'nox'");
  }

  if len(type_routine.parameters) != 0 {
    report_error_fatal(position, "Entry point 'main' can't have any parameters");
  }
  if !type_is_void(type_routine.return_type) {
    report_error_fatal(position, "Entry point 'main' can't have a return type");
  }

  if .Routine_Disabled in main_symbol.flags {
    report_error_fatal(position, "Entry point 'main' can't be disabled");
  }
}

/**
* Destroys all resources of the provided compiler.
* 
* @param compiler The compiler to destroy.
*/ 
nox_compiler_destroy :: proc(compiler: ^Compiler) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  resolver_destroy(compiler.resolver);

  for _, pack in compiler.packages {
    package_destroy(pack);
  }
  delete(compiler.packages);

  for _, search_path in compiler.package_search_paths {
    delete(search_path);
  }
  delete(compiler.package_search_paths);

  type_storage_destroy(compiler.storage);

  for library in compiler.libraries_to_link {
    delete(library);
  }
  delete(compiler.libraries_to_link);
}

/**
* A wrapped logger for the compiler to take into account the '-verbose' command line argument.
*/
Nox_Logger :: struct {
  verbose: bool,          // Is the '-verbose' command line argument set?
  logger: runtime.Logger, // The wrapped logger.
}

/**
* Creates a new 'Nox_Logger'.
* 
* @return The newly created Nox_Logger abstracted as a 'runtime.Logger'.
*/
nox_logger :: proc(verbose: bool) -> runtime.Logger {
  data := new(Nox_Logger);
  data.verbose = verbose;
  data.logger = log.create_console_logger(opt = nil);
  return log.Logger{nox_logger_proc, data, runtime.Logger_Level.Debug, nil};
}

/**
* The logging procedure for a 'Nox_Logger'.
*
* @param logger_data The data of the logger (will always be ^Nox_Logger).
* @param level       The logging level.
* @param text        The text to log.
* @param options     The logging options.
* @param location    The source code location.
*/
nox_logger_proc :: proc(
  logger_data: rawptr,
  level: runtime.Logger_Level,
  text: string,
  options: bit_set[runtime.Logger_Option],
  location := #caller_location,
) {
  data := cast(^Nox_Logger)logger_data;
  if (data.verbose) {
    data.logger.procedure(data.logger.data, level, text, nil, location);
  }
}

/**
* Destroys the resources associated with a 'Nox_Logger'.
*
* @param logger The 'Nox_Logger' to destroy.
*/
nox_logger_destroy :: proc(logger: ^runtime.Logger) {
  data := cast(^Nox_Logger)logger.data;
  log.destroy_console_logger(data.logger);
  free(data);
  logger^ = log.nil_logger();
}
