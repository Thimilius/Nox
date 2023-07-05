package nox

import "core:container/queue"
import "core:log"
import "core:mem"
import "core:os"
import "core:runtime"
import "core:strings"
import "core:sync"
import "core:thread"
import "core:time"
import "tracy"

/**
* Represents a Nox package.
*/
Package :: struct {
  name: string,                    // The name of the package.

  import_path: string,             // The relative path the package got imported from.
  full_path: string,               // The absolute path to the package directory on disk.

  files: map[string]^Package_File, // Contains all files inside the package.
}

/**
* Represents a single source file inside a package.
*/
Package_File :: struct {
  pack: ^Package,                             // The package the file belongs to.

  thread_allocator: mem.Allocator,            // The allocator of the parsing thread which is used to allocate the source string and the file itself.

  path: string,                               // The absolute path to the file on disk.
  source: string,                             // The raw source code (will be kept alive until the compiler finishes).

  ast_dynamic_pool: mem.Dynamic_Pool,         // The memory pool used by the AST allocator.
  ast_allocator: mem.Allocator,               // The allocator used for the AST.
  
  declarations: [dynamic]^Declaration,        // Contains all declarations inside a file (including all the imports).
  import_declarations: [dynamic]^Declaration, // Contains the import declarations at top scope only.
  
  symbols: map[string]^Symbol,                // Contains all symbols that are defined by the file.
  imported_symbols: map[string]^Symbol,       // Contains all symbols imported by the file (either as a '.Package' symbol or directly into scope).
}

/**
* The size of a single block for the AST allocator.
*/
AST_POOL_BLOCK_SIZE := 4 * 1024 * 1024; // 4 Mebibyte.

/**
* Defines differents modes for retrieving a symbol from a package.
*/
Symbol_Retrieval_Mode :: enum {
  All,         // All symbols regardless of their access modifier.
  Public_Only, // Only symbols with a 'public' access modifier.
}

/**
* Destroy all resources associated with a provided package.
* 
* @param pack The package to destroy.
*/
package_destroy :: proc(pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  delete(pack.full_path);

  // We cleanup the source files at the very end.
  for _, file in pack.files {
    delete(file.path);
    delete(file.symbols);
    delete(file.imported_symbols);
    
    mem.dynamic_pool_destroy(&file.ast_dynamic_pool);
    
    // For the source and the file itself we have to use the dedicated thread allocator as both got allocated on a seperate thread.
    {
      context.allocator = file.thread_allocator;
      delete(file.source);
      free(file);
    }
  }
  delete(pack.files);

  free(pack);
}

/**
* Initializes the parsing threads.
*/
package_initialize_parsing_threads :: proc() {
  compiler := cast(^Compiler) context.user_ptr;

  // The number of threads we want is based on the processor core count.
  // We subtract one, so the main thread gets taken into account.
  processor_count := os.processor_core_count() - 1;
  compiler.parsing_threads = make_dynamic_array_len_cap([dynamic]^thread.Thread, processor_count, processor_count);
  for i in 0..<processor_count {
    compiler.parsing_threads[i] = thread.create_and_start_with_data(compiler, package_parsing_thread_procedure);
  }
}

/**
* Shutdowns the parsing threads.
*/
package_shutdown_parsing_threads :: proc() {
  compiler := cast(^Compiler) context.user_ptr;

  // Tell the parsing threads to terminate.
  sync.mutex_lock(&compiler.terminate_lock);
  compiler.parsing_threads_should_terminate = true;
  sync.mutex_unlock(&compiler.terminate_lock);

  // Wait for the threads to finish and destroy them.
  for t in compiler.parsing_threads {
    thread.destroy(t);
  }

  delete(compiler.file_queue);
  delete(compiler.parsing_threads);
}

/**
* Imports a package at a provided path that can be relative.
*
* @param import_path The path to import the package from.
* @return The imported package.
*/
package_import :: proc(import_path: string) -> ^Package {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;
  pack := compiler.packages[import_path];

  if pack == nil {
    pack = new(Package);
    pack.name = package_extract_name_from_import_path(import_path);
    pack.import_path = import_path;
    pack.files = make_map(map[string]^Package_File, 1 << runtime.MAP_MIN_LOG2_CAPACITY, context.allocator);
    
    full_path, success := package_find_absolute_path(import_path);
    if !success {
      return nil;
    }
    pack.full_path = full_path;

    compiler.packages[import_path] = pack;

    log.logf(.Info, "Importing package '%v' at path '%v'", pack.name, full_path);

    package_parse(pack);
    package_compile(pack);
  }

  return pack;
}

/**
* Imports a single file as a Nox package.
*
* @param file_path The path to import the file from.
* @return The imported package.
*/
package_import_single_file :: proc(file_path: string) -> ^Package {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;

  pack := new(Package);
  extracted_package_name := package_extract_name_from_import_path(file_path);
  pack.name = extracted_package_name[:len(extracted_package_name) - 4]; // We know we have an '.nox' ending.
  pack.import_path = file_path;

  full_path := os_get_absolute_path(file_path);
  pack.full_path = full_path;

  compiler.packages[file_path] = pack;

  log.logf(.Info, "Importing file '%v'", full_path);

  file_path := strings.clone(full_path);
  package_parse_file(compiler, pack, file_path);
  package_compile(pack);

  return pack;
}

/**
* Compiles a provided package which includes parsing and symbol creation.
*
* @param pack The package to compile.
*/
package_compile :: proc(pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;

  previous_package := package_enter(pack);

  has_compiled_runtime := compiler.runtime_package != nil;
  if has_compiled_runtime {
    // The symbols of the 'runtime' package (and all builtin types) get directly imported into the scope of every other package file that gets compiled.
    // We do this before adding the declarations of the actual package being compiled.
    // That way redeclarations get non-consufing error messages.
    for _, file in pack.files {
      resolver_import_package_symbols(compiler.resolver, file, SOURCE_POSITION_BUILTIN, compiler.runtime_package);
    }
  } else {
    compiler.runtime_package = pack;
  }

  resolver_add_package_declarations(compiler.resolver, pack);
  
  if !has_compiled_runtime {
    package_file := package_get_first_package_file(compiler.runtime_package);
    resolver_import_builtin_types(compiler.resolver, package_file);
    resolver_cache_runtime_package_types(compiler.resolver, pack);
    resolver_setup_compiler_constants(compiler.resolver, package_file)
  }
  
  // Process all imports in the top scope.
  package_process_imports(pack);

  // Now resolve all directives to get all potentionally additional declarations and imports.
  resolver_resolve_package_directives(compiler.resolver, pack);

  package_leave(previous_package);
}

/**
* Parses all files of a provided package.
*
* @param pack The package to parse.
*/
package_parse :: proc(pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  files := make_dynamic_array([dynamic]string, context.temp_allocator);

  iterator := os_directory_iterator_make(pack.full_path);
  for iterator.is_valid {
    if iterator.is_directory || !os_has_extension(iterator.name, ".nox") {
      os_directory_iterator_next(&iterator);
      continue;
    }

    relative_path := strings.concatenate({pack.full_path, iterator.name}, context.temp_allocator);
    // We do not free this absolute path, as it is necessary for position information.
    absolute_path := os_get_absolute_path(relative_path);

    append(&files, absolute_path);

    os_directory_iterator_next(&iterator);
  }

  compiler := cast(^Compiler) context.user_ptr;

  // Now we reset the queue with the new files that need to be parsed.
  // That includes resetting the counter and index which track the finished state.
  sync.mutex_lock(&compiler.file_lock);
  {
    compiler.finished_files_count = 0;
    compiler.file_queue_index = 0;
    clear(&compiler.file_queue);
    for file in files {
      append(&compiler.file_queue, Queued_File{pack, file});
    }
  }
  sync.mutex_unlock(&compiler.file_lock);

  // Here we wait for the parsing threads to finish.
  files_count := len(files);
  for {
    sync.mutex_lock(&compiler.package_lock);
    finished_files_count := compiler.finished_files_count;
    sync.mutex_unlock(&compiler.package_lock);
    if finished_files_count == files_count {
      break;
    }
  }
}

/**
* Defines the procedure for a single thread used for parsing files.
* 
* @param data The data being passed in.
*/
package_parsing_thread_procedure :: proc(data: rawptr) {
  compiler := cast(^Compiler) data;

  for {
    // Check if we have a file in the queue we need to parse.
    sync.mutex_lock(&compiler.file_lock);
    has_remaining_file := compiler.file_queue_index < len(compiler.file_queue);
    if has_remaining_file {
      queued_file := compiler.file_queue[compiler.file_queue_index];
      compiler.file_queue_index += 1;
      sync.mutex_unlock(&compiler.file_lock);

      package_parse_file(compiler, queued_file.pack, queued_file.file_path);
    } else {
      sync.mutex_unlock(&compiler.file_lock);
    }

    // When we are done, look for the signal which tells us if we should terminate or keep running.
    sync.mutex_lock(&compiler.terminate_lock);
    should_terminate := compiler.parsing_threads_should_terminate;
    defer sync.mutex_unlock(&compiler.terminate_lock);
    if should_terminate {
      break;
    }
  }
}

/**
* Parses a single package file.
*
* @param compiler The reference to the compiler.
* @param pack     The package to compile.
* @param path     The path of the file to parse.
*/
package_parse_file :: proc(compiler: ^Compiler, pack: ^Package, path: string) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  source, is_valid := os_read_file(path);
  if !is_valid {
    report_error_fatal("Failed to read source file at path: %v", path);
  }

  package_file := new(Package_File);
  package_file.pack = pack;
  package_file.thread_allocator = context.allocator;
  package_file.path = path;
  package_file.source = source;

  mem.dynamic_pool_init(&package_file.ast_dynamic_pool, context.allocator, context.allocator, AST_POOL_BLOCK_SIZE);
  package_file.ast_allocator = mem.dynamic_pool_allocator(&package_file.ast_dynamic_pool);

  lexer := lexer_make(path, source);
  parser := parser_make(&lexer, package_file.ast_allocator);

  parser_parse_declarations(&parser);
  package_file.declarations = parser.declarations;
  package_file.import_declarations = parser.import_declarations;

  sync.mutex_lock(&compiler.package_lock);
  compiler.profiler.processed_lines += cast(uint) lexer.token.position.line;
  pack.files[path] = package_file;
  compiler.finished_files_count += 1;
  sync.mutex_unlock(&compiler.package_lock);
}

/**
* Processes all import declarations of a package.
*
* @param pack The package to process all imports for.
*/
package_process_imports :: proc(pack: ^Package) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  // At this point we should have gathered all import declarations in all files.
  // Including those that are inside static ifs.
  for _, file in pack.files {
    for declaration in file.import_declarations {
      package_process_import(file, declaration);
    }
  }
}

/**
* Processes all import declarations of a package file.
*
* @param file        The package file to process all imports for.
* @param declaration The associated import declaration.
*/
package_process_import :: proc(file: ^Package_File, declaration: ^Declaration) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;

  declaration_import := cast(^Declaration_Import) declaration;
  import_path := declaration.name;
  import_path = strings.trim_space(import_path);
  path_is_empty := len(import_path) == 0;

  if .Extern in declaration.flags {
    if path_is_empty {
      report_error_fatal(declaration.position, "Missing library name in extern import");
    }

    is_relative := strings.contains_any(import_path, "/");
    absolute_import_path: string;
    if is_relative {
      relative_import_path := strings.concatenate({file.pack.full_path, import_path}, context.temp_allocator);
      absolute_import_path = os_get_absolute_path(relative_import_path);
    } else {
      absolute_import_path = strings.clone(import_path);
    }
    append(&compiler.libraries_to_link, absolute_import_path);
    return;
  }

  if path_is_empty {
    report_error_fatal(declaration.position, "Missing package name in import");
  }

  package_name := package_extract_name_from_import_path(import_path);
  imported_package := package_import(import_path);
  if imported_package == nil {
    report_error_fatal(declaration.position, "Failed to import package '%v'", import_path);
  }

  if declaration_import.import_in_scope {
    resolver_import_package_symbols(compiler.resolver, file, declaration.position, imported_package);
  }

  // Currently we still add a package symbol which may be used to refer to symbols even when they are imported in scope.
  package_symbol_name := declaration_import.alias == "" ? package_name : declaration_import.alias;
  resolver_add_package_symbol(compiler.resolver, imported_package, file, package_symbol_name, declaration);
}

/**
* Enters a new package to be set as the currently active package.
*
* @param pack The package to enter.
* @return The previously active package.
*/
package_enter :: proc(pack: ^Package) -> ^Package {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;
  previous_package := compiler.current_package;
  compiler.current_package = pack;
  return previous_package;
}

/**
* Leaves the currently active package.
*
* @param previous_package The previously active package.
*/
package_leave :: proc(previous_package: ^Package) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;
  compiler.current_package = previous_package;
}

/**
* Gets a symbol by name from a package based on the provided retrieval mode, reports an error when not found and performs an access check.
*
* @param pack           The package to retrieve the symbol from.
* @param position       The source position where the symbol to be retrieved is referenced.
* @param name           The name of the symbol to get.
* @param retrieval_mode The symbol retrieval mode.
* @return The retrieved symbol.
*/
package_get_symbol :: proc(pack: ^Package, position: Source_Position, name: string, retrieval_mode: Symbol_Retrieval_Mode) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  compiler := cast(^Compiler) context.user_ptr;
  symbol := package_get_symbol_without_validation(pack, nil, position, name, retrieval_mode);

  declaration := symbol.declaration;
  if declaration != nil {
    // At this point, we should not encounter builtin symbols as they have no declaration.
    access_modifier := declaration.access_modifier;      
    if access_modifier == .Internal {
      if symbol.package_file.pack != compiler.current_package {
        report_error_fatal(position, "Trying to access internal declaration '%v' defined in package '%v'", symbol.name, symbol.package_file.pack.name);
      }
    } else if access_modifier == .Private {
      if declaration.position.file != position.file {
        report_error_fatal(position, "Trying to access private declaration '%v' defined in file '%v'", symbol.name, declaration.position.file);
      }
    }
  }

  return symbol;
}

/**
* Gets a symbol by name from a package based on the provided retrieval mode and reports an error when not found.
*
* @param pack           The package to retrieve the symbol from.
* @param file           A package file to look into first (Can be nil).
* @param position       The source position where the symbol to be retrieved is referenced.
* @param name           The name of the symbol to get.
* @param retrieval_mode The symbol retrieval mode.
* @return The retrieved symbol.
*/
package_get_symbol_without_validation :: proc(
  pack: ^Package,
  file: ^Package_File,
  position: Source_Position,
  name: string,
  retrieval_mode: Symbol_Retrieval_Mode,
) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  file := file;
  if file == nil {
    file = package_get_package_file_by_name(pack, position.file);
  }
  
  symbol: ^Symbol;
  if file == nil {
    symbol = package_get_symbol_defined_anywhere_in_package(pack, name, retrieval_mode);
  } else {
    symbol = package_get_symbol_defined_in_package(pack, file, name, retrieval_mode);
  }
  if symbol != nil {
    return symbol;
  }

  // Now check for an imported symbol in the current package file.
  if file != nil {
    symbol, found := file.imported_symbols[name];
    if found {
      return symbol;
    }
  }

  // Handle symbol not being found.  
  compiler := cast(^Compiler) context.user_ptr;
  if (pack != compiler.current_package) {
    report_error_fatal(position, "Package '%v' does not declare symbol '%v'", pack.name, name);
  } else {
    report_error_fatal(position, "Undeclared symbol '%v'", name);
  }
  return nil;
}

/**
* Gets a defined symbol by name from a package based on the provided retrieval mode.
*
* @param pack           The package to retrieve the symbol from.
* @param file           A package file to look into first.
* @param name           The name of the symbol to get.
* @param retrieval_mode The symbol retrieval mode.
* @return The retrieved symbol (or nil if not found).
*/
package_get_symbol_defined_in_package :: proc(pack: ^Package, file: ^Package_File, name: string, retrieval_mode: Symbol_Retrieval_Mode) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  symbol, found_in_file := file.symbols[name];
  if found_in_file {
    if retrieval_mode == .Public_Only {
      if symbol.access_modifier == .Public {
        return symbol
      }
    } else {
      return symbol;
    }
  }

  for _, package_file in pack.files {
    if package_file == file {
      continue;
    }

    symbol, found_in_another_file := package_file.symbols[name];
    if found_in_another_file {
      if retrieval_mode == .Public_Only {
        if symbol.access_modifier == .Public {
          return symbol
        }
      } else {
        return symbol;
      }
    }
  }

  return nil;
}

/**
* Gets a defined symbol by name from a package based on the provided retrieval mode.
*
* @param pack           The package to retrieve the symbol from.
* @param name           The name of the symbol to get.
* @param retrieval_mode The symbol retrieval mode.
* @return The retrieved symbol (or nil if not found).
*/
package_get_symbol_defined_anywhere_in_package :: proc(pack: ^Package, name: string, retrieval_mode: Symbol_Retrieval_Mode) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  for _, package_file in pack.files {
    symbol, found := package_file.symbols[name];
    if found {
      if retrieval_mode == .Public_Only {
        if symbol.access_modifier == .Public {
          return symbol
        }
      } else {
        return symbol;
      }
    }
  }

  return nil;
}

/**
* Gets a symbol by name from a package based on the provided retrieval mode.
*
* @param pack           The package to retrieve the symbol from.
* @param file           A package file to look into first.
* @param name           The name of the symbol to get.
* @param retrieval_mode The symbol retrieval mode.
* @return The retrieved symbol (or nil if not found).
*/
package_get_symbol_without_checks :: proc(pack: ^Package, file: ^Package_File, name: string, retrieval_mode: Symbol_Retrieval_Mode) -> ^Symbol {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  symbol := package_get_symbol_defined_in_package(pack, file, name, retrieval_mode);
  if symbol == nil {
    return file.imported_symbols[name];
  } else {
    return symbol;
  }
}

/**
* Gets a file inside a package by name.
*
* @param pack      The package to get the file from.
* @param file_name The file name of the package file to retrive.
* @return The retrieved package file.
*/
package_get_package_file_by_name :: proc(pack: ^Package, file_name: string) -> ^Package_File {
  return pack.files[file_name];
}

/**
* Gets the first file inside a package (not sorted).
*
* @param pack The package to get the first file from.
* @return The first file (or nil if no files are inside the package).
*/
package_get_first_package_file :: proc(pack: ^Package) -> ^Package_File {
  for _, file in pack.files {
    return file;
  }
  return nil;
}

/**
* Trys to find the absolute path of a relative package path.
*
* @param import_path The relative package path.
* @return 1. The absolute package path; 2. True if successful otherwise false.
*/
package_find_absolute_path :: proc(import_path: string) -> (string, bool) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  // We extract the package source and package path.
  seperator_index := strings.index(import_path, ":");
  package_source := seperator_index >= 0 ? import_path[:seperator_index] : "";
  package_path := seperator_index >= 0 ? import_path[seperator_index + 1:] : import_path;

  compiler := cast(^Compiler) context.user_ptr;
  package_search_path, search_path_found := compiler.package_search_paths[package_source];
  if !search_path_found {
    return "", false;
  }

  directory_path := strings.concatenate({package_search_path, package_path, OS_PATH_SEPERATOR});
  if package_is_directory_suitable(directory_path) {
    return directory_path, true;
  }

  return "", false;
}

/**
* Extracts the package name of a relative import path.
*
* @param The import path to extract the name from.
* @return The extracted package name.
*/
package_extract_name_from_import_path :: proc(import_path: string) -> string {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  // First we strip of the potential package source.
  seperator_index := strings.index(import_path, ":");
  package_name := seperator_index >= 0 ? import_path[seperator_index + 1:] : import_path;

  // Now we strip of any relative directories.
  index := strings.last_index(package_name, "/");
  if index < 0 {
    return package_name;
  }
  return package_name[index + 1:];
}

/**
* Checks whether or not a provided directory is a suitable Nox package (Contains .nox source files).
*
* @param directory_path The path to the directory.
* @return True if suitable otherwise false.
*/
package_is_directory_suitable :: proc(directory_path: string) -> bool {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  iterator := os_directory_iterator_make(directory_path);
  for iterator.is_valid {
    if os_has_extension(iterator.name, ".nox") {
      os_directory_iterator_destroy(&iterator);
      return true;
    }
    os_directory_iterator_next(&iterator);
  }
  return false;
}
