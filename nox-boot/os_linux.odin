package nox

import "core:c"
import "core:c/libc"
import "core:fmt"
import "core:log"
import "core:os"
import "core:strings"
import "tracy"

when ODIN_OS == .Linux {

  OS_PATH_SEPERATOR :: "/";
  _SC_LEVEL1_DCACHE_LINESIZE :: 190;

  foreign {
    opendir :: proc(name: cstring) -> rawptr ---;
    readdir :: proc(dirp: rawptr) -> ^os.Dirent ---;
    getpid :: proc() -> i32 ---;
    sysconf :: proc(name: i32) -> i64 ---;
    getcwd	:: proc(buf: cstring, len: c.size_t) -> cstring ---;
    readlink	:: proc(path: cstring, buf: ^byte, bufsiz: c.size_t) -> c.ssize_t ---;
  }

  /**
  * Initialize the operating system.
  */
  os_initialize :: proc() {
    // For now we don't need to do anything on Linux.
  }

  os_get_l1_cache_line_size :: proc() -> uint {
    return cast(uint) sysconf(_SC_LEVEL1_DCACHE_LINESIZE);
  }

  /**
  * Gets the absolute path of a relative path.
  * 
  * @parma path The relative path.
  * @return The absolute path.
  */
  os_get_absolute_path :: proc(path: string) -> string {
    buffer: [OS_MAX_PATH]c.char;
    os._unix_realpath(cstring(raw_data(path)), &buffer);
    return strings.clone_from_cstring(cast(cstring) cast(^u8) &buffer);
  }

  /**
  * Makes a new directory iterator.
  * 
  * @parma path The base path specification for the directory iterator.
  * @return The new directory iterator.
  */
  os_directory_iterator_make :: proc(path: string) -> Directory_List_Iterator {
    iterator: Directory_List_Iterator;
    iterator.base = path;

    cpath := strings.clone_to_cstring(path, context.temp_allocator);

    dir := opendir(cpath);
    if dir == nil {
      iterator.is_valid = false;
      iterator.is_error = true;
      return iterator;
    }

    iterator.handle = cast(uintptr) dir;
    iterator.is_valid = true;

    os_directory_iterator_next(&iterator);

    return iterator;
  }

  /**
  * Gets the next entry of a directory iterator.
  * 
  * @parma iterator The directory iterator to get the next entry of.
  */
  os_directory_iterator_next :: proc(iterator: ^Directory_List_Iterator) {
    if !iterator.is_valid do return;

    os_directory_iterator_update(iterator);
    for os_directory_excluded(iterator) {
      os_directory_iterator_update(iterator);
    }
  }
  
  /**
  * Updates the state of a directory iterator.
  * 
  * @parma iterator The directory iterator to update the state of.
  */
  os_directory_iterator_update :: proc(iterator: ^Directory_List_Iterator) {
    entry := readdir(cast(os.Dir) iterator.handle);
    if entry == nil {
      os_directory_iterator_destroy(iterator);
      return;
    }
    iterator.name = strings.clone_from_cstring(cast(cstring) cast(^u8) &entry.name, context.temp_allocator);
    iterator.is_directory = (entry.type & 4) == 4;
  }
  
  /**
  * Destroys the resouces of a directory iterator.
  * 
  * @parma iterator The directory iterator to destroy.
  */
  os_directory_iterator_destroy :: proc(iterator: ^Directory_List_Iterator) {
    if iterator.is_valid {
      iterator.is_valid = false;
      iterator.is_error = false;
      os._unix_closedir(cast(os.Dir) iterator.handle);
    }
  }

  /**
  * Prints a colored formatted message to the console.
  * 
  * @parma color  The color to use.
  * @parma format The format of the message to print.
  * @parma args   The format arguments.
  */
  os_print_colored :: proc(color: Console_Color, format: string, args: ..any) {
    tracy.ZoneC(ZONE_COLOR_DEFAULT);

    fmt.printf("\033[");

    switch color {
      case .Black: fmt.printf("30m");
      case .Grey: fmt.printf("37m");
      case .White: fmt.printf("1;37m");

      case .Red: fmt.printf("1;31m");
      case .Green: fmt.printf("1;32m");
      case .Blue: fmt.printf("1;34m");
      case .Yellow: fmt.printf("1;33m");
      case .Magenta: fmt.printf("1;35m");
      case .Cyan: fmt.printf("1;36m");

      case .Dark_Red: fmt.printf("31m");
      case .Dark_Green: fmt.printf("32m");
      case .Dark_Blue: fmt.printf("34m");
      case .Dark_Yellow: fmt.printf("33m");
      case .Dark_Magenta: fmt.printf("35m");
      case .Dark_Cyan: fmt.printf("36m");
    }

    fmt.printf(format, ..args);
    fmt.printf("\033[0m");
  }

  /**
  * Invokes the native system linker.
  * 
  * @param output_name       The name of the output.
  * @param arguments         The compilation arguments to use.
  * @param libraries_to_link The libraries to link to.
  */
  os_invoke_linker :: proc(arguments: Compilation_Arguments, libraries_to_link: []string) {
    tracy.ZoneC(ZONE_COLOR_DEFAULT);

    // We assume 'clang' is on the PATH.
    builder: strings.Builder;
    defer strings.builder_destroy(&builder);

    strings.write_string(&builder, "clang ");
    strings.write_string(&builder, "-o ");
    strings.write_string(&builder, arguments.output_name);
    strings.write_string(&builder, " ");
    strings.write_string(&builder, arguments.output_name);
    strings.write_string(&builder, ".o ");
    
    for library in libraries_to_link {
      strings.write_string(&builder, library);
      strings.write_string(&builder, " ");
    }
    
    strings.write_string(&builder, arguments.extra_linker_flags);

    linker_command := strings.to_string(builder);
    if arguments.verbose {
      log.logf(.Info, "Executing linker: %v\n", linker_command);
    }

    os_run_command(linker_command);

    if !arguments.keep_intermediate {
      strings.builder_reset(&builder);
      strings.write_string(&builder, arguments.output_name);
      strings.write_string(&builder, ".o");
      object_file_name := strings.to_string(builder);
      os_remove_file(object_file_name);
    }
  }

  /**
  * Gets the path of the running executable.
  * 
  * @return The path of the running executable.
  */
  os_get_exe_path :: proc() -> string {
    pid := getpid();
    
    pid_path := fmt.tprintf("/proc/%v/exe", pid);
    cpid_path := strings.clone_to_cstring(pid_path, context.temp_allocator);

    buffer: [OS_MAX_PATH]c.char;
    readlink(cpid_path, raw_data(&buffer), OS_MAX_PATH);

    return strings.clone_from_cstring(cast(cstring) cast(^u8) &buffer);
  }

  /**
  * Gets the current directory of the process.
  * 
  * @return The current directory of the process.
  */
  os_get_current_directory :: proc() -> string {
    buffer: [OS_MAX_PATH]c.char;
    getcwd(cast(cstring) raw_data(&buffer), OS_MAX_PATH);

    current_directory := strings.clone_from_cstring(cast(cstring) cast(^u8) &buffer);
    if strings.has_suffix(current_directory, OS_PATH_SEPERATOR) {
      return current_directory;
    } else {
      directory := strings.concatenate({current_directory, OS_PATH_SEPERATOR});
      delete(current_directory);
      return directory;
    }
  }

}
