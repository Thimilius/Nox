package nox

import "core:c/libc"
import "core:os"
import "core:runtime"
import "core:strings"
import "tracy"

OS_MAX_PATH :: 1024;

/**
* Represents a color used for colored console output.
*/
Console_Color :: enum {
  Black,
  Grey,
  White,

  Red,
  Green,
  Blue,
  Yellow,
  Magenta,
  Cyan,

  Dark_Red,
  Dark_Green,
  Dark_Blue,
  Dark_Yellow,
  Dark_Magenta,
  Dark_Cyan,
}

/**
* Checks whether or not a path has a specific file extension.
*
* @param path      The path to check.
* @param extension The extension to check.
* @return True if the path has the extension otherwise false.
*/
os_has_extension :: proc(path: string, extension: string) -> bool {
  index := strings.last_index(path, ".");
  if index < 0 {
    return false;
  }

  extension_in_path := path[index:];
  return extension_in_path == extension;
}

/**
* Checks whether a given file path exists on disk.
*
* @param path The path to check the existence for.
* @return True if the file exists on disk otherwise false.
*/
os_file_exists :: proc(path: string) -> bool {
  return os.exists(path);
}

/**
* Gets the directory of the running executable.
*
* @return The directory of the running executable.
*/
os_get_exe_directory :: proc() -> string {
  exe_path := os_get_exe_path();
  index := strings.last_index(exe_path, OS_PATH_SEPERATOR);
  assert(index > 0);
  return exe_path[:index + 1];
}

/**
* Read the text of a file.
*
* @param path The path of the file to read.
* @return 1. The text of the file; 2. True if reading was successful otherwise false.
*/
os_read_file :: proc(path: string) -> (string, bool) {
  tracy.ZoneC(ZONE_COLOR_DEFAULT);

  cpath := strings.clone_to_cstring(path);
  defer delete(cpath);

  file := libc.fopen(cpath, "rb");
  if file == nil {
    return "", false;
  }
  defer libc.fclose(file);

  libc.fseek(file, 0, libc.SEEK_END);
  length := libc.ftell(file);
  if length == 0 {
    return "", true;
  }

  libc.fseek(file, 0, libc.SEEK_SET);
  buffer: [dynamic]u8;
  resize_dynamic_array(&buffer, cast(int) length);
  
  if libc.fread(raw_data(buffer), cast(libc.size_t) length, 1, file) != 1 {
    delete(buffer);
    return "", false;
  }

  return strings.string_from_ptr(raw_data(buffer), cast(int) length), true;
}

/**
* Remove a file on disk.
*
* @param path The path of the file to remove.
*/
os_remove_file :: proc(path: string) {
  cpath := strings.clone_to_cstring(path);
  defer delete(cpath);
  libc.remove(cpath);
}

/**
* Runs a given system command.
*
* @param command The command to run.
*/
os_run_command :: proc(command: string) {
  command_cstr := strings.clone_to_cstring(command);
  defer delete(command_cstr);
  libc.system(command_cstr);
}

/**
* Represents an iterator to enumerate a directory.
*/
Directory_List_Iterator :: struct {
  is_valid: bool,     // Is the iterator still valid?
  is_error: bool,     // Has the iterator had an error?

  base: string,       // The base path of the directory.

  name: string,       // The name of the current entry (temporarily allocated).
  size: i64,          // The size of the current entry in bytes.
  is_directory: bool, // Is the current entry a directory?

  handle: uintptr,    // The native handle of the iterator.
}

/**
* Get all entries inside a directory.
*
* @param file_specification The file path specification filter for the iterator.
* @return The files inside the directory.
*/
os_directory_get_entries :: proc(file_specification: string) -> [dynamic]string {
  result: [dynamic]string;

  iterator := os_directory_iterator_make(file_specification);
  for iterator.is_valid {
    append(&result, iterator.name);
    os_directory_iterator_next(&iterator);
  }

  return result;
}

/**
* Should the current entry of a directory iterator be excluded.
*
* @param iterator The directory iterator to check.
* @return True if the current entry should be excluded otherwise false.
*/
os_directory_excluded :: proc(iterator: ^Directory_List_Iterator) -> bool {
  return iterator.is_valid && (iterator.name == "." || iterator.name == "..");
}
