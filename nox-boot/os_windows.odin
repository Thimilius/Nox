package nox

import "core:c"
import "core:c/libc"
import "core:fmt"
import "core:log"
import "core:strings"
import "core:sys/windows"
import "tracy"

when ODIN_OS == .Windows {

  OS_PATH_SEPERATOR :: "\\";

  CP_UTF8 :: 65001;

  FOREGROUND_BLUE      :: 0x0001;
  FOREGROUND_GREEN     :: 0x0002;
  FOREGROUND_RED       :: 0x0004;
  FOREGROUND_INTENSITY :: 0x0008;

  _A_SUBDIR :: 0x10;

  ENOENT: i32 : 2;

  SMALL_RECT :: struct {
    Left: windows.SHORT,
    Top: windows.SHORT,
    Right: windows.SHORT,
    Bottom: windows.SHORT,
  }

  CONSOLE_SCREEN_BUFFER_INFO :: struct {
    dwSize: windows.COORD,
    dwCursorPosition: windows.COORD,
    wAttributes: windows.WORD,
    srWindow: SMALL_RECT,
    dwMaximumWindowSize: windows.COORD,
  }

  wcstring :: ^c.wchar_t;

  _wfinddata64_t :: struct {
    attrib: i32,
    time_create: i64,
    time_access: i64,
    time_write: i64,
    size: i64,
    name: [260]c.wchar_t,
  }

  foreign import kernel32 "system:Kernel32.lib"

  @(default_calling_convention="stdcall")
  foreign kernel32 {
    SetConsoleOutputCP :: proc(wCodePageID: windows.UINT) -> windows.BOOL ---;
    SetPriorityClass :: proc(hProcess: windows.HANDLE, dwPriorityClass: windows.DWORD) -> windows.BOOL ---;
    GetCurrentProcess :: proc() -> windows.HANDLE ---;
    SetConsoleTextAttribute :: proc(hConsoleOutput: windows.HANDLE, wAttributes: windows.WORD) -> windows.BOOL ---;
    GetConsoleScreenBufferInfo :: proc(hConsoleOutput: windows.HANDLE, lpConsoleScreenBufferInfo: ^CONSOLE_SCREEN_BUFFER_INFO) -> windows.BOOL ---;
    SetUnhandledExceptionFilter :: proc(p: proc "stdcall" (rawptr) -> windows.LONG) ---;

    _wfullpath :: proc(absPath: wcstring, relPath: wcstring, maxLength: c.size_t) -> wcstring ---;

    _wfindfirst64 :: proc(filespec: wcstring, fileinfo: ^_wfinddata64_t) -> i64 ---;
    _wfindnext64 :: proc(handle: uintptr, fileinfo: ^_wfinddata64_t) -> i32 ---;
    _findclose :: proc(handle: uintptr) -> i32 ---;
  }

  foreign import sdk_finder "../vendor/sdk-finder/lib/windows/sdk-finder.lib"

  FindResult :: struct {
    windows_sdk_version: i32,

    windows_sdk_root: wcstring,
    windows_sdk_um_library_path: wcstring,
    windows_sdk_ucrt_library_path: wcstring,

    is_vs_2017: b32,
    vs_exe_path: wcstring,
    vs_library_path: wcstring,
  }

  foreign sdk_finder {
    find_visual_studio_and_windows_sdk :: proc() -> FindResult ---;
    free_resources :: proc(result: ^FindResult) ---;
  }

  /**
  * Initialize the operating system.
  */
  os_initialize :: proc() {
    SetConsoleOutputCP(CP_UTF8);
    HIGH_PRIORITY_CLASS :: 0x00000080;
    SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

    when !ODIN_DEBUG {
      SetUnhandledExceptionFilter(os_crash_handler);
    }
  }

  os_get_l1_cache_line_size :: proc() -> uint {
    buffer_size: windows.DWORD;
    windows.GetLogicalProcessorInformation(nil, &buffer_size);
    buffer_length := buffer_size / size_of(windows.SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
    buffer := make_dynamic_array_len([dynamic]windows.SYSTEM_LOGICAL_PROCESSOR_INFORMATION, cast(int) buffer_length, context.temp_allocator);
    windows.GetLogicalProcessorInformation(raw_data(buffer), &buffer_size);

    line_size: uint;
    for info in buffer {
      if info.Relationship == .RelationCache && info.DummyUnion.Cache.Level == 1 && info.DummyUnion.Cache.Type == .CacheData {
        line_size = cast(uint) info.DummyUnion.Cache.LineSize;
        break;
      }
    }
    return line_size;
  }

  os_crash_handler :: proc "stdcall" (p: rawptr) -> windows.LONG {
    libc.printf("Internal compiler error!\n");
    
    EXCEPTION_CONTINUE_SEARCH :: 0;
    return EXCEPTION_CONTINUE_SEARCH;
  }

  /**
  * Gets the absolute path of a relative path.
  * 
  * @parma path The relative path.
  * @return The absolute path.
  */
  os_get_absolute_path :: proc(path: string) -> string {
    buffer: [windows.MAX_PATH_WIDE]c.wchar_t;

    wide_path := os_utf8_decode_t(path);

    _wfullpath(raw_data(buffer[:]), wide_path, windows.MAX_PATH_WIDE);
    return strings.clone(os_utf8_encode_t(raw_data(buffer[:])));
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

    file_specification := strings.concatenate({path, "*"}, context.temp_allocator);

    file_specification_wide := os_utf8_decode_t(file_specification);

    fileinfo: _wfinddata64_t;
    handle := _wfindfirst64(file_specification_wide, &fileinfo);
    iterator.handle = cast(uintptr) handle;

    os_directory_iterator_update(&iterator, handle == -1, &fileinfo);
    if os_directory_excluded(&iterator) {
      os_directory_iterator_next(&iterator);
    }

    return iterator;
  }

  /**
  * Gets the next entry of a directory iterator.
  * 
  * @parma iterator The directory iterator to get the next entry of.
  */
  os_directory_iterator_next :: proc(iterator: ^Directory_List_Iterator) {
    if !iterator.is_valid {
      return;
    }

    fileinfo: _wfinddata64_t;
    result := _wfindnext64(iterator.handle, &fileinfo);
    os_directory_iterator_update(iterator, result != 0, &fileinfo);
    if result != 0 {
      os_directory_iterator_destroy(iterator);
      return;
    }

    for os_directory_excluded(iterator) {
      fileinfo: _wfinddata64_t;
      result := _wfindnext64(iterator.handle, &fileinfo);
      os_directory_iterator_update(iterator, result != 0, &fileinfo);
      if result != 0 {
        os_directory_iterator_destroy(iterator);
        return;
      } 
    }
  }

  /**
  * Updates the state of a directory iterator.
  * 
  * @parma iterator The directory iterator to update the state of.
  */
  os_directory_iterator_update :: proc(iterator: ^Directory_List_Iterator, done: bool, fileinfo: ^_wfinddata64_t) {
    iterator.is_valid = !done;
    iterator.is_error = done && libc.errno()^ != ENOENT;
    if !done {
      iterator.size = fileinfo.size;
      iterator.name = os_utf8_encode_t(raw_data(fileinfo.name[:]));
      iterator.is_directory = (fileinfo.attrib & _A_SUBDIR) == _A_SUBDIR;
    }
  }

  /**
  * Destroys the resouces of a directory iterator.
  * 
  * @parma iterator The directory iterator to destroy.
  */
  os_directory_iterator_destroy :: proc(iterator: ^Directory_List_Iterator) {
    if iterator.is_valid {
      _findclose(iterator.handle);
      iterator.is_valid = false;
      iterator.is_error = false;
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

    console_color: windows.WORD;
    switch color {
      case .Black: console_color = 0;
      case .Grey: console_color = FOREGROUND_INTENSITY;
      case .White: console_color = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;

      case .Red: console_color = FOREGROUND_INTENSITY | FOREGROUND_RED;
      case .Green: console_color = FOREGROUND_INTENSITY | FOREGROUND_GREEN;
      case .Blue: console_color = FOREGROUND_INTENSITY | FOREGROUND_BLUE;
      case .Yellow: console_color = FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_GREEN;
      case .Magenta: console_color = FOREGROUND_INTENSITY | FOREGROUND_RED | FOREGROUND_BLUE;
      case .Cyan: console_color = FOREGROUND_INTENSITY | FOREGROUND_GREEN | FOREGROUND_BLUE;

      case .Dark_Red: console_color = FOREGROUND_RED;
      case .Dark_Green: console_color = FOREGROUND_GREEN;
      case .Dark_Blue: console_color = FOREGROUND_BLUE;
      case .Dark_Yellow: console_color = FOREGROUND_RED | FOREGROUND_GREEN;
      case .Dark_Magenta: console_color = FOREGROUND_RED | FOREGROUND_BLUE;
      case .Dark_Cyan: console_color = FOREGROUND_GREEN | FOREGROUND_BLUE;
    }

    info: CONSOLE_SCREEN_BUFFER_INFO;
    handle := windows.GetStdHandle(windows.STD_OUTPUT_HANDLE);
    GetConsoleScreenBufferInfo(handle, &info);
    SetConsoleTextAttribute(handle, console_color);
    fmt.printf(format, ..args);
    SetConsoleTextAttribute(handle, info.wAttributes);
  }

  /**
  * Invokes the native system linker.
  * 
  * @param arguments         The compilation arguments to use.
  * @param libraries_to_link The libraries to link to.
  */
  os_invoke_linker :: proc(arguments: Compilation_Arguments, libraries_to_link: []string) {
    tracy.ZoneC(ZONE_COLOR_DEFAULT);

    result := find_visual_studio_and_windows_sdk();
    if result.windows_sdk_version == 0 {
      report_error_fatal("Failed to find windows and visual studio SDK");
    }

    defer free_resources(&result);

    windows_library_path := os_utf8_encode_t(result.windows_sdk_um_library_path);
    universal_c_runtime_library_path := os_utf8_encode_t(result.windows_sdk_ucrt_library_path);
    visual_studio_exe_path := os_utf8_encode_t(result.vs_exe_path);
    visual_studio_library_path := os_utf8_encode_t(result.vs_library_path);

    // We assume the corresponding .obj already exists.

    // NOTE: We need "legacy_stdio_definitions.lib" for functions like "printf".
    // We need "msvcrt.lib" for the C runtime library (linking it as /MD) as we are relying on it.
    // This means we have "mainCRTStartup" as the entry point to the runtime (Otherwise without the runtime we would need "/ENTRY:main").
    // More info here: https://docs.microsoft.com/en-us/cpp/c-runtime-library/crt-library-features?view=msvc-170

    // NOTE: Because of Windows we need to wrap the whole command in quotes as the command processor otherwise does not understand anything.
    // See here for details: https://stackoverflow.com/questions/9964865/c-system-not-working-when-there-are-spaces-in-two-different-parameters
    builder: strings.Builder;
    defer strings.builder_destroy(&builder);

    strings.write_string(&builder, "\"");
    
    strings.write_string(&builder, "\"");
    strings.write_string(&builder, visual_studio_exe_path);
    strings.write_string(&builder, "\\link.exe\" ");

    strings.write_string(&builder, "/NOLOGO ");
    strings.write_string(&builder, "/SUBSYSTEM:console ");
    strings.write_string(&builder, "/LIBPATH:\"");
    strings.write_string(&builder, windows_library_path);
    strings.write_string(&builder, "\" /LIBPATH:\"");
    strings.write_string(&builder, universal_c_runtime_library_path);
    strings.write_string(&builder, "\" /LIBPATH:\"");
    strings.write_string(&builder, visual_studio_library_path);
    strings.write_string(&builder, "\" ");
    strings.write_string(&builder, "/INCREMENTAL:NO "); 
    if arguments.build_debug {
      strings.write_string(&builder, "/PDB:");
      strings.write_string(&builder, arguments.output_name);
      strings.write_string(&builder, ".pdb ");
      strings.write_string(&builder, "/DEBUG ");
    }
    strings.write_string(&builder, arguments.output_name);
    strings.write_string(&builder, ".obj libcmt.lib legacy_stdio_definitions.lib ");

    current_directory := os_get_current_directory();
    defer delete(current_directory);

    for library, i in libraries_to_link {
      strings.write_string(&builder, "\"");
      strings.write_string(&builder, library);
      strings.write_string(&builder, "\"");
      strings.write_string(&builder, " ");
    }

    strings.write_string(&builder, arguments.extra_linker_flags);

    strings.write_string(&builder, "\"");
    linker_command := strings.to_string(builder);
    if arguments.verbose {
      log.logf(.Info, "Executing linker: %v\n", linker_command);
    }

    os_run_command(linker_command);

    if !arguments.keep_intermediate {
      strings.builder_reset(&builder);
      strings.write_string(&builder, arguments.output_name);
      strings.write_string(&builder, ".obj");
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
    buffer: [OS_MAX_PATH]windows.WCHAR;
    windows.GetModuleFileNameW(nil, raw_data(&buffer), OS_MAX_PATH);
    return strings.clone(os_utf8_encode_t(raw_data(&buffer)));
  }

  /**
  * Gets the current directory of the process.
  * 
  * @return The current directory of the process.
  */
  os_get_current_directory :: proc() -> string {
    buffer: [OS_MAX_PATH]windows.WCHAR;
    windows.GetCurrentDirectoryW(OS_MAX_PATH, raw_data(&buffer));

    current_directory := os_utf8_encode_t(raw_data(&buffer));
    if strings.has_suffix(current_directory, OS_PATH_SEPERATOR) {
      return strings.clone(current_directory);
    } else {
      directory := strings.concatenate({current_directory, OS_PATH_SEPERATOR});
      return directory;
    }
  }

  /**
  * Encodes an UTF-16 string to UTF-8 (temporarily allocated).
  * 
  * @param str The UTF-16 string to encode.
  * @return The encoded UTF-8 string.
  */
  os_utf8_encode_t :: proc(str: wcstring) -> string {
    in_length: i32 = cast(i32) libc.wcslen(str);
    
    out_length := windows.WideCharToMultiByte(CP_UTF8, 0, str, in_length, nil, 0, nil, nil);
    builder := strings.builder_make(cast(int) out_length, cast(int) out_length, context.temp_allocator);
    windows.WideCharToMultiByte(CP_UTF8, 0, str, in_length, raw_data(builder.buf), out_length, nil, nil)

    return strings.to_string(builder);
  }

  /**
  * Decodes an UTF-8 string to UTF-16 (temporarily allocated).
  * 
  * @param str The UTF-8 string to decode.
  * @return The decoded UTF-16 string.
  */
  os_utf8_decode_t :: proc(str: string) -> wcstring {
    in_length: i32 = cast(i32) len(str);

    out_length := windows.MultiByteToWideChar(CP_UTF8, 0, raw_data(str), in_length, nil, 0);
    buffer := make_dynamic_array_len_cap([dynamic]c.wchar_t, out_length, out_length, context.temp_allocator);
    windows.MultiByteToWideChar(CP_UTF8, 0, raw_data(str), in_length, raw_data(buffer), out_length);

    return raw_data(buffer);
  }

}
