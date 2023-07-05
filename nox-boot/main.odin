package nox

import "core:fmt"
import "core:mem"
import "tracy"

TRACY_ENABLE :: #config(TRACY_ENABLE, false)

/**
* Main entry point.
*/
main :: proc() {
  when ODIN_DEBUG {
    // Setup tracking allocator.
    track: mem.Tracking_Allocator;
    defer mem.tracking_allocator_destroy(&track);
    mem.tracking_allocator_init(&track, context.allocator);
    context.allocator = mem.tracking_allocator(&track);
  }

  when TRACY_ENABLE {
    for !tracy.IsConnected() {}

    tracy.SetThreadName("main");

    // Setup additional tracking allocator for Tracy.
    context.allocator = tracy.MakeProfiledAllocator(
      self              = &tracy.ProfiledAllocatorData{},
      callstack_size    = 32,
      backing_allocator = context.allocator,
      secure            = true,
    );
  }

  nox_main();

  when ODIN_DEBUG {
    for _, leak in track.allocation_map {
      os_print_colored(.Red, "Compiler leaked %v bytes in %v\n", leak.size, leak.location);
    }
    for bad_free in track.bad_free_array {
      os_print_colored(.Red, "Compiler badly freed in %v\n", bad_free.location);
    }
  }
}
