const std = @import("std");

const allocator = std.heap.page_allocator;

pub fn main() void {
    // const args = std.process.argsAlloc(allocator);
    var args = std.process.argsAlloc(allocator);
    // _ = args.skip();
    // const fileName = args.next();
    std.debug.print("FileName: {s}!\n", .{args[0]});
}
