const std = @import("std");
const mem = @import("mem.zig");
const Self = @This();

pub const Table = @import("Strings/Table.zig");
pub const Capabilities = @import("Strings/Capabilities.zig");

capabilities: Capabilities,
table: Table,

pub fn init(allocator: std.mem.Allocator, strings_section: []const u8, string_table_section: []const u8) std.mem.Allocator.Error!@This() {
    const table = try Table.init(allocator, string_table_section);
    const capabilities = Capabilities.init(strings_section, table);
    return Self{
        .capabilities = capabilities,
        .table = table,
    };
}

pub fn deinit(self: @This()) void {
    self.table.deinit();
}

test "string capabilities" {
    const TermInfo = @import("main.zig").TermInfo;
    const term_info = try TermInfo.initFromFile(std.testing.allocator, "/usr/share/terminfo/a/adm3a");
    defer term_info.deinit();

    try std.testing.expectEqualSlices(u8, term_info.strings.capabilities.bell.?, &[_]u8{0x07});
    try std.testing.expectEqualSlices(u8, term_info.strings.capabilities.cursor_address.?, &[_]u8{0x1b} ++ "=%p1%' '%+%c%p2%' '%+%c");
}
