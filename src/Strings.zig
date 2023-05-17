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

pub fn getStringByField(self: *const Self, comptime field: Capabilities.Field()) []const u8 {
    const field_name = @tagName(field);
    const slice = @field(self.capabilities, field_name) orelse unreachable;
    return self.table.getStringFromSlice(slice);
}

test "getStringBySlice" {
    const TermInfo = @import("main.zig").TermInfo;
    const term_info = (try TermInfo.initFromFile(std.testing.allocator, "/usr/share/terminfo/a/adm3a")).Regular;
    defer term_info.deinit();

    try std.testing.expectEqualSlices(u8, term_info.strings.getStringByField(.bell), &[_]u8{0x07});
}
