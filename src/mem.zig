const std = @import("std");

/// Returns the unsigned version of the given type.
pub fn unsigned(comptime N: type) type {
    // deconstruct the type into type info
    var info = @typeInfo(N);
    switch (info) {
        .Int => |*int| {
            int.signedness = .unsigned;
        },
        else => @compileError("unsigned() can only be called on integer types"),
    }
    // reconstruct the type
    return @Type(info);
}

/// Gets a little-endian integer of type N.
pub fn getInt(comptime N: type, memory: []const u8) N {
    std.debug.assert(@sizeOf(N) <= memory.len);
    var acc: N = 0;
    inline for (0..@sizeOf(N)) |i| {
        acc += @bitCast(N, @shlExact(@as(unsigned(N), memory[i]), i * 8));
    }
    return acc;
}

test "unsigned()" {
    try std.testing.expectEqual(unsigned(i32), u32);
}

test "getShortInt i32" {
    const bytes = [4]u8{ 0x1a, 0x32, 0x6e, 0x4f };
    try std.testing.expect(getInt(i32, &bytes) == 0x4f6e321a);
}

test "getShortInt i16" {
    const bytes = [2]u8{ 0x1a, 0x32 };
    try std.testing.expect(getInt(i16, &bytes) == 0x321a);
}
