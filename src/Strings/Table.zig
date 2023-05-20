const std = @import("std");

// The table stores the string table section data and an array of slices to the
// data that divide the string table into the strings that it contains.

allocator: std.mem.Allocator,
data: []const u8,
slices: [][]const u8,

/// Returns the slice for the given byte offset.
/// For a well-defined terminfo, this function should never return `null`.
pub fn getSliceFromByteOffset(self: *const @This(), byte_offset: usize) ?[]const u8 {
    const S = struct {
        pub fn cmp(ctx: void, key: usize, mid_item: []const u8) std.math.Order {
            _ = ctx;
            return std.math.order(key, @ptrToInt(mid_item.ptr));
        }
    };
    const target_ptr = @ptrToInt(self.data.ptr) + byte_offset;
    const index = std.sort.binarySearch([]const u8, target_ptr, self.slices, {}, S.cmp) orelse return null;
    return self.slices[index];
}

/// Initialize and parse the string table.
pub fn init(allocator: std.mem.Allocator, section: []const u8) std.mem.Allocator.Error!@This() {
    // alloc space for section data
    var data = try allocator.alloc(u8, section.len);
    errdefer allocator.free(data);

    // move the section data into the allocated region
    @memcpy(data, section);

    // create arraylist for slices
    var slices = std.ArrayList([]const u8).init(allocator);
    // we'll eventually call .toOwnedSlice() on this array, so we only need
    // to deinit on error
    errdefer slices.deinit();

    var start: usize = 0;
    for (data, 0..) |char, end| {
        if (char == 0) {
            // we've found a string
            try slices.append(data[start..end]);
            // move start to the char after the sentinel
            start = end + 1;
        }
    }

    return .{
        .allocator = allocator,
        .data = data,
        .slices = try slices.toOwnedSlice(),
    };
}

// Deinitializes the string table.
// Note: the string capabilities rely on the string table.
pub fn deinit(self: @This()) void {
    self.allocator.free(self.data);
    self.allocator.free(self.slices);
}
