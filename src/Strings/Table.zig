const std = @import("std");

allocator: std.mem.Allocator,
data: []const u8,
slices: []const Slice,

pub const Slice = struct {
    index: usize,
    len: usize,

    pub fn cmp(context: void, key: usize, mid_item: @This()) std.math.Order {
        _ = context;
        return std.math.order(key, mid_item.index);
    }
};

/// Returns the string within the given slice.
pub fn getStringFromSlice(self: *const @This(), slice: Slice) []const u8 {
    return self.data[slice.index .. slice.index + slice.len];
}

/// Returns the `Slice` for the given byte offset.
/// For a well-defined terminfo, this function should never return `null`.
pub fn getSliceFromByteOffset(self: *const @This(), byte_offset: usize) ?Slice {
    const index = std.sort.binarySearch(Slice, byte_offset, self.slices, {}, Slice.cmp) orelse return null;
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
    var slices = std.ArrayList(Slice).init(allocator);
    // we'll eventually call .toOwnedSlice() on this array, so we only need
    // to deinit on error
    errdefer slices.deinit();

    var start: usize = 0;
    for (data, 0..) |char, end| {
        if (char == 0) {
            // we've found a string
            try slices.append(Slice{
                .index = start,
                .len = end - start,
            });
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
