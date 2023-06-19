const std = @import("std");
pub const bools = @import("bools.zig");
pub const nums = @import("nums.zig");
pub const Strings = @import("Strings.zig");

test {
    _ = TermInfo.initFromEnv;
    _ = TermInfo.get_bool;
    _ = TermInfo.get_num;
    _ = TermInfo.get_names;
}

/// A TermInfo struct
pub const TermInfo = struct {
    const Self = @This();

    const Type = enum {
        Regular,
        Extended,

        pub fn getIntWidth(self: Type) usize {
            return switch (self) {
                .Regular => 2, // i16
                .Extended => 4, // i32
            };
        }
    };

    names: Names,
    bools: [bools.num_capabilities]bool,
    nums: [nums.num_capabilities]?i32,
    strings: Strings,
    size: usize,

    pub fn get_bool(self: *const Self, cap: bools.Capability) bool {
        return self.bools[@enumToInt(cap)];
    }

    pub fn get_num(self: *const Self, cap: nums.Capability) ?i32 {
        return self.nums[@enumToInt(cap)];
    }

    pub fn get_names(self: *const Self) *const Names {
        return &self.names;
    }

    /// Deinitializes and frees memory.
    pub fn deinit(self: Self) void {
        self.names.deinit();
        self.strings.deinit();
    }

    pub const InitFromEnvError = std.process.GetEnvVarOwnedError || InitFromTermError;
    pub fn initFromEnv(allocator: std.mem.Allocator) InitFromEnvError!Self {
        const term = try std.process.getEnvVarOwned(allocator, "TERM");
        defer allocator.free(term);

        return try initFromTerm(allocator, term);
    }

    pub const InitFromTermError = error{
        MissingTermInfoFile,
        InvalidTermName,
    } || InitFromFileError;
    pub fn initFromTerm(allocator: std.mem.Allocator, term: []const u8) InitFromTermError!Self {
        if (term.len == 0) {
            return error.InvalidTermName;
        }

        const first_char = term[0];
        var path_buf: [64]u8 = undefined;
        const path = std.fmt.bufPrint(&path_buf, "/usr/share/terminfo/{c}/{s}", .{ first_char, term }) catch {
            return error.InvalidTermName;
        };

        return try initFromFile(allocator, path);
    }

    pub const InitFromFileError = std.fs.File.OpenError || std.fs.File.ReadError || InitFromMemoryError;
    pub fn initFromFile(allocator: std.mem.Allocator, filepath: []const u8) InitFromFileError!Self {
        const file = try std.fs.openFileAbsolute(filepath, std.fs.File.OpenFlags{
            .mode = .read_only,
        });
        var buf: [4096]u8 = undefined;
        _ = try file.read(&buf);
        return try Self.initFromMemory(allocator, &buf);
    }

    pub const InitFromMemoryError = error{
        NotATermInfoError,
    } || std.mem.Allocator.Error;
    pub fn initFromMemory(allocator: std.mem.Allocator, memory: []const u8) InitFromMemoryError!Self {
        const getInt = @import("mem.zig").getInt;

        var offset: usize = 0;
        const magic_number = @bitCast(u16, getInt(i16, memory[offset .. offset + 2]));
        offset += 2;

        const typ: Type = switch (magic_number) {
            0o0432 => blk: {
                break :blk .Regular;
            },
            0o1036 => blk: {
                break :blk .Extended;
            },
            else => {
                return error.NotATermInfoError;
            },
        };

        // get section sizes
        const term_names_size = @bitCast(u16, getInt(i16, memory[offset .. offset + 2]));
        offset += 2;

        const bools_size = @bitCast(u16, getInt(i16, memory[offset .. offset + 2]));
        offset += 2;

        const nums_size = @bitCast(u16, getInt(i16, memory[offset .. offset + 2])) * typ.getIntWidth();
        offset += 2;

        const strings_size = @bitCast(u16, getInt(i16, memory[offset .. offset + 2])) * @sizeOf(i16);
        offset += 2;

        const str_table_size = @bitCast(u16, getInt(i16, memory[offset .. offset + 2]));
        offset += 2;

        std.debug.assert(offset == 12);

        // get sections
        const names_section = memory[offset .. offset + term_names_size];
        offset += term_names_size;

        const bools_section = memory[offset .. offset + bools_size];
        offset += bools_size;

        // nums section must start on an even byte
        if (offset % 2 != 0) {
            offset += 1;
        }

        const nums_section = memory[offset .. offset + nums_size];
        offset += nums_size;

        const strings_section = memory[offset .. offset + strings_size];
        offset += strings_size;

        const str_table_section = memory[offset .. offset + str_table_size];
        offset += str_table_size;

        // parse names section
        const names = try Names.init(allocator, names_section);

        // parse bools section
        const bools_data = bools.parse(bools_section);

        // parse nums section
        const nums_data = switch (typ) {
            .Regular => nums.parse(i16, nums_section),
            .Extended => nums.parse(i32, nums_section),
        };

        const strings = try Strings.init(allocator, strings_section, str_table_section);

        return TermInfo{
            .names = names,
            .bools = bools_data,
            .nums = nums_data,
            .strings = strings,
            .size = offset,
        };
    }
};

pub const Names = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    values: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator, section: []const u8) std.mem.Allocator.Error!Self {
        var names: std.ArrayList([]const u8) = std.ArrayList([]const u8).init(allocator);

        var start: u16 = 0;
        var end: u16 = 0;

        // ##### a b c | d e f g h |  i  j #####
        //       0 1 2 3 4 5 6 7 8 9 10 11

        while (end < section.len) {
            // find end of string
            while (end < section.len and section[end] != '|' and section[end] != 0) {
                end += 1;
            }

            const name_len: u16 = end - start;
            const is_empty = name_len == 0;
            if (!is_empty) {
                var dest: []u8 = try allocator.alloc(u8, name_len);
                @memcpy(dest, section[start..end]);
                try names.append(dest);
            }

            // move past '|'
            end += 1;
            // restart at next name
            start = end;
        }

        return Self{
            .allocator = allocator,
            .values = names,
        };
    }

    pub fn deinit(self: Self) void {
        for (self.values.items) |item| {
            self.allocator.free(item);
        }

        self.values.deinit();
    }

    pub inline fn getPrimary(self: *const Self) []const u8 {
        return self.values.items[0];
    }

    pub inline fn getAliases(self: *const Self) [][]const u8 {
        return self.names.items[1..];
    }
};

test {
    std.testing.refAllDeclsRecursive(@This());
}

test "basic" {
    const term_info = try TermInfo.initFromFile(std.testing.allocator, "/usr/share/terminfo/a/adm3a");
    defer term_info.deinit();
    const name = term_info.names.getPrimary();
    try std.testing.expectEqualSlices(u8, name, "adm3a");
}
