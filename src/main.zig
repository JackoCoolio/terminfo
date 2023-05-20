const std = @import("std");
const Strings = @import("Strings.zig");

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
    bool_capabilities: BooleanCapabilities,
    num_capabilities: NumericCapabilities,
    strings: Strings,
    size: usize,

    pub fn getNames(self: *const Self) *const Names {
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
                std.log.err("invalid magic number: actual `0o{o}` != expected `0o0432` or `0o1036`", .{magic_number});
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
        const bools = BooleanCapabilities.init(bools_section);

        const nums = switch (typ) {
            .Regular => NumericCapabilities.init(i16, nums_section),
            .Extended => NumericCapabilities.init(i32, nums_section),
        };

        const strings = try Strings.init(allocator, strings_section, str_table_section);

        return TermInfo{
            .names = names,
            .bool_capabilities = bools,
            .num_capabilities = nums,
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

/// Boolean capabilities in the same order as `<term.h>`.
pub const BooleanCapabilities = struct {
    const Self = @This();

    auto_left_margin: bool,
    auto_right_margin: bool,
    no_esc_ctlc: bool,
    ceol_standout_glitch: bool,
    eat_newline_glitch: bool,
    erase_overstrike: bool,
    generic_type: bool,
    hard_copy: bool,
    has_meta_key: bool,
    has_status_line: bool,
    insert_null_glitch: bool,
    memory_above: bool,
    memory_below: bool,
    move_insert_mode: bool,
    move_standout_mode: bool,
    over_strike: bool,
    status_line_esc_ok: bool,
    dest_tabs_magic_smso: bool,
    tilde_glitch: bool,
    transparent_underline: bool,
    xon_xoff: bool,
    needs_xon_xoff: bool,
    prtr_silent: bool,
    hard_cursor: bool,
    non_rev_rmcup: bool,
    no_pad_char: bool,
    non_dest_scroll_region: bool,
    can_change: bool,
    back_color_erase: bool,
    hue_lightness_saturation: bool,
    col_addr_glitch: bool,
    cr_cancels_micro_mode: bool,
    has_print_wheel: bool,
    row_addr_glitch: bool,
    semi_auto_rigth_margin: bool,
    cpi_changes_res: bool,
    lpi_changes_res: bool,

    pub fn init(section: []const u8) Self {
        var capabilities: Self = std.mem.zeroes(Self);
        const fields = @typeInfo(Self).Struct.fields;
        inline for (fields, 0..) |field, byte_index| {
            const byte = section[byte_index];
            const value = if (byte == 1) true else if (byte == 0) false else unreachable;
            @field(capabilities, field.name) = value;
        }

        return capabilities;
    }
};

/// Numeric capabilities in the same order as `<term.h>`.
pub const NumericCapabilities = struct {
    const Self = @This();

    columns: ?i32,
    init_tabs: ?i32,
    lines: ?i32,
    lines_of_memory: ?i32,
    magic_cookie_glitch: ?i32,
    padding_baud_rate: ?i32,
    virtual_terminal: ?i32,
    width_status_line: ?i32,
    num_labels: ?i32,
    label_height: ?i32,
    label_width: ?i32,
    max_attributes: ?i32,
    maximum_windows: ?i32,
    max_colors: ?i32,
    max_pairs: ?i32,
    no_color_video: ?i32,
    buffer_capacity: ?i32,
    dot_vert_spacing: ?i32,
    dot_horz_spacing: ?i32,
    max_micro_address: ?i32,
    max_micro_jump: ?i32,
    micro_col_size: ?i32,
    micro_line_size: ?i32,
    number_of_pins: ?i32,
    output_res_char: ?i32,
    output_res_line: ?i32,
    output_res_horz_inch: ?i32,
    output_res_vert_inch: ?i32,
    print_rate: ?i32,
    wide_char_size: ?i32,
    buttons: ?i32,
    bit_image_entwining: ?i32,
    bit_image_type: ?i32,

    pub fn init(comptime N: type, section: []const u8) Self {
        const int_width = @sizeOf(N);
        var capabilities: NumericCapabilities = std.mem.zeroes(NumericCapabilities);
        const fields = @typeInfo(NumericCapabilities).Struct.fields;
        var int_i: usize = 0;
        inline for (fields) |field| {
            if (int_i >= section.len) {
                break;
            }
            const bytes = section[int_i .. int_i + int_width];
            const value = @import("mem.zig").getInt(N, bytes);

            if (value == -1) {
                // value of -1 means capability isn't supported
                @field(capabilities, field.name) = null;
            } else {
                @field(capabilities, field.name) = @as(i32, value);
            }

            int_i += int_width;
        }
        return capabilities;
    }
};

test "basic" {
    const term_info = try TermInfo.initFromFile(std.testing.allocator, "/usr/share/terminfo/a/adm3a");
    defer term_info.deinit();
    const name = term_info.names.getPrimary();
    try std.testing.expectEqualSlices(u8, name, "adm3a");
}

test "initFromEnv" {
    const term_info = try TermInfo.initFromEnv(std.testing.allocator);
    defer term_info.deinit();
    const name = term_info.names.getPrimary();
    try std.testing.expectEqualSlices(u8, name, "alacritty");
}
