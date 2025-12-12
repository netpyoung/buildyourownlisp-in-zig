const std = @import("std");
const module_builtin = @import("builtin");

const c_libedit = if (module_builtin.os.tag != .windows) @cImport({
    // https://salsa.debian.org/debian/libedit/-/blob/master/src/editline/readline.h
    @cInclude("editline/readline.h");
}) else struct {};

fn readLine(prompt: [:0]const u8) [*c]u8 {
    if (module_builtin.os.tag != .windows) {
        const input: [*c]u8 = c_libedit.readline(prompt);
        return input;
    }

    std.debug.print("{s}", .{prompt});
    // https://ziglang.org/download/0.15.1/release-notes.html#New-stdIoWriter-and-stdIoReader-API

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const reader = &stdin_reader.interface;
    const line = reader.takeDelimiterExclusive('\n') catch unreachable;

    const len = line.len;
    const total_len = len + 1; // +1 for null terminator

    const mem = std.c.malloc(total_len).?;
    const out: [*c]u8 = @ptrCast(mem);

    @memcpy(out[0..len], line[0..len]);
    out[len] = 0; // null terminator

    return out;
}

fn addHistory(input: [*c]u8) void {
    if (module_builtin.os.tag != .windows) {
        _ = c_libedit.add_history(input);
    }
}

pub fn main() !void {
    std.debug.print(
        \\ Lispy Version 0.0.0.0.1
        \\ Press Ctrl+c to Exit
        \\
    , .{});

    while (true) {
        const input = readLine("lispy> ");
        defer std.c.free(input);

        addHistory(input);

        std.debug.print("No you're a {s}\n", .{input});
    }
}

test "string test" {
    const expect = std.testing.expect;
    const expectEqual = std.testing.expectEqual;
    const expectEqualStrings = std.testing.expectEqualStrings;

    const allocator = std.testing.allocator;

    const z_str: []const u8 = "asdf";
    const c_str: [:0]u8 = try allocator.dupeZ(u8, z_str);
    defer allocator.free(c_str);

    try expectEqual(4, z_str.len);
    try expectEqual(4, c_str.len);
    try expect(std.mem.eql(u8, z_str, c_str));

    try expectEqual(0, c_str[4]);
    // try expectEqual(0, z_str[4]); // error: index 4 outside slice of length 4
    // try expectEqual(0, c_str[5]); // panic: index out of bounds: index 5, len 4

    try expectEqualStrings(z_str, c_str);
}
