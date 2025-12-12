const std = @import("std");
const module_builtin = @import("builtin");

const c_libedit = if (module_builtin.os.tag != .windows) @cImport({
    // https://salsa.debian.org/debian/libedit/-/blob/master/src/editline/readline.h
    @cInclude("editline/readline.h");
}) else struct {};

const c_mpc = @cImport({
    @cInclude("mpc.h");
});

fn readLine(prompt: [:0]const u8) [*c]u8 {
    if (module_builtin.os.tag != .windows) {
        const input: [*c]u8 = c_libedit.readline(prompt);
        return input;
    }

    std.debug.print("{s}", .{prompt});

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const reader = &stdin_reader.interface;
    const line = reader.takeDelimiterExclusive('\n') catch unreachable;

    const len = line.len;
    const total_len = len + 1; // +1 for null terminator

    const mem = std.c.malloc(total_len).?;
    const out: [*c]u8 = @ptrCast(mem);

    @memcpy(out[0..len], line);
    out[len] = 0; // null terminator

    return out;
}

fn addHistory(input: [*c]u8) void {
    if (module_builtin.os.tag != .windows) {
        _ = c_libedit.add_history(input);
    }
}

fn eval_op(x: i64, op: [*:0]const u8, y: i64) i64 {
    if (std.mem.eql(u8, std.mem.span(op), "+")) {
        return x + y;
    }
    if (std.mem.eql(u8, std.mem.span(op), "-")) {
        return x - y;
    }
    if (std.mem.eql(u8, std.mem.span(op), "*")) {
        return x * y;
    }
    if (std.mem.eql(u8, std.mem.span(op), "/")) {
        return @divTrunc(x, y);
    }
    return 0;
}

pub fn eval(t: *c_mpc.mpc_ast_t) i64 {
    // number 노드인 경우 바로 값 반환
    if (std.mem.indexOf(u8, std.mem.span(t.tag), "number") != null) {
        return std.fmt.parseInt(i64, std.mem.span(t.contents), 10) catch 0;
    }

    // 연산자(op)는 두 번째 자식의 contents
    const op = t.children[1].*.contents;

    // 세 번째 자식을 먼저 계산
    var x = eval(t.children[2]);

    // 나머지 자식 노드를 반복하며 계산
    var i: usize = 3;
    while (i < t.children_num and std.mem.indexOf(u8, std.mem.span(t.children[i].*.tag), "expr") != null) {
        const right = eval(t.children[i]);
        x = eval_op(x, op, right);
        i += 1;
    }

    return x;
}

pub fn main() void {
    const Number = c_mpc.mpc_new("number");
    const Operator = c_mpc.mpc_new("operator");
    const Expr = c_mpc.mpc_new("expr");
    const Lispy = c_mpc.mpc_new("lispy");
    defer c_mpc.mpc_cleanup(4, Number, Operator, Expr, Lispy);

    const lang =
        \\ number   : /-?[0-9]+/ ;
        \\ operator : '+' | '-' | '*' | '/' ;
        \\ expr     : <number> | '(' <operator> <expr>+ ')' ;
        \\ lispy    : /^/ <operator> <expr>+ /$/ ;
    ;

    if (c_mpc.mpca_lang(c_mpc.MPCA_LANG_DEFAULT, lang, Number, Operator, Expr, Lispy) != 0) {
        std.debug.print("Failed to define grammar\n", .{});
        return;
    }

    std.debug.print("Lispy Version 0.0.0.0.2\n", .{});
    std.debug.print("Press Ctrl+c to Exit\n\n", .{});

    while (true) {
        const input = readLine("lispy> ");
        defer std.c.free(input);

        // Prepare result struct
        var result: c_mpc.mpc_result_t = undefined;

        if (c_mpc.mpc_parse("<stdin>", input, Lispy, &result) != 0) {
            const output: *c_mpc.mpc_ast_t = @ptrCast(@alignCast(result.output));
            defer c_mpc.mpc_ast_delete(output);

            const x = eval(output);
            std.debug.print("{d}\n", .{x});
        } else {
            c_mpc.mpc_err_print(result.@"error");
            c_mpc.mpc_err_delete(result.@"error");
        }
    }
}
