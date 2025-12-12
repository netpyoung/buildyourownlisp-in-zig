const std = @import("std");
const module_builtin = @import("builtin");

const c_libedit = if (module_builtin.os.tag != .windows) @cImport({
    // https://salsa.debian.org/debian/libedit/-/blob/master/src/editline/readline.h
    @cInclude("editline/readline.h");
}) else struct {};

const c_mpc = @cImport({
    @cInclude("mpc.h");
});

const assert = std.debug.assert;

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

// ===================================================

fn zstr_Equal(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

fn cstr_Equal(a: [*:0]const u8, b: [:0]const u8) bool {
    return std.mem.orderZ(u8, a, b) == .eq;
}

fn cstr_Contains(a: [*:0]const u8, b: [:0]const u8) bool {
    return std.mem.indexOf(u8, std.mem.span(a), b) != null;
}

// ===================================================

const E_LVAL = enum(i32) {
    ERR = 0,
    NUM,
    SYM,
    SEXPR,
};

const Lval = struct {
    Type: E_LVAL = E_LVAL.ERR,
    Num: i64 = 0,
    Err: []const u8 = undefined,
    Sym: []const u8 = undefined,
    Cell: std.ArrayList(*Lval) = undefined,

    pub fn Dispose(v: *Lval) void {
        const allocator = std.heap.page_allocator;

        switch (v.Type) {
            .NUM => {},
            .ERR => {
                allocator.free(v.Err);
            },
            .SYM => {
                allocator.free(v.Sym);
            },
            .SEXPR => {},
        }

        for (v.Cell.items) |item| {
            item.Dispose();
        }
        v.Cell.deinit(allocator);
        allocator.destroy(v);
    }

    pub fn Print(val: *const Lval) void {
        switch (val.Type) {
            .NUM => {
                std.debug.print("{d}", .{val.Num});
            },
            .ERR => {
                std.debug.print("Error: {s}", .{val.Err});
            },
            .SYM => {
                std.debug.print("{s}", .{val.Sym});
            },
            .SEXPR => {
                lval_expr_print(val, '(', ')');
            },
        }
    }

    pub fn Println(val: *const Lval) void {
        val.Print();
        std.debug.print("\n", .{});
    }

    fn lval_expr_print(val: *const Lval, open: u8, close: u8) void {
        std.debug.print("{c}", .{open});
        for (val.Cell.items, 0..) |item, i| {
            item.Print();
            if (i != val.Cell.items.len - 1) {
                std.debug.print(" ", .{});
            }
        }
        std.debug.print("{c}", .{close});
    }

    fn Add(v: *Lval, x: *Lval) *Lval {
        assert(v.Type == .SEXPR);

        const allocator = std.heap.page_allocator;

        v.Cell.append(allocator, x) catch unreachable;
        return v;
    }

    fn Pop(v: *Lval, i: usize) *Lval {
        assert(v.Type == .SEXPR);

        const ret = v.Cell.orderedRemove(i);
        return ret;
    }

    fn Take(v: *Lval, i: usize) *Lval {
        assert(v.Type == .SEXPR);

        const x = Pop(v, i);
        v.Dispose();
        return x;
    }
};

fn lval_num(x: i64) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .NUM;
    v.Cell = std.ArrayList(*Lval).empty;
    v.Num = x;
    return v;
}

fn lval_err(msg: []const u8) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .ERR;
    v.Cell = std.ArrayList(*Lval).empty;
    v.Err = allocator.dupe(u8, msg) catch unreachable;
    return v;
}

fn lval_sym(msg: []const u8) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch undefined;
    v.Type = .SYM;
    v.Cell = std.ArrayList(*Lval).empty;

    v.Sym = allocator.dupe(u8, msg) catch unreachable;
    return v;
}

fn lval_sexpr() *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .SEXPR;
    v.Cell = std.ArrayList(*Lval).empty;
    return v;
}

fn builtin_op(a: *Lval, op: []const u8) *Lval {
    defer a.Dispose();

    for (a.Cell.items) |item| {
        if (item.Type != .NUM) {
            return lval_err("Cannot operate on non-number!");
        }
    }

    const x = a.Pop(0);
    if (zstr_Equal(op, "-") and a.Cell.items.len == 0) {
        x.Num = -x.Num;
    }

    while (a.Cell.items.len > 0) {
        const y = a.Pop(0);
        defer y.Dispose();

        if (zstr_Equal(op, "+")) {
            x.Num += y.Num;
        } else if (zstr_Equal(op, "-")) {
            x.Num -= y.Num;
        } else if (zstr_Equal(op, "*")) {
            x.Num *= y.Num;
        } else if (zstr_Equal(op, "/")) {
            if (y.Num == 0) {
                return lval_err("Division By Zero.");
            } else {
                x.Num = @divTrunc(x.Num, y.Num);
            }
        }
    }
    return x;
}

fn lval_eval_sexpr(v: *Lval) *Lval {
    for (0.., v.Cell.items) |i, item| {
        const evaled = lval_eval(item);
        v.Cell.items[i] = evaled;
    }

    for (0.., v.Cell.items) |i, item| {
        if (item.Type == .ERR) {
            return v.Take(i);
        }
    }

    if (v.Cell.items.len == 0) {
        return v;
    }

    if (v.Cell.items.len == 1) {
        return v.Take(0);
    }

    const first = v.Pop(0);
    defer first.Dispose();

    if (first.Type != .SYM) {
        v.Dispose();
        return lval_err("S-expression Does not start with symbol.");
    }

    const result = builtin_op(v, first.Sym);
    return result;
}

fn lval_eval(v: *Lval) *Lval {
    if (v.Type == .SEXPR) {
        return lval_eval_sexpr(v);
    }
    return v;
}

fn lval_read_num(t: *c_mpc.mpc_ast_t) *Lval {
    const slice = std.mem.span(t.contents);

    const parsed = std.fmt.parseInt(i64, slice, 10) catch {
        return lval_err("invalid number");
    };

    return lval_num(parsed);
}

fn lval_read(t: *c_mpc.mpc_ast_t) *Lval {
    if (cstr_Contains(t.tag, "number")) {
        return lval_read_num(t);
    }
    if (cstr_Contains(t.tag, "symbol")) {
        return lval_sym(std.mem.span(t.contents));
    }

    var x: *Lval = undefined;
    if (cstr_Equal(t.tag, ">")) {
        x = lval_sexpr();
    } else if (cstr_Contains(t.tag, "sexpr")) {
        x = lval_sexpr();
    }

    for (0..@intCast(t.children_num), t.children) |_, child| {
        if (cstr_Equal(child.*.contents, "(")) {
            continue;
        }
        if (cstr_Equal(child.*.contents, ")")) {
            continue;
        }
        if (cstr_Equal(child.*.tag, "regex")) {
            continue;
        }

        const child_val = lval_read(child);
        x = x.Add(child_val);
    }

    return x;
}

// ===================================================
pub fn main() void {
    const Number = c_mpc.mpc_new("number");
    const Symbol = c_mpc.mpc_new("symbol");
    const Sexpr = c_mpc.mpc_new("sexpr");
    const Expr = c_mpc.mpc_new("expr");
    const Lispy = c_mpc.mpc_new("lispy");
    defer c_mpc.mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);

    const lang =
        \\ number : /-?[0-9]+/ ;
        \\ symbol : '+' | '-' | '*' | '/' ;
        \\ sexpr  : '(' <expr>* ')' ;
        \\ expr   : <number> | <symbol> | <sexpr> ;
        \\ lispy  : /^/ <expr>* /$/ ;
    ;

    if (c_mpc.mpca_lang(c_mpc.MPCA_LANG_DEFAULT, lang, Number, Symbol, Sexpr, Expr, Lispy) != 0) {
        std.debug.print("Failed to define grammar\n", .{});
        return;
    }

    std.debug.print("Lispy Version 0.0.0.0.2\n", .{});
    std.debug.print("Press Ctrl+c to Exit\n\n", .{});

    while (true) {
        const input: [*c]u8 = readLine("lispy> ");
        defer std.c.free(input);

        var result: c_mpc.mpc_result_t = undefined;
        if (c_mpc.mpc_parse("<stdin>", input, Lispy, &result) != 0) {
            const output: *c_mpc.mpc_ast_t = @ptrCast(@alignCast(result.output));
            defer c_mpc.mpc_ast_delete(output);

            c_mpc.mpc_ast_print(output);

            const r: *Lval = lval_read(output);
            const e: *Lval = lval_eval(r);
            e.Println();
            e.Dispose();
        } else {
            c_mpc.mpc_err_print(result.@"error");
            c_mpc.mpc_err_delete(result.@"error");
        }
    }
}
