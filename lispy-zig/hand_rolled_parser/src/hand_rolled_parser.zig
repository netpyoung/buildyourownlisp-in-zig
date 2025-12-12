const std = @import("std");
const module_builtin = @import("builtin");

const c_libedit = if (module_builtin.os.tag != .windows) @cImport({
    // https://salsa.debian.org/debian/libedit/-/blob/master/src/editline/readline.h
    @cInclude("editline/readline.h");
}) else struct {};

const c_string = @cImport({
    @cInclude("string.h");
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

fn zstr_Contains(a: []const u8, b: []const u8) bool {
    return std.mem.indexOf(u8, a, b) != null;
}

fn cstr_Equal(a: [*:0]const u8, b: [:0]const u8) bool {
    return std.mem.orderZ(u8, a, b) == .eq;
}

fn cstr_Contains(a: [*:0]const u8, b: [:0]const u8) bool {
    return std.mem.indexOf(u8, std.mem.span(a), b) != null;
}

fn LASSERT(v: *Lval, condition: bool, comptime fmt: []const u8, args_fmt: anytype) ?*Lval {
    if (condition) {
        return null;
    }

    const err = lval_err_format(fmt, args_fmt);
    v.Dispose();
    return err;
}

fn LASSERT_TYPE(func: []const u8, v: *Lval, index: usize, expect: E_LVAL) ?*Lval {
    return LASSERT(v, v.Cell.items[index].Type == expect, "Function '{s}' passed incorrect type for argument {d}. Got {s}, Expected {s}.", .{ func, index, v.Cell.items[index].Type.Name(), expect.Name() });
}

fn LASSERT_NUM(func: []const u8, v: *Lval, num: usize) ?*Lval {
    return LASSERT(v, v.Cell.items.len == num, "Function '{s}' passed incorrect number of arguments. Got {d}, Expected {d}.", .{ func, v.Cell.items.len, num });
}

fn LASSERT_NOT_EMPTY(func: []const u8, v: *Lval, index: usize) ?*Lval {
    return LASSERT(v, v.Cell.items[index].Cell.items.len != 0, "Function '{s}' passed {{}} for argument {d}.", .{ func, index });
}

// ===================================================

const E_LVAL = enum(i32) {
    ERR = 0,
    NUM,
    SYM,
    SEXPR,
    QEXPR,
    FUN,
    STR,

    fn Name(t: E_LVAL) []const u8 {
        switch (t) {
            .FUN => return "Function",
            .NUM => return "Number",
            .ERR => return "Error",
            .SYM => return "Symbol",
            .SEXPR => return "S-Expression",
            .QEXPR => return "Q-Expression",
            .STR => return "String",
            // else => return "Unknown",
        }
    }
};

const Lval = struct {
    Type: E_LVAL = E_LVAL.ERR,

    // basic
    Num: i64 = 0,
    Err: []const u8 = undefined,
    Sym: []const u8 = undefined,
    Str: []const u8 = undefined,

    // function
    BuiltinOrNull: ?*const lbuiltin,
    Env: *Lenv,
    Formals: *Lval,
    Body: *Lval,

    // expression
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
            .QEXPR => {},
            .FUN => {
                if (v.BuiltinOrNull == null) {
                    v.Env.Dispose();
                    v.Formals.Dispose();
                    v.Body.Dispose();
                }
            },
            .STR => {
                allocator.free(v.Str);
            },
        }

        for (v.Cell.items) |item| {
            item.Dispose();
        }
        v.Cell.deinit(allocator);
        allocator.destroy(v);
    }

    pub fn Clone(v: *Lval) *Lval {
        const allocator = std.heap.page_allocator;

        const x: *Lval = allocator.create(Lval) catch unreachable;
        x.Type = v.Type;

        switch (v.Type) {
            .NUM => {
                x.Num = v.Num;
            },
            .ERR => {
                x.Err = allocator.dupe(u8, v.Err) catch unreachable;
            },
            .SYM => {
                x.Sym = allocator.dupe(u8, v.Sym) catch unreachable;
            },
            .SEXPR, .QEXPR => {},
            .FUN => {
                if (v.BuiltinOrNull) |builtin| {
                    x.BuiltinOrNull = builtin;
                } else {
                    x.BuiltinOrNull = null;
                    x.Env = v.Env.Clone();
                    x.Formals = v.Formals.Clone();
                    x.Body = v.Body.Clone();
                }
            },
            .STR => {
                x.Str = allocator.dupe(u8, v.Str) catch unreachable;
            },
        }
        x.Cell = std.ArrayList(*Lval).empty;
        for (v.Cell.items) |item| {
            const copied = item.Clone();
            x.Cell.append(allocator, copied) catch unreachable;
        }
        return x;
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
                val.lval_expr_print('(', ')');
            },
            .QEXPR => {
                val.lval_expr_print('{', '}');
            },
            .FUN => {
                if (val.BuiltinOrNull != null) {
                    std.debug.print("<function>", .{});
                } else {
                    std.debug.print("(\\ ", .{});
                    val.Formals.Print();
                    std.debug.print(" ", .{});
                    val.Body.Print();
                    std.debug.print(")", .{});
                }
            },
            .STR => {
                val.lval_print_str();
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
        assert(v.Type == .SEXPR or v.Type == .QEXPR);

        const allocator = std.heap.page_allocator;
        v.Cell.append(allocator, x) catch unreachable;
        return v;
    }

    fn Pop(v: *Lval, i: usize) *Lval {
        assert(v.Type == .SEXPR or v.Type == .QEXPR);

        const ret = v.Cell.orderedRemove(i);
        return ret;
    }

    fn Take(v: *Lval, i: usize) *Lval {
        assert(v.Type == .SEXPR or v.Type == .QEXPR);

        const x = v.Pop(i);
        v.Dispose();
        return x;
    }

    fn Join(x: *Lval, y: *Lval) *Lval {
        assert(x.Type == .SEXPR or x.Type == .QEXPR);

        for (y.Cell.items) |item| {
            _ = x.Add(item.Clone());
        }
        y.Dispose();
        return x;
    }

    pub fn IsSame(x: *Lval, y: *Lval) bool {
        if (x.Type != y.Type) {
            return false;
        }

        switch (x.Type) {
            .NUM => {
                return (x.Num == y.Num);
            },
            .ERR => {
                return std.mem.eql(u8, x.Err, y.Err);
            },
            .SYM => {
                return std.mem.eql(u8, x.Sym, y.Sym);
            },
            .FUN => {
                if (x.BuiltinOrNull != null or y.BuiltinOrNull != null) {
                    return x.BuiltinOrNull == y.BuiltinOrNull;
                }

                return IsSame(x.Formals, y.Formals) and IsSame(x.Body, y.Body);
            },
            .QEXPR, .SEXPR => {
                if (x.Cell.items.len != y.Cell.items.len) {
                    return false;
                }
                for (0..y.Cell.items.len) |i| {
                    if (!IsSame(x.Cell.items[i], y.Cell.items[i])) {
                        return false;
                    }
                }
                return true;
            },
            .STR => {
                return zstr_Equal(x.Str, y.Str);
            },
        }
        return false;
    }

    fn lval_print_str(v: *const Lval) void {
        std.debug.print("\"", .{});
        for (v.Str) |ch| {
            if (std.mem.indexOfScalar(u8, lval_str_escapable, ch) != null) {
                std.debug.print("{s}", .{lval_str_escape(ch)});
            } else {
                std.debug.print("{c}", .{ch});
            }
        }
        std.debug.print("\"", .{});
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

fn lval_err_format(comptime fmt: []const u8, args_fmt: anytype) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .ERR;
    v.Cell = std.ArrayList(*Lval).empty;
    const formatted = std.fmt.allocPrint(allocator, fmt, args_fmt) catch unreachable;
    v.Err = formatted;
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

fn lval_qexpr() *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .QEXPR;
    v.Cell = std.ArrayList(*Lval).empty;
    return v;
}

fn lval_fun(func: *const lbuiltin) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .FUN;
    v.Cell = std.ArrayList(*Lval).empty;
    v.BuiltinOrNull = func;
    return v;
}

fn lval_lambda(formals: *Lval, body: *Lval) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .FUN;
    v.Cell = std.ArrayList(*Lval).empty;
    v.BuiltinOrNull = null;
    v.Env = Lenv.Init();
    v.Formals = formals;
    v.Body = body;
    return v;
}

fn lval_str(s: []const u8) *Lval {
    const allocator = std.heap.page_allocator;
    const v = allocator.create(Lval) catch unreachable;
    v.Type = .STR;
    v.Cell = std.ArrayList(*Lval).empty;
    v.Str = allocator.dupe(u8, s) catch unreachable;
    return v;
}

// ===================================================
const lbuiltin = fn (env: *Lenv, val: *Lval) *Lval;

const Lenv = struct {
    ParentOrNull: ?*Lenv = null,

    Syms: std.ArrayList([]const u8) = undefined,
    Vals: std.ArrayList(*Lval) = undefined,

    pub fn Init() *Lenv {
        const allocator = std.heap.page_allocator;
        const ret = allocator.create(Lenv) catch unreachable;
        ret.Syms = std.ArrayList([]const u8).empty;
        ret.Vals = std.ArrayList(*Lval).empty;
        ret.ParentOrNull = null;
        return ret;
    }

    fn Clone(e: *Lenv) *Lenv {
        const allocator = std.heap.page_allocator;
        const ret = Lenv.Init();
        ret.ParentOrNull = e.ParentOrNull;

        for (e.Syms.items) |item| {
            const newSym = allocator.dupe(u8, item) catch unreachable;
            ret.Syms.append(allocator, newSym) catch unreachable;
        }

        for (e.Vals.items) |item| {
            const newVal = item.Clone();
            ret.Vals.append(allocator, newVal) catch unreachable;
        }

        return ret;
    }

    pub fn Dispose(e: *Lenv) void {
        const allocator = std.heap.page_allocator;

        for (e.Syms.items) |item| {
            allocator.free(item);
        }
        e.Syms.deinit(allocator);

        for (e.Vals.items) |item| {
            item.Dispose();
        }
        e.Vals.deinit(allocator);

        allocator.destroy(e);
    }

    fn Get(e: *Lenv, k: *Lval) *Lval {
        for (0.., e.Syms.items) |i, sym| {
            if (zstr_Equal(sym, k.Sym)) {
                const val = e.Vals.items[i];
                const copied = val.Clone();
                return copied;
            }
        }

        if (e.ParentOrNull) |parent| {
            return parent.Get(k);
        }

        return lval_err_format("unbound symbol '{s}'", .{k.Sym});
    }

    fn Put(e: *Lenv, k: *Lval, v: *Lval) void {
        const allocator = std.heap.page_allocator;

        const newVal = v.Clone();

        for (0.., e.Syms.items) |i, sym| {
            // override
            if (zstr_Equal(sym, k.Sym)) {
                const oldVal = e.Vals.items[i];
                oldVal.Dispose();
                e.Vals.items[i] = newVal;
                return;
            }
        }

        const newSym = allocator.dupe(u8, k.Sym) catch unreachable;
        e.Vals.append(allocator, newVal) catch unreachable;
        e.Syms.append(allocator, newSym) catch unreachable;
    }

    fn Def(e: *Lenv, k: *Lval, v: *Lval) void {
        var curr = e;
        while (curr.ParentOrNull) |parent| {
            curr = parent;
        }
        curr.Put(k, v);
    }

    fn Eval(e: *Lenv, v: *Lval) *Lval {
        if (v.Type == .SYM) {
            const x = e.Get(v);
            v.Dispose();
            return x;
        }

        if (v.Type == .SEXPR) {
            return e.EvalSexpr(v);
        }
        return v;
    }

    fn EvalSexpr(e: *Lenv, v: *Lval) *Lval {
        for (0.., v.Cell.items) |i, item| {
            const evaled = e.Eval(item);
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

        if (first.Type != .FUN) {
            v.Dispose();
            return lval_err_format("S-Expression starts with incorrect type. Got {s}, Expected {s}.", .{ first.Type.Name(), E_LVAL.FUN.Name() });
        }

        const result = e.Call(first, v);
        return result;
    }

    fn lenv_add_builtin(e: *Lenv, name: []const u8, func: lbuiltin) void {
        const k = lval_sym(name);
        const v = lval_fun(func);
        e.Put(k, v);
        k.Dispose();
        v.Dispose();
    }

    fn Call(e: *Lenv, f: *Lval, a: *Lval) *Lval {
        if (f.BuiltinOrNull) |builtin| {
            const called = builtin(e, a);
            return called;
        }

        const given = a.Cell.items.len;
        const total = f.Formals.Cell.items.len;
        while (a.Cell.items.len != 0) {
            if (f.Formals.Cell.items.len == 0) {
                a.Dispose();
                return lval_err_format("Function passed too many arguments. Got {d}, Expected {d}.", .{ given, total });
            }

            const sym = f.Formals.Pop(0);
            if (zstr_Equal(sym.Sym, "&")) {
                if (f.Formals.Cell.items.len != 1) {
                    a.Dispose();
                    return lval_err_format("Function format invalid. Symbol '&' not followed by single symbol.", .{});
                }

                const nsym = f.Formals.Pop(0);
                f.Env.Put(nsym, builtin_list(e, a));
                sym.Dispose();
                nsym.Dispose();
                break;
            }

            const val = a.Pop(0);
            f.Env.Put(sym, val);
            sym.Dispose();
            val.Dispose();
        }

        a.Dispose();

        if (f.Formals.Cell.items.len > 0 and zstr_Equal(f.Formals.Cell.items[0].Sym, "&")) {
            if (f.Formals.Cell.items.len != 2) {
                return lval_err_format("Function format invalid. Symbol '&' not followed by single symbol.", .{});
            }

            f.Formals.Pop(0).Dispose();

            const sym = f.Formals.Pop(0);
            const val = lval_qexpr();
            f.Env.Put(sym, val);
            sym.Dispose();
            val.Dispose();
        }

        if (f.Formals.Cell.items.len == 0) {
            f.Env.ParentOrNull = e;
            return builtin_eval(f.Env, lval_sexpr().Add(f.Body.Clone()));
        }

        return f.Clone();
    }
};

// ===================================================

fn builtin_list(_: *Lenv, a: *Lval) *Lval {
    a.Type = .QEXPR;
    return a;
}

fn builtin_head(_: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("head", a, 1)) |err| {
        return err;
    }
    if (LASSERT_TYPE("head", a, 0, .QEXPR)) |err| {
        return err;
    }
    if (LASSERT_NOT_EMPTY("head", a, 0)) |err| {
        return err;
    }

    const v = a.Take(0);
    while (v.Cell.items.len > 1) {
        v.Pop(1).Dispose();
    }
    return v;
}

fn builtin_tail(_: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("tail", a, 1)) |err| {
        return err;
    }
    if (LASSERT_TYPE("tail", a, 0, .QEXPR)) |err| {
        return err;
    }
    if (LASSERT_NOT_EMPTY("tail", a, 0)) |err| {
        return err;
    }

    const v = a.Take(0);
    v.Pop(0).Dispose();
    return v;
}

fn builtin_join(_: *Lenv, a: *Lval) *Lval {
    for (0..a.Cell.items.len) |i| {
        if (LASSERT_TYPE("join", a, i, .QEXPR)) |err| {
            return err;
        }
    }

    var x = a.Pop(0);
    while (a.Cell.items.len > 0) {
        x = x.Join(a.Pop(0));
    }
    a.Dispose();
    return x;
}

fn builtin_eval(e: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("eval", a, 1)) |err| {
        return err;
    }

    if (LASSERT_TYPE("eval", a, 0, .QEXPR)) |err| {
        return err;
    }

    const x = a.Take(0);
    x.Type = .SEXPR;
    return e.Eval(x);
}

fn builtin_op(_: *Lenv, a: *Lval, op: []const u8) *Lval {
    for (0..a.Cell.items.len) |i| {
        if (LASSERT_TYPE(op, a, i, .NUM)) |err| {
            return err;
        }
    }

    const x = a.Pop(0);
    if (zstr_Equal(op, "-") and a.Cell.items.len == 0) {
        x.Num = -x.Num;
    }

    while (a.Cell.items.len > 0) {
        const y = a.Pop(0);

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
        y.Dispose();
    }
    a.Dispose();
    return x;
}

fn builtin_lambda(_: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("\\", a, 2)) |err| {
        return err;
    }

    if (LASSERT_TYPE("\\", a, 0, .QEXPR)) |err| {
        return err;
    }

    if (LASSERT_TYPE("\\", a, 1, .QEXPR)) |err| {
        return err;
    }

    for (a.Cell.items[0].Cell.items) |item| {
        const t = item.Type;
        if (LASSERT(a, t == .SYM, "Cannot define non-symbol. Got {s}, Expected {s}.", .{ t.Name(), E_LVAL.SYM.Name() })) |err| {
            return err;
        }
    }

    const formals = a.Pop(0);
    const body = a.Pop(0);
    a.Dispose();

    return lval_lambda(formals, body);
}

fn builtin_var(e: *Lenv, a: *Lval, func: []const u8) *Lval {
    if (LASSERT_TYPE(func, a, 0, .QEXPR)) |err| {
        return err;
    }

    const syms = a.Cell.items[0];

    for (syms.Cell.items) |item| {
        if (LASSERT(a, item.Type == .SYM, "Function 'def' cannot define non-symbol. Got {s}, Expected {s}.", .{ item.Type.Name(), E_LVAL.SYM.Name() })) |err| {
            return err;
        }
    }

    if (LASSERT(a, syms.Cell.items.len == a.Cell.items.len - 1, "Function 'def' passed too many arguments for symbols. Got {d}, Expected {d}.", .{ syms.Cell.items.len, a.Cell.items.len - 1 })) |err| {
        return err;
    }

    for (0.., syms.Cell.items) |i, item| {
        if (zstr_Equal(func, "def")) {
            e.Def(item, a.Cell.items[i + 1]);
        }

        if (zstr_Equal(func, "=")) {
            e.Put(item, a.Cell.items[i + 1]);
        }
    }

    a.Dispose();
    return lval_sexpr();
}

fn builtin_def(e: *Lenv, a: *Lval) *Lval {
    return builtin_var(e, a, "def");
}

fn builtin_put(e: *Lenv, a: *Lval) *Lval {
    return builtin_var(e, a, "put");
}

fn builtin_add(env: *Lenv, a: *Lval) *Lval {
    return builtin_op(env, a, "+");
}

fn builtin_sub(env: *Lenv, a: *Lval) *Lval {
    return builtin_op(env, a, "-");
}

fn builtin_mul(env: *Lenv, a: *Lval) *Lval {
    return builtin_op(env, a, "*");
}

fn builtin_div(env: *Lenv, a: *Lval) *Lval {
    return builtin_op(env, a, "/");
}

fn builtin_if(e: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("if", a, 3)) |err| {
        return err;
    }
    if (LASSERT_TYPE("if", a, 0, .NUM)) |err| {
        return err;
    }
    if (LASSERT_TYPE("if", a, 1, .QEXPR)) |err| {
        return err;
    }
    if (LASSERT_TYPE("if", a, 2, .QEXPR)) |err| {
        return err;
    }

    a.Cell.items[1].Type = .SEXPR;
    a.Cell.items[2].Type = .SEXPR;

    var x: *Lval = undefined;
    if (a.Cell.items[0].Num != 0) {
        x = e.Eval(a.Pop(1));
    } else {
        x = e.Eval(a.Pop(2));
    }

    a.Dispose();
    return x;
}

fn builtin_eq(e: *Lenv, a: *Lval) *Lval {
    return builtin_cmp(e, a, "==");
}

fn builtin_ne(e: *Lenv, a: *Lval) *Lval {
    return builtin_cmp(e, a, "!=");
}

fn builtin_cmp(_: *Lenv, a: *Lval, op: []const u8) *Lval {
    if (LASSERT_NUM(op, a, 2)) |err| {
        return err;
    }

    var r: i64 = 0;
    if (zstr_Equal(op, "==")) {
        r = @intFromBool(a.Cell.items[0].IsSame(a.Cell.items[1]));
    } else if (zstr_Equal(op, "!=")) {
        r = @intFromBool(!a.Cell.items[0].IsSame(a.Cell.items[1]));
    }
    a.Dispose();
    return lval_num(r);
}

fn builtin_gt(e: *Lenv, a: *Lval) *Lval {
    return builtin_ord(e, a, ">");
}
fn builtin_lt(e: *Lenv, a: *Lval) *Lval {
    return builtin_ord(e, a, "<");
}
fn builtin_ge(e: *Lenv, a: *Lval) *Lval {
    return builtin_ord(e, a, ">=");
}
fn builtin_le(e: *Lenv, a: *Lval) *Lval {
    return builtin_ord(e, a, "<=");
}

fn builtin_ord(_: *Lenv, a: *Lval, op: []const u8) *Lval {
    if (LASSERT_NUM(op, a, 2)) |err| {
        return err;
    }
    if (LASSERT_TYPE(op, a, 0, .NUM)) |err| {
        return err;
    }
    if (LASSERT_TYPE(op, a, 1, .NUM)) |err| {
        return err;
    }

    var r: i64 = 0;
    if (zstr_Equal(op, ">")) {
        r = @intFromBool(a.Cell.items[0].Num > a.Cell.items[1].Num);
    } else if (zstr_Equal(op, "<")) {
        r = @intFromBool(a.Cell.items[0].Num < a.Cell.items[1].Num);
    } else if (zstr_Equal(op, ">=")) {
        r = @intFromBool(a.Cell.items[0].Num >= a.Cell.items[1].Num);
    } else if (zstr_Equal(op, "<=")) {
        r = @intFromBool(a.Cell.items[0].Num <= a.Cell.items[1].Num);
    }

    a.Dispose();
    return lval_num(r);
}

fn builtin_load(e: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("load", a, 1)) |err| {
        return err;
    }
    if (LASSERT_TYPE("load", a, 0, .STR)) |err| {
        return err;
    }

    const allocator = std.heap.page_allocator;
    const filename = a.Cell.items[0].Str;

    // 파일 열기
    var file = std.fs.cwd().openFile(filename, .{}) catch {
        const msg = std.fmt.allocPrint(allocator, "Could not load Library {s}", .{filename}) catch unreachable;
        a.Dispose();
        return lval_err(msg);
    };
    defer file.close();

    // 파일 내용 읽기
    const stat = file.stat() catch unreachable;
    const file_len = stat.size;

    var input_buf = allocator.alloc(u8, file_len + 1) catch unreachable;
    defer allocator.free(input_buf);

    const read_len = file.readAll(input_buf) catch unreachable;
    input_buf[read_len] = 0; // null-terminator

    // S-expression 읽기
    var pos: usize = 0;
    var expr = lval_read_expr(input_buf, &pos, 0);
    defer expr.Dispose();
    a.Dispose();

    if (expr.Type != .ERR) {
        while (expr.Cell.items.len > 0) {
            const x = e.Eval(expr.Pop(0));
            if (x.Type == .ERR) {
                x.Println();
            }
            x.Dispose();
        }
    } else {
        expr.Println();
    }

    return lval_sexpr();
}

fn builtin_error(_: *Lenv, a: *Lval) *Lval {
    if (LASSERT_NUM("error", a, 1)) |err| {
        return err;
    }
    if (LASSERT_TYPE("error", a, 0, .STR)) |err| {
        return err;
    }

    const ret = lval_err(a.Cell.items[0].Str);
    a.Dispose();
    return ret;
}

fn builtin_print(_: *Lenv, a: *Lval) *Lval {
    for (a.Cell.items) |item| {
        item.Print();
        std.debug.print(" ", .{});
    }
    std.debug.print("\n", .{});
    a.Dispose();
    return lval_sexpr();
}

fn lenv_add_builtins(e: *Lenv) void {
    //* Variable Functions
    e.lenv_add_builtin("\\", builtin_lambda);
    e.lenv_add_builtin("def", builtin_def);
    e.lenv_add_builtin("=", builtin_put);

    // List Functions
    e.lenv_add_builtin("list", builtin_list);
    e.lenv_add_builtin("head", builtin_head);
    e.lenv_add_builtin("tail", builtin_tail);
    e.lenv_add_builtin("eval", builtin_eval);
    e.lenv_add_builtin("join", builtin_join);

    // Mathematical Functions
    e.lenv_add_builtin("+", builtin_add);
    e.lenv_add_builtin("-", builtin_sub);
    e.lenv_add_builtin("*", builtin_mul);
    e.lenv_add_builtin("/", builtin_div);

    // Comparison Functions
    e.lenv_add_builtin("if", builtin_if);
    e.lenv_add_builtin("==", builtin_eq);
    e.lenv_add_builtin("!=", builtin_ne);
    e.lenv_add_builtin(">", builtin_gt);
    e.lenv_add_builtin("<", builtin_lt);
    e.lenv_add_builtin(">=", builtin_ge);
    e.lenv_add_builtin("<=", builtin_le);

    // String Functions
    e.lenv_add_builtin("load", builtin_load);
    e.lenv_add_builtin("error", builtin_error);
    e.lenv_add_builtin("print", builtin_print);
}

// ===================================================

fn lval_read_expr(s: []const u8, i: *usize, end: u8) *Lval {
    var x: *Lval = undefined;
    if (end == '}') {
        x = lval_qexpr();
    } else {
        x = lval_sexpr();
    }

    while (i.* < s.len) {
        if (s[i.*] == end) {
            break;
        }

        const y = lval_read(s, i);
        if (y.Type == .ERR) {
            x.Dispose();
            return y;
        }
        _ = x.Add(y);
    }

    i.* += 1;
    return x;
}

fn skipWhitespaceAndComments(s: []const u8, i: *usize) void {
    const whitespaceAndComments = " \t\x11\r\n;";

    while (i.* < s.len) {
        const yy = std.mem.indexOfScalar(u8, whitespaceAndComments, s[i.*]);
        if (yy == null) {
            return;
        }

        if (s[i.*] == ';') {
            while (i.* < s.len and s[i.*] != '\n') {
                i.* += 1;
            }
        }
        i.* += 1;
    }
}

fn lval_read(s: []const u8, i: *usize) *Lval {
    skipWhitespaceAndComments(s, i);

    if (i.* >= s.len) {
        return lval_err("Unexpected end of input");
    }

    const ch = s[i.*];
    var x: *Lval = undefined;

    if (ch == '(') {
        i.* += 1;
        x = lval_read_expr(s, i, ')');
    } else if (ch == '{') {
        i.* += 1;
        x = lval_read_expr(s, i, '}');
    } else if (std.ascii.isAlphanumeric(ch) or std.mem.indexOfScalar(u8, "_+-*\\/=<>!&", ch) != null) {
        x = lval_read_sym(s, i);
    } else if (ch == '"') {
        x = lval_read_str(s, i);
    } else {
        return lval_err_format("Unexpected character: '{c}'", .{ch});
    }

    skipWhitespaceAndComments(s, i);
    return x;
}

fn lval_read_sym(s: []const u8, i_ptr: *usize) *Lval {
    const allocator = std.heap.page_allocator;
    const valid_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*\\/=<>!&";

    var part = std.ArrayList(u8).empty;
    defer part.deinit(allocator);

    const len = s.len;
    while (i_ptr.* < len and std.mem.indexOfScalar(u8, valid_chars, s[i_ptr.*]) != null) {
        part.append(allocator, s[i_ptr.*]) catch unreachable;
        i_ptr.* += 1;
    }

    const part_str = part.toOwnedSlice(allocator) catch unreachable; // `[]u8` (not null-terminated)
    defer allocator.free(part_str);

    var is_num = false;
    if (part_str.len > 0) {
        const first = part_str[0];
        is_num = std.mem.indexOfScalar(u8, "-0123456789", first) != null;

        if (is_num) {
            for (1..part_str.len) |j| {
                if (std.mem.indexOfScalar(u8, "0123456789", part_str[j]) == null) {
                    is_num = false;
                    break;
                }
            }
            if (part_str.len == 1 and first == '-') {
                is_num = false;
            }
        }
    }

    if (is_num) {
        const str_val = std.fmt.parseInt(i64, part_str, 10) catch {
            return lval_err("Invalid Number");
        };
        return lval_num(str_val);
    }
    return lval_sym(part_str); // Assume lval_sym duplicates the input string
}

fn lval_read_str(s: []const u8, i: *usize) *Lval {
    const allocator = std.heap.page_allocator;
    var part = std.ArrayList(u8).empty;
    defer part.deinit(allocator);

    i.* += 1;

    while (s[i.*] != '"') {
        if (i.* >= s.len) {
            return lval_err("Unexpected end of input");
        }

        var c = s[i.*];

        if (c == '\\') {
            i.* += 1;
            if (i.* >= s.len) {
                return lval_err("Unexpected end of input after \\");
            }
            const esc = s[i.*];
            if (std.mem.indexOfScalar(u8, lval_str_unescapable, esc) == null) {
                return lval_err_format("Invalid escape sequence \\{c}", .{esc});
            }

            c = lval_str_unescape(esc);
        }

        part.append(allocator, c) catch unreachable;
        i.* += 1;
    }

    i.* += 1;

    const ret = lval_str(part.items);
    return ret;
}

const lval_str_escapable = "\x07\x08\x12\n\r\t\x11\\\'\"";
const lval_str_unescapable = "abfnrtv\\\'\"";

fn lval_str_escape(x: u8) []const u8 {
    switch (x) {
        0x07 => return "\\a", // '\a' Bell
        0x08 => return "\\b", // '\b' Backspace
        0x12 => return "\\f", // '\f' Form Feed
        '\n' => return "\\n",
        '\r' => return "\\r",
        '\t' => return "\\t",
        0x11 => return "\\v", // '\v' Vertical Tab
        '\\' => return "\\\\",
        '\'' => return "\\\'",
        '\"' => return "\\\"",
        else => return "",
    }
}

fn lval_str_unescape(x: u8) u8 {
    switch (x) {
        'a' => return 0x07, // '\a' Bell
        'b' => return 0x08, // '\b' Backspace
        'f' => return 0x12, // '\f' Form Feed
        'n' => return '\n',
        'r' => return '\r',
        't' => return '\t',
        'v' => return 0x11, // '\v' Vertical Tab
        '\\' => return '\\',
        '\'' => return '\'',
        '\"' => return '\"',
        else => return 0x00,
    }
}
// ===================================================

pub fn main() void {
    const allocator = std.heap.page_allocator;

    const args = std.process.argsAlloc(allocator) catch unreachable;
    defer std.process.argsFree(allocator, args);

    const env = Lenv.Init();
    lenv_add_builtins(env);

    const argc = args.len;
    if (argc == 1) {
        std.debug.print("Lispy Version 0.0.0.0.2\n", .{});
        std.debug.print("Press Ctrl+c to Exit\n\n", .{});

        while (true) {
            const input: [*c]u8 = readLine("lispy> ");
            defer std.c.free(input);

            var pos: usize = 0;
            const r: *Lval = lval_read_expr(std.mem.span(input), &pos, 0x00);
            const e: *Lval = env.Eval(r);
            e.Println();
            e.Dispose();
        }
    }
    if (argc >= 2) {
        for (args[1..]) |arg| {
            const lval_args = lval_sexpr().Add(lval_str(arg));

            const x = builtin_load(env, lval_args);
            if (x.Type == .ERR) {
                x.Println();
            }
            x.Dispose();
        }
    }
}
