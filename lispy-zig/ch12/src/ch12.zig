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

    fn Name(t: E_LVAL) []const u8 {
        switch (t) {
            .FUN => return "Function",
            .NUM => return "Number",
            .ERR => return "Error",
            .SYM => return "Symbol",
            .SEXPR => return "S-Expression",
            .QEXPR => return "Q-Expression",
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
    } else if (cstr_Contains(t.tag, "qexpr")) {
        x = lval_qexpr();
    }

    for (0..@intCast(t.children_num), t.children) |_, child| {
        if (cstr_Equal(child.*.contents, "(")) {
            continue;
        }
        if (cstr_Equal(child.*.contents, ")")) {
            continue;
        }
        if (cstr_Equal(child.*.contents, "{")) {
            continue;
        }
        if (cstr_Equal(child.*.contents, "}")) {
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

fn lenv_add_builtins(e: *Lenv) void {
    e.lenv_add_builtin("\\", builtin_lambda);
    e.lenv_add_builtin("def", builtin_def);
    e.lenv_add_builtin("=", builtin_put);

    e.lenv_add_builtin("list", builtin_list);
    e.lenv_add_builtin("head", builtin_head);
    e.lenv_add_builtin("tail", builtin_tail);
    e.lenv_add_builtin("eval", builtin_eval);
    e.lenv_add_builtin("join", builtin_join);

    e.lenv_add_builtin("+", builtin_add);
    e.lenv_add_builtin("-", builtin_sub);
    e.lenv_add_builtin("*", builtin_mul);
    e.lenv_add_builtin("/", builtin_div);
}

// ===================================================

pub fn main() void {
    const Number = c_mpc.mpc_new("number");
    const Symbol = c_mpc.mpc_new("symbol");
    const Sexpr = c_mpc.mpc_new("sexpr");
    const Qexpr = c_mpc.mpc_new("qexpr");
    const Expr = c_mpc.mpc_new("expr");
    const Lispy = c_mpc.mpc_new("lispy");
    defer c_mpc.mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

    const lang =
        \\ number : /-?[0-9]+/ ;
        \\ symbol : /[a-zA-Z0-9_+\-*\/\\=<>!&]+/ ;
        \\ sexpr  : '(' <expr>* ')' ;
        \\ qexpr  : '{' <expr>* '}' ;
        \\ expr   : <number> | <symbol> | <sexpr> | <qexpr> ;
        \\ lispy  : /^/ <expr>* /$/ ;
    ;

    if (c_mpc.mpca_lang(c_mpc.MPCA_LANG_DEFAULT, lang, Number, Symbol, Sexpr, Qexpr, Expr, Lispy) != 0) {
        std.debug.print("Failed to define grammar\n", .{});
        return;
    }

    const env = Lenv.Init();
    lenv_add_builtins(env);

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
            const e: *Lval = env.Eval(r);
            e.Println();
            e.Dispose();
        } else {
            c_mpc.mpc_err_print(result.@"error");
            c_mpc.mpc_err_delete(result.@"error");
        }
    }
}
