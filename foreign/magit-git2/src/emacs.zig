//! Thin helpers over `emacs_env`: interning, strings, lists, signals.
//!
//! Every helper either returns a value or leaves a non-local exit pending
//! in Emacs and returns `error.Signaled`; module functions propagate that
//! error up to the trampoline, which then returns to Emacs at once.

const std = @import("std");
const c = @import("emacs_c");

pub const Value = c.emacs_value;

pub const Error = error{ Signaled, OutOfMemory };

const allocator = std.heap.c_allocator;

pub const Env = struct {
    raw: *c.emacs_env,

    fn pending(env: Env) bool {
        return env.raw.non_local_exit_check.?(env.raw) != c.emacs_funcall_exit_return;
    }

    fn check(env: Env, v: Value) Error!Value {
        if (env.pending()) return error.Signaled;
        return v;
    }

    pub fn intern(env: Env, name: [*:0]const u8) Error!Value {
        return env.check(env.raw.intern.?(env.raw, name));
    }

    pub fn nil(env: Env) Error!Value {
        return env.intern("nil");
    }

    pub fn t(env: Env) Error!Value {
        return env.intern("t");
    }

    pub fn boolean(env: Env, b: bool) Error!Value {
        return if (b) env.t() else env.nil();
    }

    pub fn isNil(env: Env, v: Value) bool {
        return !env.raw.is_not_nil.?(env.raw, v);
    }

    pub fn int(env: Env, n: i64) Error!Value {
        return env.check(env.raw.make_integer.?(env.raw, n));
    }

    pub fn getInt(env: Env, v: Value) Error!i64 {
        const n = env.raw.extract_integer.?(env.raw, v);
        if (env.pending()) return error.Signaled;
        return n;
    }

    /// A multibyte Lisp string from UTF-8 bytes.
    pub fn string(env: Env, s: []const u8) Error!Value {
        return env.check(env.raw.make_string.?(env.raw, s.ptr, @intCast(s.len)));
    }

    /// Like `string', for a C string that may be NULL (giving nil).
    pub fn cstring(env: Env, s: ?[*:0]const u8) Error!Value {
        const p = s orelse return env.nil();
        return env.string(std.mem.span(p));
    }

    /// Copy the Lisp string V as NUL-terminated UTF-8.  Free with `free'.
    pub fn getString(env: Env, v: Value) Error![:0]u8 {
        var len: c.ptrdiff_t = 0;
        if (!env.raw.copy_string_contents.?(env.raw, v, null, &len))
            return error.Signaled;
        // LEN counts the terminating NUL.
        const buf = try allocator.allocSentinel(u8, @intCast(len - 1), 0);
        errdefer allocator.free(buf);
        if (!env.raw.copy_string_contents.?(env.raw, v, buf.ptr, &len))
            return error.Signaled;
        return buf;
    }

    /// Like `getString', but nil gives null.
    pub fn getOptString(env: Env, v: Value) Error!?[:0]u8 {
        if (env.isNil(v)) return null;
        return try env.getString(v);
    }

    pub fn free(_: Env, s: [:0]u8) void {
        allocator.free(s);
    }

    pub fn funcall(env: Env, f: Value, args: []const Value) Error!Value {
        return env.check(env.raw.funcall.?(
            env.raw,
            f,
            @intCast(args.len),
            @constCast(args.ptr),
        ));
    }

    pub fn call(env: Env, name: [*:0]const u8, args: []const Value) Error!Value {
        return env.funcall(try env.intern(name), args);
    }

    pub fn list(env: Env, items: []const Value) Error!Value {
        return env.call("list", items);
    }

    pub fn cons(env: Env, a: Value, b: Value) Error!Value {
        return env.call("cons", &.{ a, b });
    }

    /// The elements of the Lisp list V, in a buffer to free with `freeValues'.
    pub fn listItems(env: Env, v: Value) Error![]Value {
        const vec = try env.call("vconcat", &.{v});
        const n = env.raw.vec_size.?(env.raw, vec);
        if (env.pending()) return error.Signaled;
        const items = try allocator.alloc(Value, @intCast(n));
        errdefer allocator.free(items);
        for (items, 0..) |*item, i| {
            item.* = env.raw.vec_get.?(env.raw, vec, @intCast(i));
            if (env.pending()) return error.Signaled;
        }
        return items;
    }

    pub fn freeValues(_: Env, items: []Value) void {
        allocator.free(items);
    }

    /// Leave `(signal SYMBOL DATA)' pending.
    pub fn signal(env: Env, symbol: [*:0]const u8, data: Value) Error {
        const sym = env.intern(symbol) catch return error.Signaled;
        env.raw.non_local_exit_signal.?(env.raw, sym, data);
        return error.Signaled;
    }

    /// Signal `error' with MESSAGE.
    pub fn fail(env: Env, message: []const u8) Error {
        const msg = env.string(message) catch return error.Signaled;
        const data = env.list(&.{msg}) catch return error.Signaled;
        return env.signal("error", data);
    }

    /// `defalias' NAME to a module function F of exactly ARITY arguments.
    pub fn defun(
        env: Env,
        name: [*:0]const u8,
        arity: usize,
        f: c.emacs_function,
        doc: [*:0]const u8,
    ) Error!void {
        const func = try env.check(env.raw.make_function.?(
            env.raw,
            @intCast(arity),
            @intCast(arity),
            f,
            doc,
            null,
        ));
        _ = try env.call("defalias", &.{ try env.intern(name), func });
    }
};

/// Collects Lisp values to turn into a list in one `list' call.
pub const ListBuilder = struct {
    items: std.ArrayList(Value) = .empty,

    pub fn append(self: *ListBuilder, v: Value) Error!void {
        try self.items.append(allocator, v);
    }

    pub fn finish(self: *ListBuilder, env: Env) Error!Value {
        defer self.deinit();
        return env.list(self.items.items);
    }

    pub fn deinit(self: *ListBuilder) void {
        self.items.deinit(allocator);
    }
};

/// Wrap F, a Zig function over an Env and the argument slice, as an Emacs
/// module function.  Errors become Lisp signals; nothing unwinds into
/// Emacs.
pub fn trampoline(
    comptime f: fn (Env, []const Value) Error!Value,
) c.emacs_function {
    return struct {
        fn call(
            raw: [*c]c.emacs_env,
            nargs: c.ptrdiff_t,
            args: [*c]c.emacs_value,
            _: ?*anyopaque,
        ) callconv(.c) Value {
            const env = Env{ .raw = raw };
            const argv: []const Value = if (nargs > 0) args[0..@intCast(nargs)] else &.{};
            return f(env, argv) catch |err| switch (err) {
                error.Signaled => null,
                error.OutOfMemory => blk: {
                    _ = env.fail("magit-git2: out of memory") catch {};
                    break :blk null;
                },
            };
        }
    }.call;
}

test "ListBuilder grows and frees" {
    var b: ListBuilder = .{};
    defer b.deinit();
    for (0..100) |_| try b.append(null);
    try std.testing.expectEqual(@as(usize, 100), b.items.items.len);
}
