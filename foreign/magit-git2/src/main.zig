// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Kostafey <kostafey@gmail.com>

//! magit-git2-core: libgit2 queries for magit-git2.el.
//!
//! Every function takes the directory Magit runs git in as its first
//! argument and returns data -- strings, integers, lists, plists -- never
//! git-formatted text: lisp/magit-git2.el does the formatting.  "Not
//! found" results come back as nil; other libgit2 failures signal
//! `magit-git2-error' with (CODE KLASS MESSAGE), as do strings that are
//! not valid UTF-8.

const std = @import("std");
const ec = @import("emacs_c");
const emacs = @import("emacs.zig");
const git = @import("git.zig");
const walk = @import("walk.zig");
const c = git.c;

const Env = emacs.Env;
const Value = emacs.Value;
const Error = emacs.Error;
const ListBuilder = emacs.ListBuilder;

const allocator = std.heap.c_allocator;

export const plugin_is_GPL_compatible: c_int = 0;

// Errors

fn signalGit(env: Env) Error {
    const f = &git.last;
    const data = env.list(&.{
        env.int(f.code) catch return error.Signaled,
        env.int(f.klass) catch return error.Signaled,
        env.string(f.text()) catch return error.Signaled,
    }) catch return error.Signaled;
    return env.signal("magit-git2-error", data);
}

/// Check the libgit2 return code RC; signal on failure.
fn check(env: Env, rc: c_int) Error!c_int {
    return git.check(rc) catch signalGit(env);
}

/// Like `check', but "not found" (and ALSO) give false.
fn found(env: Env, rc: c_int, also: []const c_int) Error!bool {
    return git.found(rc, also) catch signalGit(env);
}

/// A Lisp string from S, which must be valid UTF-8.
fn text(env: Env, s: []const u8) Error!Value {
    if (!std.unicode.utf8ValidateSlice(s)) {
        git.last = .{ .code = -1000 };
        const msg = "string is not valid UTF-8";
        @memcpy(git.last.message[0..msg.len], msg);
        git.last.message_len = msg.len;
        return signalGit(env);
    }
    return env.string(s);
}

fn ctext(env: Env, s: ?[*:0]const u8) Error!Value {
    const p = s orelse return env.nil();
    return text(env, std.mem.span(p));
}

fn hexValue(env: Env, oid: git.Oid) Error!Value {
    const h = git.hex(oid);
    return env.string(&h);
}

fn oidArg(env: Env, v: Value) Error!git.Oid {
    const s = try env.getString(v);
    defer env.free(s);
    return git.parseHex(s) orelse env.fail("magit-git2: not a full object id");
}

fn repoArg(env: Env, v: Value) Error!?*c.git_repository {
    const dir = try env.getString(v);
    defer env.free(dir);
    return git.open(dir) catch |err| switch (err) {
        error.Git => signalGit(env),
        error.OutOfMemory => error.OutOfMemory,
    };
}

fn kw(env: Env, b: *ListBuilder, key: [*:0]const u8, value: Value) Error!void {
    try b.append(try env.intern(key));
    try b.append(value);
}

fn objectType(env: Env, t: c.git_object_t) Error!Value {
    return env.intern(switch (t) {
        c.GIT_OBJECT_COMMIT => "commit",
        c.GIT_OBJECT_TREE => "tree",
        c.GIT_OBJECT_BLOB => "blob",
        c.GIT_OBJECT_TAG => "tag",
        else => "unknown",
    });
}

// Module functions

fn version(env: Env, _: []const Value) Error!Value {
    var major: c_int = 0;
    var minor: c_int = 0;
    var rev: c_int = 0;
    _ = try check(env, c.git_libgit2_version(&major, &minor, &rev));
    var buf: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}.{d}.{d}", .{ major, minor, rev }) catch unreachable;
    return env.string(s);
}

fn release(env: Env, _: []const Value) Error!Value {
    git.releaseAll();
    return env.nil();
}

/// The config levels by name; libgit2 before 1.8 has no worktree level.
const config_levels = [_]struct { [*:0]const u8, c_int }{
    .{ "programdata", c.GIT_CONFIG_LEVEL_PROGRAMDATA },
    .{ "system", c.GIT_CONFIG_LEVEL_SYSTEM },
    .{ "xdg", c.GIT_CONFIG_LEVEL_XDG },
    .{ "global", c.GIT_CONFIG_LEVEL_GLOBAL },
    .{ "local", c.GIT_CONFIG_LEVEL_LOCAL },
} ++ (if (@hasDecl(c, "GIT_CONFIG_LEVEL_WORKTREE"))
    [_]struct { [*:0]const u8, c_int }{.{ "worktree", c.GIT_CONFIG_LEVEL_WORKTREE }}
else
    [_]struct { [*:0]const u8, c_int }{}) ++ [_]struct { [*:0]const u8, c_int }{
    .{ "app", c.GIT_CONFIG_LEVEL_APP },
};

fn configLevel(env: Env, v: Value) Error!c_int {
    for (config_levels) |l| {
        if (env.raw.eq.?(env.raw, v, try env.intern(l[0]))) return l[1];
    }
    return env.fail("magit-git2: unknown config level");
}

fn levelSymbol(env: Env, level: c_int) Error!Value {
    for (config_levels) |l| {
        if (l[1] == level) return env.intern(l[0]);
    }
    return env.intern("unknown");
}

/// (magit-git2--set-search-path LEVEL PATH)
fn setSearchPath(env: Env, args: []const Value) Error!Value {
    const level = try configLevel(env, args[0]);
    const path = try env.getOptString(args[1]);
    defer if (path) |p| env.free(p);
    const ptr: [*c]const u8 = if (path) |p| p.ptr else null;
    _ = try check(env, c.git_libgit2_opts(c.GIT_OPT_SET_SEARCH_PATH, @as(c_int, level), ptr));
    // Repositories opened before keep the config they read.
    git.releaseAll();
    return env.nil();
}

/// (magit-git2--set-system-config-path DIR)
fn setSystemConfigPath(env: Env, args: []const Value) Error!Value {
    return setSearchPath(env, &.{ try env.intern("system"), args[0] });
}

/// (magit-git2--repo-info DIR)
fn repoInfo(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    var b: ListBuilder = .{};
    errdefer b.deinit();
    try kw(env, &b, ":gitdir", try ctext(env, c.git_repository_path(repo)));
    try kw(env, &b, ":commondir", try ctext(env, c.git_repository_commondir(repo)));
    try kw(env, &b, ":workdir", try ctext(env, c.git_repository_workdir(repo)));
    try kw(env, &b, ":bare", try env.boolean(c.git_repository_is_bare(repo) == 1));
    try kw(env, &b, ":worktree", try env.boolean(c.git_repository_is_worktree(repo) == 1));
    try kw(env, &b, ":shallow", try env.boolean(c.git_repository_is_shallow(repo) == 1));
    return b.finish(env);
}

/// (magit-git2--revparse DIR SPEC): nil or (HASH TYPE REFNAME).
fn revparse(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const spec = try env.getString(args[1]);
    defer env.free(spec);
    var obj: ?*c.git_object = null;
    var ref: ?*c.git_reference = null;
    if (!try found(env, c.git_revparse_ext(&obj, &ref, repo, spec.ptr), &.{ c.GIT_EINVALIDSPEC, c.GIT_EUNBORNBRANCH }))
        return env.nil();
    defer c.git_object_free(obj);
    defer c.git_reference_free(ref);
    var refname = try env.nil();
    if (ref != null) {
        var resolved: ?*c.git_reference = null;
        if (try found(env, c.git_reference_resolve(&resolved, ref), &.{})) {
            defer c.git_reference_free(resolved);
            refname = try ctext(env, c.git_reference_name(resolved));
        }
    }
    return env.list(&.{
        try hexValue(env, git.oidOf(c.git_object_id(obj))),
        try objectType(env, c.git_object_type(obj)),
        refname,
    });
}

fn uniqueLength(env: Env, odb: ?*c.git_odb, oid: git.Oid, min: usize) Error!usize {
    var len = @max(min, 4);
    const id = git.gitOid(oid);
    while (len < 40) : (len += 1) {
        var out: c.git_oid = .{};
        const rc = c.git_odb_exists_prefix(&out, odb, &id, len);
        if (rc == c.GIT_EAMBIGUOUS) continue;
        _ = try check(env, rc);
        return len;
    }
    return 40;
}

/// (magit-git2--short-id DIR HASH MINLEN): the shortest unique prefix of
/// HASH that is at least MINLEN long.
fn shortId(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const oid = try oidArg(env, args[1]);
    const min: usize = @intCast(@max(4, @min(40, try env.getInt(args[2]))));
    var odb: ?*c.git_odb = null;
    _ = try check(env, c.git_repository_odb(&odb, repo));
    defer c.git_odb_free(odb);
    const len = try uniqueLength(env, odb, oid, min);
    const h = git.hex(oid);
    return env.string(h[0..len]);
}

/// (magit-git2--ref DIR NAME): nil, (symbolic TARGET) or (direct HASH).
fn refLookup(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const name = try env.getString(args[1]);
    defer env.free(name);
    var ref: ?*c.git_reference = null;
    if (!try found(env, c.git_reference_lookup(&ref, repo, name.ptr), &.{c.GIT_EINVALIDSPEC}))
        return env.nil();
    defer c.git_reference_free(ref);
    if (c.git_reference_type(ref) == c.GIT_REFERENCE_SYMBOLIC) {
        return env.list(&.{
            try env.intern("symbolic"),
            try ctext(env, c.git_reference_symbolic_target(ref)),
        });
    }
    return env.list(&.{
        try env.intern("direct"),
        try hexValue(env, git.oidOf(c.git_reference_target(ref))),
    });
}

/// (magit-git2--refs DIR): ((NAME HASH PEELED SYMREF) ...) for every ref
/// but HEAD, unsorted.  HASH is what NAME resolves to, PEELED the object
/// a tag object HASH finally points to (else nil), SYMREF the target of a
/// symbolic NAME (else nil).  Dangling symbolic refs are left out.
fn refs(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    var iter: ?*c.git_reference_iterator = null;
    _ = try check(env, c.git_reference_iterator_new(&iter, repo));
    defer c.git_reference_iterator_free(iter);
    var odb: ?*c.git_odb = null;
    _ = try check(env, c.git_repository_odb(&odb, repo));
    defer c.git_odb_free(odb);
    var out: ListBuilder = .{};
    errdefer out.deinit();
    while (true) {
        var ref: ?*c.git_reference = null;
        const rc = c.git_reference_next(&ref, iter);
        if (rc == c.GIT_ITEROVER) break;
        _ = try check(env, rc);
        defer c.git_reference_free(ref);
        var symref = try env.nil();
        var resolved: ?*c.git_reference = null;
        if (c.git_reference_type(ref) == c.GIT_REFERENCE_SYMBOLIC) {
            symref = try ctext(env, c.git_reference_symbolic_target(ref));
            if (!try found(env, c.git_reference_resolve(&resolved, ref), &.{})) continue;
        }
        defer if (resolved != null) c.git_reference_free(resolved);
        const target = c.git_reference_target(if (resolved != null) resolved else ref);
        var size: usize = 0;
        var otype: c.git_object_t = 0;
        // A ref to a missing object is left out, as git leaves it out.
        if (!try found(env, c.git_odb_read_header(&size, &otype, odb, target), &.{})) continue;
        var peeled = try env.nil();
        if (otype == c.GIT_OBJECT_TAG) {
            var obj: ?*c.git_object = null;
            var p: ?*c.git_object = null;
            _ = try check(env, c.git_object_lookup(&obj, repo, target, c.GIT_OBJECT_TAG));
            defer c.git_object_free(obj);
            if (try found(env, c.git_object_peel(&p, obj, c.GIT_OBJECT_ANY), &.{})) {
                defer c.git_object_free(p);
                peeled = try hexValue(env, git.oidOf(c.git_object_id(p)));
            }
        }
        try out.append(try env.list(&.{
            try ctext(env, c.git_reference_name(ref)),
            try hexValue(env, git.oidOf(target)),
            peeled,
            symref,
        }));
    }
    return out.finish(env);
}

/// (magit-git2--head DIR): plist (:symref NAME :hash HASH).  Detached:
/// :symref nil.  Unborn: :hash nil.
fn head(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    var ref: ?*c.git_reference = null;
    if (!try found(env, c.git_reference_lookup(&ref, repo, "HEAD"), &.{}))
        return env.nil();
    defer c.git_reference_free(ref);
    var symref = try env.nil();
    var hash = try env.nil();
    if (c.git_reference_type(ref) == c.GIT_REFERENCE_SYMBOLIC) {
        symref = try ctext(env, c.git_reference_symbolic_target(ref));
        var resolved: ?*c.git_reference = null;
        if (try found(env, c.git_reference_resolve(&resolved, ref), &.{})) {
            defer c.git_reference_free(resolved);
            hash = try hexValue(env, git.oidOf(c.git_reference_target(resolved)));
        }
    } else {
        hash = try hexValue(env, git.oidOf(c.git_reference_target(ref)));
    }
    return env.list(&.{ try env.intern(":symref"), symref, try env.intern(":hash"), hash });
}

/// (magit-git2--config DIR LEVEL): ((NAME VALUE LEVEL) ...) in libgit2's
/// iteration order; LEVEL nil for all levels.  VALUE is nil for a
/// variable without "=".
fn config(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    var snapshot: ?*c.git_config = null;
    _ = try check(env, c.git_repository_config_snapshot(&snapshot, repo));
    defer c.git_config_free(snapshot);
    var cfg = snapshot;
    var level_cfg: ?*c.git_config = null;
    defer if (level_cfg != null) c.git_config_free(level_cfg);
    if (!env.isNil(args[1])) {
        const level = try configLevel(env, args[1]);
        if (!try found(env, c.git_config_open_level(&level_cfg, snapshot, level), &.{}))
            return env.nil();
        cfg = level_cfg;
    }
    var iter: ?*c.git_config_iterator = null;
    _ = try check(env, c.git_config_iterator_new(&iter, cfg));
    defer c.git_config_iterator_free(iter);
    var out: ListBuilder = .{};
    errdefer out.deinit();
    while (true) {
        var entry: [*c]c.git_config_entry = null;
        const rc = c.git_config_next(&entry, iter);
        if (rc == c.GIT_ITEROVER) break;
        _ = try check(env, rc);
        try out.append(try env.list(&.{
            try ctext(env, entry.*.name),
            try ctext(env, entry.*.value),
            try levelSymbol(env, entry.*.level),
        }));
    }
    return out.finish(env);
}

/// (magit-git2--upstream DIR BRANCH): the full name of the upstream of
/// the local branch BRANCH (a full ref name), or nil.
fn upstream(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const branch = try env.getString(args[1]);
    defer env.free(branch);
    var buf: c.git_buf = .{};
    defer c.git_buf_dispose(&buf);
    if (!try found(env, c.git_branch_upstream_name(&buf, repo, branch.ptr), &.{c.GIT_EINVALIDSPEC}))
        return env.nil();
    return text(env, buf.ptr[0..buf.size]);
}

/// (magit-git2--ahead-behind DIR LOCAL UPSTREAM): (AHEAD . BEHIND).
fn aheadBehind(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const a = git.gitOid(try oidArg(env, args[1]));
    const b = git.gitOid(try oidArg(env, args[2]));
    var ahead: usize = 0;
    var behind: usize = 0;
    _ = try check(env, c.git_graph_ahead_behind(&ahead, &behind, repo, &a, &b));
    return env.cons(try env.int(@intCast(ahead)), try env.int(@intCast(behind)));
}

/// (magit-git2--merge-base DIR A B): hash or nil.
fn mergeBase(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const a = git.gitOid(try oidArg(env, args[1]));
    const b = git.gitOid(try oidArg(env, args[2]));
    var out: c.git_oid = .{};
    if (!try found(env, c.git_merge_base(&out, repo, &a, &b), &.{})) return env.nil();
    return hexValue(env, git.oidOf(&out));
}

/// (magit-git2--descendant-p DIR COMMIT ANCESTOR): whether COMMIT is a
/// descendant of ANCESTOR, not counting COMMIT itself.
fn descendantP(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const a = git.gitOid(try oidArg(env, args[1]));
    const b = git.gitOid(try oidArg(env, args[2]));
    return env.boolean(try check(env, c.git_graph_descendant_of(repo, &a, &b)) == 1);
}

/// (magit-git2--reachable-from-any DIR COMMIT HASHES): whether COMMIT is
/// one of HASHES or an ancestor of one.
fn reachableFromAny(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const commit = git.gitOid(try oidArg(env, args[1]));
    const items = try env.listItems(args[2]);
    defer env.freeValues(items);
    if (items.len == 0) return env.nil();
    const ids = try allocator.alloc(c.git_oid, items.len);
    defer allocator.free(ids);
    for (items, ids) |item, *id| id.* = git.gitOid(try oidArg(env, item));
    return env.boolean(try check(env, c.git_graph_reachable_from_any(repo, &commit, ids.ptr, ids.len)) == 1);
}

/// (magit-git2--tips-reaching DIR COMMIT TIPS CUTOFF): the TIPS from
/// which COMMIT is reachable through commits dated CUTOFF or later --
/// the date cutoff of `git name-rev'.
fn tipsReaching(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const target = try oidArg(env, args[1]);
    const items = try env.listItems(args[2]);
    defer env.freeValues(items);
    const cutoff = try env.getInt(args[3]);
    // Commits a finished, unsuccessful walk went through: they do not
    // lead to COMMIT.
    var dead: std.AutoHashMapUnmanaged(git.Oid, void) = .empty;
    defer dead.deinit(allocator);
    var out: ListBuilder = .{};
    errdefer out.deinit();
    for (items) |item| {
        const tip = try oidArg(env, item);
        var seen: std.AutoHashMapUnmanaged(git.Oid, void) = .empty;
        defer seen.deinit(allocator);
        var stack: std.ArrayList(git.Oid) = .empty;
        defer stack.deinit(allocator);
        try stack.append(allocator, tip);
        var reached = false;
        while (stack.pop()) |oid| {
            if (std.mem.eql(u8, &oid, &target)) {
                reached = true;
                break;
            }
            if (dead.contains(oid)) continue;
            const gop = try seen.getOrPut(allocator, oid);
            if (gop.found_existing) continue;
            var commit: ?*c.git_commit = null;
            const id = git.gitOid(oid);
            if (!try found(env, c.git_commit_lookup(&commit, repo, &id), &.{})) continue;
            defer c.git_commit_free(commit);
            if (c.git_commit_time(commit) < cutoff) continue;
            const n = c.git_commit_parentcount(commit);
            var i: c_uint = 0;
            while (i < n) : (i += 1) {
                try stack.append(allocator, git.oidOf(c.git_commit_parent_id(commit, i)));
            }
        }
        if (reached) {
            try out.append(item);
        } else {
            var it = seen.keyIterator();
            while (it.next()) |k| try dead.put(allocator, k.*, {});
        }
    }
    return out.finish(env);
}

/// (magit-git2--describe DIR HASH ABBREV): "TAG-N-gHASH" from all tags,
/// the hash abbreviated to at least ABBREV digits, or nil when no tag
/// describes HASH.
fn describe(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const id = git.gitOid(try oidArg(env, args[1]));
    const abbrev = try env.getInt(args[2]);
    var obj: ?*c.git_object = null;
    _ = try check(env, c.git_object_lookup(&obj, repo, &id, c.GIT_OBJECT_ANY));
    defer c.git_object_free(obj);
    var opts: c.git_describe_options = .{};
    _ = try check(env, c.git_describe_options_init(&opts, c.GIT_DESCRIBE_OPTIONS_VERSION));
    opts.describe_strategy = c.GIT_DESCRIBE_TAGS;
    var result: ?*c.git_describe_result = null;
    if (!try found(env, c.git_describe_commit(&result, obj, &opts), &.{})) return env.nil();
    defer c.git_describe_result_free(result);
    var fopts: c.git_describe_format_options = .{};
    _ = try check(env, c.git_describe_format_options_init(&fopts, c.GIT_DESCRIBE_FORMAT_OPTIONS_VERSION));
    fopts.abbreviated_size = @intCast(@max(4, @min(40, abbrev)));
    fopts.always_use_long_format = 1;
    var buf: c.git_buf = .{};
    defer c.git_buf_dispose(&buf);
    _ = try check(env, c.git_describe_format(&buf, result, &fopts));
    return text(env, buf.ptr[0..buf.size]);
}

fn signature(
    env: Env,
    b: *ListBuilder,
    comptime prefix: []const u8,
    sig: [*c]const c.git_signature,
) Error!void {
    try kw(env, b, ":" ++ prefix ++ "-name", try ctext(env, sig.*.name));
    try kw(env, b, ":" ++ prefix ++ "-email", try ctext(env, sig.*.email));
    try kw(env, b, ":" ++ prefix ++ "-time", try env.int(sig.*.when.time));
    try kw(env, b, ":" ++ prefix ++ "-offset", try env.int(sig.*.when.offset));
}

fn commitPlist(
    env: Env,
    repo: *c.git_repository,
    odb: ?*c.git_odb,
    mailmap: ?*c.git_mailmap,
    oid: git.Oid,
    abbrev: usize,
) Error!Value {
    var commit: ?*c.git_commit = null;
    const id = git.gitOid(oid);
    _ = try check(env, c.git_commit_lookup(&commit, repo, &id));
    defer c.git_commit_free(commit);
    var b: ListBuilder = .{};
    errdefer b.deinit();
    const h = git.hex(oid);
    try kw(env, &b, ":hash", try env.string(&h));
    try kw(env, &b, ":short", try env.string(h[0..try uniqueLength(env, odb, oid, abbrev)]));
    var parents: ListBuilder = .{};
    errdefer parents.deinit();
    const n = c.git_commit_parentcount(commit);
    var i: c_uint = 0;
    while (i < n) : (i += 1) {
        try parents.append(try hexValue(env, git.oidOf(c.git_commit_parent_id(commit, i))));
    }
    try kw(env, &b, ":parents", try parents.finish(env));
    try signature(env, &b, "author", c.git_commit_author(commit));
    try signature(env, &b, "committer", c.git_commit_committer(commit));
    if (mailmap != null) {
        var mm: [*c]c.git_signature = null;
        _ = try check(env, c.git_commit_author_with_mailmap(&mm, commit, mailmap));
        defer c.git_signature_free(mm);
        try kw(env, &b, ":mailmap-author-name", try ctext(env, mm.*.name));
        try kw(env, &b, ":mailmap-author-email", try ctext(env, mm.*.email));
        var mc: [*c]c.git_signature = null;
        _ = try check(env, c.git_commit_committer_with_mailmap(&mc, commit, mailmap));
        defer c.git_signature_free(mc);
        try kw(env, &b, ":mailmap-committer-name", try ctext(env, mc.*.name));
        try kw(env, &b, ":mailmap-committer-email", try ctext(env, mc.*.email));
    }
    try kw(env, &b, ":encoding", try ctext(env, c.git_commit_message_encoding(commit)));
    try kw(env, &b, ":message", try ctext(env, c.git_commit_message_raw(commit)));
    return b.finish(env);
}

/// (magit-git2--log DIR STARTS MAX NO-WALK ABBREV MAILMAP): the commits
/// `git log' shows for STARTS, ((HASH . UNINTERESTING) ...) in command
/// line order, as plists; at most MAX of them (nil: no limit).  :short
/// is the hash abbreviated to at least ABBREV digits; with MAILMAP the
/// plists also have the mailmapped author.
fn log(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const items = try env.listItems(args[1]);
    defer env.freeValues(items);
    const max: ?usize = if (env.isNil(args[2])) null else @intCast(@max(0, try env.getInt(args[2])));
    const no_walk = !env.isNil(args[3]);
    const abbrev: usize = @intCast(@max(4, @min(40, try env.getInt(args[4]))));

    var starts: std.ArrayList(walk.Start) = .empty;
    defer starts.deinit(allocator);
    for (items) |item| {
        const pair = try env.listItems(try env.call("list", &.{
            try env.call("car", &.{item}),
            try env.call("cdr", &.{item}),
        }));
        defer env.freeValues(pair);
        try starts.append(allocator, .{
            .oid = try oidArg(env, pair[0]),
            .uninteresting = !env.isNil(pair[1]),
        });
    }

    var walker = walk.Walker(git.Source).init(allocator, .{ .repo = repo });
    defer walker.deinit();
    var order: std.ArrayList(git.Oid) = .empty;
    defer order.deinit(allocator);
    walker.run(starts.items, max, no_walk, &order) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.MissingCommit => {
            git.last = .{ .code = c.GIT_ENOTFOUND };
            const msg = "commit missing from the object database";
            @memcpy(git.last.message[0..msg.len], msg);
            git.last.message_len = msg.len;
            return signalGit(env);
        },
        error.SourceFailed => return signalGit(env),
    };

    var odb: ?*c.git_odb = null;
    _ = try check(env, c.git_repository_odb(&odb, repo));
    defer c.git_odb_free(odb);
    var mailmap: ?*c.git_mailmap = null;
    if (!env.isNil(args[5]))
        _ = try check(env, c.git_mailmap_from_repository(&mailmap, repo));
    defer if (mailmap != null) c.git_mailmap_free(mailmap);

    var out: ListBuilder = .{};
    errdefer out.deinit();
    for (order.items) |oid| {
        try out.append(try commitPlist(env, repo, odb, mailmap, oid, abbrev));
    }
    return out.finish(env);
}

/// (magit-git2--commits DIR HASHES ABBREV MAILMAP): plists like those
/// of `magit-git2--log' for HASHES, in their order.
fn commits(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const items = try env.listItems(args[1]);
    defer env.freeValues(items);
    const abbrev: usize = @intCast(@max(4, @min(40, try env.getInt(args[2]))));
    var odb: ?*c.git_odb = null;
    _ = try check(env, c.git_repository_odb(&odb, repo));
    defer c.git_odb_free(odb);
    var mailmap: ?*c.git_mailmap = null;
    if (!env.isNil(args[3]))
        _ = try check(env, c.git_mailmap_from_repository(&mailmap, repo));
    defer if (mailmap != null) c.git_mailmap_free(mailmap);
    var out: ListBuilder = .{};
    errdefer out.deinit();
    for (items) |item| {
        try out.append(try commitPlist(env, repo, odb, mailmap, try oidArg(env, item), abbrev));
    }
    return out.finish(env);
}

/// (magit-git2--reflog DIR REFNAME): ((NEW OLD NAME EMAIL TIME OFFSET
/// MESSAGE) ...), newest first; nil when REFNAME has no reflog.
fn reflog(env: Env, args: []const Value) Error!Value {
    const repo = try repoArg(env, args[0]) orelse return env.nil();
    const name = try env.getString(args[1]);
    defer env.free(name);
    var log_: ?*c.git_reflog = null;
    if (!try found(env, c.git_reflog_read(&log_, repo, name.ptr), &.{c.GIT_EINVALIDSPEC}))
        return env.nil();
    defer c.git_reflog_free(log_);
    var out: ListBuilder = .{};
    errdefer out.deinit();
    const n = c.git_reflog_entrycount(log_);
    var i: usize = 0;
    while (i < n) : (i += 1) {
        const e = c.git_reflog_entry_byindex(log_, i);
        const sig = c.git_reflog_entry_committer(e);
        try out.append(try env.list(&.{
            try hexValue(env, git.oidOf(c.git_reflog_entry_id_new(e))),
            try hexValue(env, git.oidOf(c.git_reflog_entry_id_old(e))),
            try ctext(env, sig.*.name),
            try ctext(env, sig.*.email),
            try env.int(sig.*.when.time),
            try env.int(sig.*.when.offset),
            try ctext(env, c.git_reflog_entry_message(e)),
        }));
    }
    return out.finish(env);
}

// Registration

const Def = struct {
    name: [*:0]const u8,
    arity: usize,
    f: fn (Env, []const Value) Error!Value,
    doc: [*:0]const u8,
};

const defs = [_]Def{
    .{ .name = "magit-git2--version", .arity = 0, .f = version, .doc = "Return the libgit2 version as a string." },
    .{ .name = "magit-git2--release", .arity = 0, .f = release, .doc = "Free all cached repository handles." },
    .{ .name = "magit-git2--set-search-path", .arity = 2, .f = setSearchPath, .doc = "Set the config search path of LEVEL to PATH (nil: default).\n\n(fn LEVEL PATH)" },
    .{ .name = "magit-git2--set-system-config-path", .arity = 1, .f = setSystemConfigPath, .doc = "Read the system config from DIR/gitconfig (nil: default).\n\n(fn DIR)" },
    .{ .name = "magit-git2--repo-info", .arity = 1, .f = repoInfo, .doc = "Return a plist describing the repository of DIR, or nil.\n\n(fn DIR)" },
    .{ .name = "magit-git2--revparse", .arity = 2, .f = revparse, .doc = "Resolve SPEC to (HASH TYPE REFNAME), or nil.\n\n(fn DIR SPEC)" },
    .{ .name = "magit-git2--short-id", .arity = 3, .f = shortId, .doc = "Abbreviate HASH uniquely to at least MINLEN digits.\n\n(fn DIR HASH MINLEN)" },
    .{ .name = "magit-git2--ref", .arity = 2, .f = refLookup, .doc = "Look up ref NAME: nil, (symbolic TARGET) or (direct HASH).\n\n(fn DIR NAME)" },
    .{ .name = "magit-git2--refs", .arity = 1, .f = refs, .doc = "Return ((NAME HASH PEELED SYMREF) ...) for all refs.\n\n(fn DIR)" },
    .{ .name = "magit-git2--head", .arity = 1, .f = head, .doc = "Return HEAD as plist (:symref NAME :hash HASH).\n\n(fn DIR)" },
    .{ .name = "magit-git2--config", .arity = 2, .f = config, .doc = "Return ((NAME VALUE LEVEL) ...) of LEVEL (nil: all).\n\n(fn DIR LEVEL)" },
    .{ .name = "magit-git2--upstream", .arity = 2, .f = upstream, .doc = "Return the upstream ref of local BRANCH, or nil.\n\n(fn DIR BRANCH)" },
    .{ .name = "magit-git2--ahead-behind", .arity = 3, .f = aheadBehind, .doc = "Return (AHEAD . BEHIND) of LOCAL against UPSTREAM.\n\n(fn DIR LOCAL UPSTREAM)" },
    .{ .name = "magit-git2--merge-base", .arity = 3, .f = mergeBase, .doc = "Return a merge base of A and B, or nil.\n\n(fn DIR A B)" },
    .{ .name = "magit-git2--descendant-p", .arity = 3, .f = descendantP, .doc = "Whether COMMIT descends from ANCESTOR.\n\n(fn DIR COMMIT ANCESTOR)" },
    .{ .name = "magit-git2--reachable-from-any", .arity = 3, .f = reachableFromAny, .doc = "Whether COMMIT is reachable from any of HASHES.\n\n(fn DIR COMMIT HASHES)" },
    .{ .name = "magit-git2--tips-reaching", .arity = 4, .f = tipsReaching, .doc = "Return the TIPS that reach COMMIT through commits dated CUTOFF or later.\n\n(fn DIR COMMIT TIPS CUTOFF)" },
    .{ .name = "magit-git2--describe", .arity = 3, .f = describe, .doc = "Describe HASH by tags in the long format, or nil.\n\n(fn DIR HASH ABBREV)" },
    .{ .name = "magit-git2--log", .arity = 6, .f = log, .doc = "Return the commits git log shows for STARTS.\n\n(fn DIR STARTS MAX NO-WALK ABBREV MAILMAP)" },
    .{ .name = "magit-git2--commits", .arity = 4, .f = commits, .doc = "Return commit plists for HASHES.\n\n(fn DIR HASHES ABBREV MAILMAP)" },
    .{ .name = "magit-git2--reflog", .arity = 2, .f = reflog, .doc = "Return the reflog of REFNAME, newest first.\n\n(fn DIR REFNAME)" },
};

export fn emacs_module_init(rt: *ec.struct_emacs_runtime) callconv(.c) c_int {
    // The ABI only grows: an Emacs at least as new as the oldest
    // environment used here (25) is fine.
    if (rt.size < @sizeOf(ec.struct_emacs_runtime)) return 1;
    const raw = rt.get_environment.?(rt);
    if (raw.*.size < @sizeOf(ec.struct_emacs_env_25)) return 2;
    const env = Env{ .raw = raw };
    if (c.git_libgit2_init() < 0) return 3;
    // libgit2 caches up to 256 MiB of objects per process by default.
    _ = c.git_libgit2_opts(c.GIT_OPT_SET_CACHE_MAX_SIZE, @as(isize, 32 * 1024 * 1024));
    init(env) catch return 4;
    return 0;
}

fn init(env: Env) Error!void {
    _ = try env.call("define-error", &.{
        try env.intern("magit-git2-error"),
        try env.string("libgit2 error"),
    });
    inline for (defs) |d| {
        try env.defun(d.name, d.arity, emacs.trampoline(d.f), d.doc);
    }
    _ = try env.call("provide", &.{try env.intern("magit-git2-core-module")});
}

test {
    _ = git;
}
