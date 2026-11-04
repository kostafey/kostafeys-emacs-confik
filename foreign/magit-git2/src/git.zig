// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Kostafey <kostafey@gmail.com>

//! libgit2 wrappers: the repository cache, error capture, object ids.

const std = @import("std");
pub const c = @import("git2_c");
const walk = @import("walk.zig");

const allocator = std.heap.c_allocator;

/// A failed libgit2 call; `last` describes it.
pub const Error = error{Git};

pub const Failure = struct {
    code: c_int = 0,
    klass: c_int = 0,
    message: [512]u8 = undefined,
    message_len: usize = 0,

    pub fn text(self: *const Failure) []const u8 {
        return self.message[0..self.message_len];
    }
};

/// The last failure, copied out of libgit2's thread-local error before
/// another call can overwrite it.
pub var last: Failure = .{};

/// Check the return code RC of a libgit2 call: negative is a failure.
pub fn check(rc: c_int) Error!c_int {
    if (rc >= 0) return rc;
    last = .{ .code = rc };
    if (c.git_error_last()) |e| {
        last.klass = e.*.klass;
        if (e.*.message) |m| {
            const s = std.mem.span(m);
            const n = @min(s.len, last.message.len);
            @memcpy(last.message[0..n], s[0..n]);
            last.message_len = n;
        }
    }
    return error.Git;
}

/// Like `check', but GIT_ENOTFOUND (and the codes listed in ALSO) give
/// false instead of a failure.
pub fn found(rc: c_int, also: []const c_int) Error!bool {
    if (rc == c.GIT_ENOTFOUND) return false;
    for (also) |code| if (rc == code) return false;
    _ = try check(rc);
    return true;
}

pub const Oid = walk.Oid;

pub fn oidOf(id: *const c.git_oid) Oid {
    return id.id;
}

pub fn gitOid(oid: Oid) c.git_oid {
    return .{ .id = oid };
}

pub const Hex = [40]u8;

pub fn hex(oid: Oid) Hex {
    return std.fmt.bytesToHex(oid, .lower);
}

/// Parse a full 40-digit hex id.
pub fn parseHex(s: []const u8) ?Oid {
    if (s.len != 40) return null;
    var oid: Oid = undefined;
    _ = std.fmt.hexToBytes(&oid, s) catch return null;
    return oid;
}

// The repository cache: one handle per git directory.  Emacs never
// unloads a module, so the handles live until `releaseAll'; Elisp calls
// that after each top-level operation, so that on Windows no pack file
// stays open and blocks `git gc'.

const Entry = struct {
    gitdir: [:0]u8,
    repo: *c.git_repository,
};

var cache: std.ArrayList(Entry) = .empty;

pub fn releaseAll() void {
    for (cache.items) |e| {
        c.git_repository_free(e.repo);
        allocator.free(e.gitdir);
    }
    cache.clearRetainingCapacity();
}

/// The repository DIR belongs to, or null if it belongs to none.
pub fn open(dir: [:0]const u8) (Error || error{OutOfMemory})!?*c.git_repository {
    var buf: c.git_buf = .{};
    defer c.git_buf_dispose(&buf);
    if (!try found(c.git_repository_discover(&buf, dir.ptr, 0, null), &.{}))
        return null;
    const gitdir = buf.ptr[0..buf.size];
    for (cache.items) |e| {
        if (std.mem.eql(u8, e.gitdir, gitdir)) return e.repo;
    }
    var repo: ?*c.git_repository = null;
    _ = try check(c.git_repository_open_ext(
        &repo,
        buf.ptr,
        c.GIT_REPOSITORY_OPEN_NO_SEARCH,
        null,
    ));
    errdefer c.git_repository_free(repo);
    const copy = try allocator.dupeSentinel(u8, gitdir, 0);
    errdefer allocator.free(copy);
    try cache.append(allocator, .{ .gitdir = copy, .repo = repo.? });
    return repo.?;
}

/// Commits for the walker, read through libgit2.
pub const Source = struct {
    repo: *c.git_repository,

    pub fn parse(
        self: Source,
        gpa: std.mem.Allocator,
        oid: Oid,
        parents: *std.ArrayList(Oid),
    ) walk.Error!?i64 {
        var commit: ?*c.git_commit = null;
        const id = gitOid(oid);
        const ok = found(c.git_commit_lookup(&commit, self.repo, &id), &.{}) catch
            return error.SourceFailed;
        if (!ok) return null;
        defer c.git_commit_free(commit);
        const n = c.git_commit_parentcount(commit);
        var i: c_uint = 0;
        while (i < n) : (i += 1) {
            try parents.append(gpa, oidOf(c.git_commit_parent_id(commit, i)));
        }
        return c.git_commit_time(commit);
    }
};

test "hex round trip" {
    const oid = parseHex("209709708715f4dc81245e9a81912e538886bfc0").?;
    try std.testing.expectEqualStrings(
        "209709708715f4dc81245e9a81912e538886bfc0",
        &hex(oid),
    );
    try std.testing.expect(parseHex("2097") == null);
    try std.testing.expect(parseHex("z09709708715f4dc81245e9a81912e538886bfc0") == null);
}
