// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Kostafey <kostafey@gmail.com>

//! The order `git log' lists commits in when no ordering option is
//! given, for start commits, negative (^) commits, --no-walk and -n.
//!
//! What git does, as its output shows:
//!
//! - A queue holds commits newest committer date first; commits with
//!   equal dates leave it in the order they entered it.  It starts with
//!   the start commits, and a commit leaving it brings in its parents.
//!   No commit enters twice.
//! - Without negative commits the walk streams: commits are listed as
//!   they leave the queue.  --no-walk lists the start commits alone, in
//!   queue order.
//! - Negative commits exclude what they reach, as far as the walk has
//!   read the history: from a negative start commit, and from every
//!   excluded commit that leaves the queue, the exclusion runs down the
//!   parent links already read.  The walk collects the included commits
//!   that leave the queue and lists, at the end, those still included.
//!   It stops when, five times in a row, an excluded commit leaves a
//!   queue that then holds only excluded commits, none of them dated at
//!   or after the last included one.  So with clock skew it lists
//!   commits a negative one reaches, as git does.  A negative commit
//!   cancels --no-walk.
//!
//! libgit2's revwalk is not used because its date-ordered queue does not
//! keep equal dates in the order they were queued.
//!
//! The walker reads commits through a `Source': `parse(gpa, oid,
//! parents)' appends the parent ids of commit OID to PARENTS and returns
//! its committer date, or null when the object is missing.  The libgit2
//! source is in git.zig; the tests below use an in-memory graph.

const std = @import("std");

pub const Oid = [20]u8;

pub const Start = struct {
    oid: Oid,
    uninteresting: bool,
};

pub const Error = error{ OutOfMemory, MissingCommit, SourceFailed };

/// How many excluded commits in a row may leave a queue with nothing left
/// to find before a walk with negative commits ends.
const patience = 5;

const Commit = struct {
    id: Oid,
    date: i64 = 0,
    /// Indices of the parents; null until the commit is read.
    parents: ?[]const u32 = null,
    excluded: bool = false,
    entered: bool = false,
    queued: bool = false,
};

/// A queue entry; `seq' keeps equal dates in arrival order.
const Entry = struct {
    date: i64,
    seq: u64,
    commit: u32,

    fn first(_: void, a: Entry, b: Entry) std.math.Order {
        if (a.date != b.date) return std.math.order(b.date, a.date);
        return std.math.order(a.seq, b.seq);
    }
};

pub fn Walker(comptime Source: type) type {
    return struct {
        const Self = @This();

        gpa: std.mem.Allocator,
        arena: std.heap.ArenaAllocator,
        source: Source,
        commits: std.ArrayList(Commit) = .empty,
        by_id: std.AutoHashMapUnmanaged(Oid, u32) = .empty,
        queue: std.PriorityQueue(Entry, void, Entry.first) = .empty,
        arrivals: u64 = 0,
        /// How many queued commits are not excluded.
        included_queued: usize = 0,
        ids: std.ArrayList(Oid) = .empty,

        pub fn init(gpa: std.mem.Allocator, source: Source) Self {
            return .{ .gpa = gpa, .arena = .init(gpa), .source = source };
        }

        pub fn deinit(self: *Self) void {
            self.commits.deinit(self.gpa);
            self.by_id.deinit(self.gpa);
            self.queue.deinit(self.gpa);
            self.ids.deinit(self.gpa);
            self.arena.deinit();
        }

        /// The index of commit ID, added unread if new.
        fn at(self: *Self, id: Oid) Error!u32 {
            const slot = try self.by_id.getOrPut(self.gpa, id);
            if (!slot.found_existing) {
                slot.value_ptr.* = @intCast(self.commits.items.len);
                try self.commits.append(self.gpa, .{ .id = id });
            }
            return slot.value_ptr.*;
        }

        /// Read the date and parents of commit I unless known; false when
        /// its object is missing.
        fn read(self: *Self, i: u32) Error!bool {
            if (self.commits.items[i].parents != null) return true;
            self.ids.clearRetainingCapacity();
            const date = (try self.source.parse(self.gpa, self.commits.items[i].id, &self.ids)) orelse
                return false;
            const parents = try self.arena.allocator().alloc(u32, self.ids.items.len);
            for (self.ids.items, parents) |id, *p| p.* = try self.at(id);
            self.commits.items[i].date = date;
            self.commits.items[i].parents = parents;
            return true;
        }

        fn parentsOf(self: *Self, i: u32) []const u32 {
            return self.commits.items[i].parents orelse &.{};
        }

        fn exclude(self: *Self, i: u32) void {
            const c = &self.commits.items[i];
            if (c.excluded) return;
            c.excluded = true;
            if (c.queued) self.included_queued -= 1;
        }

        /// Exclude what lies below commit I through the parent links read
        /// so far, stopping at commits already excluded.
        fn excludeBelow(self: *Self, i: u32) Error!void {
            var todo: std.ArrayList(u32) = .empty;
            defer todo.deinit(self.gpa);
            try todo.appendSlice(self.gpa, self.parentsOf(i));
            while (todo.pop()) |j| {
                if (self.commits.items[j].excluded) continue;
                self.exclude(j);
                try todo.appendSlice(self.gpa, self.parentsOf(j));
            }
        }

        fn enter(self: *Self, i: u32) Error!void {
            const c = &self.commits.items[i];
            if (c.entered) return;
            c.entered = true;
            c.queued = true;
            if (!c.excluded) self.included_queued += 1;
            try self.queue.push(self.gpa, .{ .date = c.date, .seq = self.arrivals, .commit = i });
            self.arrivals += 1;
        }

        fn leave(self: *Self) ?u32 {
            const e = self.queue.pop() orelse return null;
            const c = &self.commits.items[e.commit];
            c.queued = false;
            if (!c.excluded) self.included_queued -= 1;
            return e.commit;
        }

        /// Bring in the parents of commit I, which has just left the
        /// queue.  An excluded commit passes its exclusion on, and a
        /// missing parent of it is no error.
        fn follow(self: *Self, i: u32) Error!void {
            const excluded = self.commits.items[i].excluded;
            for (self.parentsOf(i)) |p| {
                if (excluded) {
                    self.exclude(p);
                    if (!try self.read(p)) continue;
                    try self.excludeBelow(p);
                } else if (!try self.read(p)) return error.MissingCommit;
                try self.enter(p);
            }
        }

        /// Append commit I to OUT; false once that used up LEFT.
        fn list(self: *Self, i: u32, out: *std.ArrayList(Oid), left: *usize) Error!bool {
            try out.append(self.gpa, self.commits.items[i].id);
            left.* -= 1;
            return left.* > 0;
        }

        /// Walk from STARTS like `git log' and append up to MAX_COUNT
        /// (null: no limit) commit ids to OUT, in git's order.
        pub fn run(
            self: *Self,
            starts: []const Start,
            max_count: ?usize,
            no_walk: bool,
            out: *std.ArrayList(Oid),
        ) Error!void {
            var left = max_count orelse std.math.maxInt(usize);
            if (left == 0) return;

            // A commit given both ways is excluded.
            var ranged = false;
            for (starts) |s| {
                const i = try self.at(s.oid);
                if (!s.uninteresting) continue;
                self.commits.items[i].excluded = true;
                ranged = true;
            }
            for (starts) |s| {
                const i = try self.at(s.oid);
                if (!try self.read(i)) return error.MissingCommit;
                if (self.commits.items[i].excluded) try self.excludeBelow(i);
                try self.enter(i);
            }

            if (!ranged) {
                while (self.leave()) |i| {
                    if (!no_walk) try self.follow(i);
                    if (!try self.list(i, out, &left)) return;
                }
                return;
            }

            var picked: std.ArrayList(u32) = .empty;
            defer picked.deinit(self.gpa);
            var last_included: i64 = std.math.maxInt(i64);
            var streak: u32 = 0;
            while (self.leave()) |i| {
                try self.follow(i);
                if (!self.commits.items[i].excluded) {
                    last_included = self.commits.items[i].date;
                    try picked.append(self.gpa, i);
                    continue;
                }
                const newest = self.queue.peek() orelse break;
                if (self.included_queued > 0 or newest.date >= last_included) {
                    streak = 0;
                    continue;
                }
                streak += 1;
                if (streak == patience) break;
            }
            for (picked.items) |i| {
                if (self.commits.items[i].excluded) continue;
                if (!try self.list(i, out, &left)) return;
            }
        }
    };
}

// Tests: commits named by one letter, oid = the letter repeated.

const TestGraph = struct {
    // name, date, parents
    commits: []const struct { u8, i64, []const u8 },

    fn oid(name: u8) Oid {
        return @splat(name);
    }

    pub fn parse(self: TestGraph, gpa: std.mem.Allocator, id: Oid, out: *std.ArrayList(Oid)) Error!?i64 {
        for (self.commits) |cm| {
            if (cm[0] != id[0]) continue;
            for (cm[2]) |p| try out.append(gpa, oid(p));
            return cm[1];
        }
        return null;
    }
};

fn expectWalk(
    graph: TestGraph,
    starts: []const struct { u8, bool },
    max: ?usize,
    no_walk: bool,
    expected: []const u8,
) !void {
    const gpa = std.testing.allocator;
    var w = Walker(TestGraph).init(gpa, graph);
    defer w.deinit();
    var s: std.ArrayList(Start) = .empty;
    defer s.deinit(gpa);
    for (starts) |st| try s.append(gpa, .{ .oid = TestGraph.oid(st[0]), .uninteresting = st[1] });
    var out: std.ArrayList(Oid) = .empty;
    defer out.deinit(gpa);
    try w.run(s.items, max, no_walk, &out);
    var names: std.ArrayList(u8) = .empty;
    defer names.deinit(gpa);
    for (out.items) |o| try names.append(gpa, o[0]);
    try std.testing.expectEqualStrings(expected, names.items);
}

//   a - b - c - d (HEAD)       a=1 b=2 c=3 d=4
//        \     /
//         x - y                x=5 y=3
const merge_graph: TestGraph = .{ .commits = &.{
    .{ 'a', 1, "" },
    .{ 'b', 2, "a" },
    .{ 'c', 3, "b" },
    .{ 'x', 5, "b" },
    .{ 'y', 3, "x" },
    .{ 'd', 4, "cy" },
} };

test "date order, equal dates in insertion order" {
    // c and y share date 3: c, the first parent, is queued first.
    try expectWalk(merge_graph, &.{.{ 'd', false }}, null, false, "dcyxba");
}

test "max count" {
    try expectWalk(merge_graph, &.{.{ 'd', false }}, 3, false, "dcy");
}

test "range excludes the negative side" {
    // d ^y: y, x, b, a uninteresting.
    try expectWalk(merge_graph, &.{ .{ 'y', true }, .{ 'd', false } }, null, false, "dc");
}

test "no-walk shows only the given commits, newest first" {
    try expectWalk(merge_graph, &.{ .{ 'b', false }, .{ 'x', false } }, null, true, "xb");
}

test "a negative revision turns no-walk off" {
    try expectWalk(merge_graph, &.{ .{ 'b', true }, .{ 'd', false } }, null, true, "dcyx");
}

test "clock skew: a commit older than its parent" {
    // p (date 10) has parent q (date 20): git still shows p before q.
    const g: TestGraph = .{ .commits = &.{
        .{ 'q', 20, "" },
        .{ 'p', 10, "q" },
    } };
    try expectWalk(g, &.{.{ 'p', false }}, null, false, "pq");
}

test "clock skew: a commit collected early is excluded later" {
    // d ^y: z leaves the queue before the walk down y, through the
    // skewed m and k, reaches it, so z is collected, then dropped.
    const g: TestGraph = .{ .commits = &.{
        .{ 'z', 5, "" },
        .{ 'd', 10, "z" },
        .{ 'k', 1, "z" },
        .{ 'm', 1, "k" },
        .{ 'y', 8, "m" },
    } };
    try expectWalk(g, &.{ .{ 'd', false }, .{ 'y', true } }, null, false, "d");
}

test "clock skew: the walk gives up before reaching a commit" {
    // d ^y: q lies below y, but six old excluded commits lie between,
    // and the walk ends after five of them: git lists q too.
    const g: TestGraph = .{ .commits = &.{
        .{ 'q', 10, "" },
        .{ 'd', 20, "q" },
        .{ '7', 5, "q" },
        .{ '6', 5, "7" },
        .{ '5', 5, "6" },
        .{ '4', 5, "5" },
        .{ '3', 5, "4" },
        .{ '2', 5, "3" },
        .{ '1', 5, "2" },
        .{ 'y', 19, "1" },
    } };
    try expectWalk(g, &.{ .{ 'd', false }, .{ 'y', true } }, null, false, "dq");
}

test "missing start commit" {
    const gpa = std.testing.allocator;
    var w = Walker(TestGraph).init(gpa, merge_graph);
    defer w.deinit();
    var out: std.ArrayList(Oid) = .empty;
    defer out.deinit(gpa);
    try std.testing.expectError(
        error.MissingCommit,
        w.run(&.{.{ .oid = TestGraph.oid('z'), .uninteresting = false }}, null, false, &out),
    );
}
