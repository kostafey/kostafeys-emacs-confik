//! The commit order of `git log' without --topo-order/--date-order: a port
//! of revision.c (git 2.55) for start commits, negative (^) commits,
//! --no-walk and -n, nothing else.
//!
//! libgit2's own revwalk is not used because its time-ordered queue does
//! not keep commits with equal dates in insertion order, which git's does.
//!
//! The walker is generic over a commit source: `Source.parse(gpa, oid,
//! parents)' appends the parent ids of commit OID to PARENTS and returns
//! its committer date, or null when the object is missing.  The libgit2
//! source lives in main.zig; the tests below use an in-memory graph.

const std = @import("std");

pub const Oid = [20]u8;

pub const Start = struct {
    oid: Oid,
    uninteresting: bool,
};

pub const Error = error{ OutOfMemory, MissingCommit, SourceFailed };

/// SLOP from revision.c: how many uninteresting commits limit_list looks
/// at past the last interesting one before it gives up.
const slop_initial = 5;

const Node = struct {
    oid: Oid,
    date: i64 = 0,
    parents_start: u32 = 0,
    parents_len: u32 = 0,
    parsed: bool = false,
    seen: bool = false,
    added: bool = false,
    uninteresting: bool = false,
    shown: bool = false,
};

pub fn Walker(comptime Source: type) type {
    return struct {
        const Self = @This();

        gpa: std.mem.Allocator,
        source: Source,
        nodes: std.ArrayList(Node) = .empty,
        edges: std.ArrayList(u32) = .empty,
        index: std.AutoHashMapUnmanaged(Oid, u32) = .empty,
        scratch: std.ArrayList(Oid) = .empty,

        pub fn init(gpa: std.mem.Allocator, source: Source) Self {
            return .{ .gpa = gpa, .source = source };
        }

        pub fn deinit(self: *Self) void {
            self.nodes.deinit(self.gpa);
            self.edges.deinit(self.gpa);
            self.index.deinit(self.gpa);
            self.scratch.deinit(self.gpa);
        }

        fn node(self: *Self, oid: Oid) Error!u32 {
            const gop = try self.index.getOrPut(self.gpa, oid);
            if (!gop.found_existing) {
                gop.value_ptr.* = @intCast(self.nodes.items.len);
                try self.nodes.append(self.gpa, .{ .oid = oid });
            }
            return gop.value_ptr.*;
        }

        /// Parse node N; false if its object is missing.
        fn parse(self: *Self, n: u32) Error!bool {
            if (self.nodes.items[n].parsed) return true;
            self.scratch.clearRetainingCapacity();
            const date = (try self.source.parse(self.gpa, self.nodes.items[n].oid, &self.scratch)) orelse
                return false;
            const start: u32 = @intCast(self.edges.items.len);
            // `node' may grow `nodes', so look the node up again after.
            for (self.scratch.items) |p| {
                const pn = try self.node(p);
                try self.edges.append(self.gpa, pn);
            }
            const nd = &self.nodes.items[n];
            nd.date = date;
            nd.parents_start = start;
            nd.parents_len = @intCast(self.scratch.items.len);
            nd.parsed = true;
            return true;
        }

        fn parents(self: *Self, n: u32) []const u32 {
            const nd = self.nodes.items[n];
            if (!nd.parsed) return &.{};
            return self.edges.items[nd.parents_start..][0..nd.parents_len];
        }

        // mark_parents_uninteresting(): only parsed commits pass the mark
        // on to their own parents.
        fn markParentsUninteresting(self: *Self, n: u32) Error!void {
            var stack: std.ArrayList(u32) = .empty;
            defer stack.deinit(self.gpa);
            for (self.parents(n)) |p| try self.markOne(p, &stack);
            while (stack.pop()) |p| try self.markOne(p, &stack);
        }

        fn markOne(self: *Self, n: u32, stack: *std.ArrayList(u32)) Error!void {
            if (self.nodes.items[n].uninteresting) return;
            self.nodes.items[n].uninteresting = true;
            for (self.parents(n)) |p| try stack.append(self.gpa, p);
        }

        // process_parents()
        fn processParents(self: *Self, n: u32, queue: *Queue) Error!void {
            if (self.nodes.items[n].added) return;
            self.nodes.items[n].added = true;
            if (self.nodes.items[n].uninteresting) {
                const ps = self.nodes.items[n];
                var i: u32 = 0;
                while (i < ps.parents_len) : (i += 1) {
                    const p = self.edges.items[ps.parents_start + i];
                    self.nodes.items[p].uninteresting = true;
                    if (!try self.parse(p)) continue;
                    if (self.nodes.items[p].parents_len > 0)
                        try self.markParentsUninteresting(p);
                    if (self.nodes.items[p].seen) continue;
                    self.nodes.items[p].seen = true;
                    try queue.put(self, p);
                }
                return;
            }
            const ps = self.nodes.items[n];
            var i: u32 = 0;
            while (i < ps.parents_len) : (i += 1) {
                const p = self.edges.items[ps.parents_start + i];
                if (!try self.parse(p)) return error.MissingCommit;
                if (!self.nodes.items[p].seen) {
                    self.nodes.items[p].seen = true;
                    try queue.put(self, p);
                }
            }
        }

        // everybody_uninteresting()
        fn everybodyUninteresting(self: *Self, queue: *const Queue, cache: *?u32) bool {
            if (cache.*) |n| {
                if (!self.nodes.items[n].uninteresting) return false;
            }
            for (queue.items.items) |item| {
                if (self.nodes.items[item.node].uninteresting) continue;
                cache.* = item.node;
                return false;
            }
            return true;
        }

        // still_interesting()
        fn stillInteresting(self: *Self, queue: *const Queue, date: i64, slop: u32, cache: *?u32) u32 {
            const head = queue.peek() orelse return 0;
            if (date <= self.nodes.items[head].date) return slop_initial;
            if (!self.everybodyUninteresting(queue, cache)) return slop_initial;
            return slop - 1;
        }

        // limit_list()
        fn limitList(self: *Self, list: []const u32, out: *std.ArrayList(u32)) Error!void {
            var queue: Queue = .{};
            defer queue.deinit(self.gpa);
            for (list) |n| try queue.put(self, n);
            var slop: u32 = slop_initial;
            var date: i64 = std.math.maxInt(i64);
            var cache: ?u32 = null;
            while (queue.get(self)) |n| {
                if (cache == n) cache = null;
                try self.processParents(n, &queue);
                if (self.nodes.items[n].uninteresting) {
                    try self.markParentsUninteresting(n);
                    slop = self.stillInteresting(&queue, date, slop, &cache);
                    if (slop != 0) continue;
                    break;
                }
                date = self.nodes.items[n].date;
                try out.append(self.gpa, n);
            }
        }

        /// Whether get_commit_action() would show N; marks it shown if so.
        fn show(self: *Self, n: u32) bool {
            const nd = &self.nodes.items[n];
            if (nd.shown or nd.uninteresting) return false;
            nd.shown = true;
            return true;
        }

        /// Walk from STARTS like `git log' and append up to MAX_COUNT
        /// (null: no limit) commit ids to OUT, in git's order.
        pub fn run(
            self: *Self,
            starts: []const Start,
            max_count: ?usize,
            no_walk_requested: bool,
            out: *std.ArrayList(Oid),
        ) Error!void {
            var remaining = max_count orelse std.math.maxInt(usize);
            if (remaining == 0) return;

            // setup_revisions(): flags go on the objects first, and a
            // negative revision turns --no-walk off.
            var no_walk = no_walk_requested;
            for (starts) |s| {
                const n = try self.node(s.oid);
                if (s.uninteresting) {
                    self.nodes.items[n].uninteresting = true;
                    no_walk = false;
                }
            }

            // prepare_revision_walk() and handle_commit()
            var list: std.ArrayList(u32) = .empty;
            defer list.deinit(self.gpa);
            var limited = false;
            for (starts) |s| {
                const n = try self.node(s.oid);
                if (!try self.parse(n)) return error.MissingCommit;
                if (self.nodes.items[n].uninteresting) {
                    try self.markParentsUninteresting(n);
                    limited = true;
                }
                if (!self.nodes.items[n].seen) {
                    self.nodes.items[n].seen = true;
                    try list.append(self.gpa, n);
                }
            }
            // commit_list_sort_by_date(): a stable sort, newest first.
            std.sort.insertion(u32, list.items, self, newerThan);

            if (no_walk or limited) {
                var commits: std.ArrayList(u32) = .empty;
                defer commits.deinit(self.gpa);
                if (no_walk)
                    try commits.appendSlice(self.gpa, list.items)
                else
                    try self.limitList(list.items, &commits);
                for (commits.items) |n| {
                    if (!self.show(n)) continue;
                    try out.append(self.gpa, self.nodes.items[n].oid);
                    remaining -= 1;
                    if (remaining == 0) return;
                }
                return;
            }

            // The streaming walk: a date-ordered queue, ties in insertion
            // order.
            var queue: Queue = .{};
            defer queue.deinit(self.gpa);
            for (list.items) |n| try queue.put(self, n);
            while (queue.get(self)) |n| {
                try self.processParents(n, &queue);
                if (!self.show(n)) continue;
                try out.append(self.gpa, self.nodes.items[n].oid);
                remaining -= 1;
                if (remaining == 0) return;
            }
        }

        fn newerThan(self: *Self, a: u32, b: u32) bool {
            return self.nodes.items[a].date > self.nodes.items[b].date;
        }

        /// git's prio_queue ordered by compare_commits_by_commit_date:
        /// newest first, equal dates in insertion order.
        const Queue = struct {
            items: std.ArrayList(Item) = .empty,
            counter: u64 = 0,

            const Item = struct { node: u32, ctr: u64 };

            fn before(w: *Self, a: Item, b: Item) bool {
                const da = w.nodes.items[a.node].date;
                const db = w.nodes.items[b.node].date;
                if (da != db) return da > db;
                return a.ctr < b.ctr;
            }

            fn put(q: *Queue, w: *Self, n: u32) Error!void {
                try q.items.append(w.gpa, .{ .node = n, .ctr = q.counter });
                q.counter += 1;
                var i = q.items.items.len - 1;
                const a = q.items.items;
                while (i > 0) {
                    const parent = (i - 1) / 2;
                    if (!before(w, a[i], a[parent])) break;
                    std.mem.swap(Item, &a[i], &a[parent]);
                    i = parent;
                }
            }

            fn peek(q: *const Queue) ?u32 {
                if (q.items.items.len == 0) return null;
                return q.items.items[0].node;
            }

            fn get(q: *Queue, w: *Self) ?u32 {
                const a = q.items.items;
                if (a.len == 0) return null;
                const top = a[0].node;
                const last = q.items.pop().?;
                if (q.items.items.len == 0) return top;
                const b = q.items.items;
                b[0] = last;
                var i: usize = 0;
                while (true) {
                    const l = 2 * i + 1;
                    const r = l + 1;
                    var best = i;
                    if (l < b.len and before(w, b[l], b[best])) best = l;
                    if (r < b.len and before(w, b[r], b[best])) best = r;
                    if (best == i) break;
                    std.mem.swap(Item, &b[i], &b[best]);
                    i = best;
                }
                return top;
            }

            fn deinit(q: *Queue, gpa: std.mem.Allocator) void {
                q.items.deinit(gpa);
            }
        };
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
