// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Kostafey <kostafey@gmail.com>

//! Directory paths as git prints them.  On Windows that is the real name
//! on disk, whatever case or volume the caller's path spells it with.

const std = @import("std");
const builtin = @import("builtin");
const windows = std.os.windows;

const open_existing = 3;
const file_share_all = 7;
const file_flag_backup_semantics = 0x02000000;
const max_path_length = 32768;

extern "kernel32" fn CreateFileW(
    name: [*:0]const u16,
    access: u32,
    share: u32,
    security: ?*anyopaque,
    disposition: u32,
    flags: u32,
    template: ?windows.HANDLE,
) callconv(.winapi) windows.HANDLE;

extern "kernel32" fn GetFinalPathNameByHandleW(
    file: windows.HANDLE,
    path: [*]u16,
    length: u32,
    flags: u32,
) callconv(.winapi) u32;

extern "kernel32" fn CloseHandle(object: windows.HANDLE) callconv(.winapi) c_int;

/// PATH, a final path name, without the `\\?\' that marks it and with
/// forward slashes.  PATH is changed; the result is a part of it.
fn clean(path: []u8) []u8 {
    const unc = "\\\\?\\UNC\\";
    const dos = "\\\\?\\";
    var result = path;
    if (std.mem.startsWith(u8, path, unc)) {
        result = path[unc.len - 2 ..];
        result[0] = '\\';
    } else if (std.mem.startsWith(u8, path, dos)) {
        result = path[dos.len..];
    }
    std.mem.replaceScalar(u8, result, '\\', '/');
    return result;
}

fn finalOnWindows(allocator: std.mem.Allocator, path: []const u8) ?[]u8 {
    const name = std.unicode.utf8ToUtf16LeAllocZ(allocator, path) catch return null;
    defer allocator.free(name);
    const handle = CreateFileW(
        name.ptr,
        0,
        file_share_all,
        null,
        open_existing,
        file_flag_backup_semantics,
        null,
    );
    if (handle == windows.INVALID_HANDLE_VALUE) return null;
    defer _ = CloseHandle(handle);
    const wide = allocator.alloc(u16, max_path_length) catch return null;
    defer allocator.free(wide);
    const length = GetFinalPathNameByHandleW(handle, wide.ptr, max_path_length, 0);
    if (length == 0 or length >= max_path_length) return null;
    const utf8 = std.unicode.utf16LeToUtf8Alloc(allocator, wide[0..length]) catch return null;
    defer allocator.free(utf8);
    return allocator.dupe(u8, clean(utf8)) catch null;
}

/// The directory PATH as git prints it, or null when that cannot be told.
/// The caller frees the result.
pub fn final(allocator: std.mem.Allocator, path: []const u8) ?[]u8 {
    if (builtin.os.tag == .windows) return finalOnWindows(allocator, path);
    // libgit2 has already resolved it.
    return allocator.dupe(u8, path) catch null;
}

test "final path names lose their marker and backslashes" {
    const cases = [_][2][]const u8{
        .{ "\\\\?\\C:\\work\\repo", "C:/work/repo" },
        .{ "\\\\?\\C:\\", "C:/" },
        .{ "\\\\?\\UNC\\server\\share\\repo", "//server/share/repo" },
        .{ "C:\\work", "C:/work" },
    };
    for (cases) |case| {
        const path = try std.testing.allocator.dupe(u8, case[0]);
        defer std.testing.allocator.free(path);
        try std.testing.expectEqualStrings(case[1], clean(path));
    }
}

test "a directory on Windows has a final path" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;
    const path = final(std.testing.allocator, "c:/windows/").?;
    defer std.testing.allocator.free(path);
    try std.testing.expect(std.ascii.eqlIgnoreCase("c:/windows", path));
}

test "a missing directory has no final path" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;
    try std.testing.expect(final(std.testing.allocator, "c:/no/such/directory") == null);
}
