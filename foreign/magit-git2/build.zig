// SPDX-License-Identifier: GPL-2.0-or-later
// Copyright (C) 2026 Kostafey <kostafey@gmail.com>

//! Builds magit-git2-core, the Emacs dynamic module.
//!
//!   zig build                               vendored libgit2, host target
//!   zig build -Dsystem-libgit2=true         system libgit2 via pkg-config
//!   zig build -Dtarget=x86_64-windows-gnu   the Windows .dll
//!   zig build test                          unit tests of the Zig helpers
//!
//! The module lands in zig-out/lib as magit-git2-core.so (.dll on
//! Windows): the file name `module-file-suffix' expects, without the
//! "lib" prefix.

const std = @import("std");
const libgit2 = @import("build/libgit2.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    // "safe" (ReleaseSafe before Zig 0.17) unless asked otherwise: a
    // module crash is an Emacs crash, so the bounds checks stay.
    const optimize = b.option(
        std.builtin.OptimizeMode,
        "optimize",
        "Optimization mode (default: safe)",
    ) orelse .safe;
    const system_libgit2 = b.option(
        bool,
        "system-libgit2",
        "Link the system libgit2 (pkg-config) instead of building it",
    ) orelse false;

    // C headers, translated: emacs-module.h as vendored, git2.h from
    // whichever libgit2 is linked.
    const emacs_c = b.addTranslateC(.{
        .root_source_file = b.path("vendor/emacs-module.h"),
        .target = target,
        .optimize = optimize,
    });
    const git2_c = if (system_libgit2) blk: {
        // Any header of the installed libgit2 serves as root: the
        // translation must find git2.h through pkg-config's include path.
        const t = b.addTranslateC(.{
            .root_source_file = b.addWriteFiles().add(
                "git2_include.h",
                "#include <git2.h>\n",
            ),
            .target = target,
            .optimize = optimize,
        });
        t.linkSystemLibrary("libgit2", .{ .use_pkg_config = .force });
        break :blk t;
    } else blk: {
        const src = b.lazyDependency("libgit2", .{}) orelse return;
        const t = b.addTranslateC(.{
            .root_source_file = src.path("include/git2.h"),
            .target = target,
            .optimize = optimize,
        });
        t.addIncludePath(src.path("include"));
        break :blk t;
    };

    const mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .pic = true,
        .imports = &.{
            .{ .name = "emacs_c", .module = emacs_c.createModule() },
            .{ .name = "git2_c", .module = git2_c.createModule() },
        },
    });
    if (system_libgit2) {
        mod.linkSystemLibrary("libgit2", .{ .use_pkg_config = .force });
    } else if (b.lazyDependency("libgit2", .{})) |src| {
        mod.linkLibrary(libgit2.build(b, target, optimize, src));
    }

    const lib = b.addLibrary(.{
        .name = "magit-git2-core",
        .linkage = .dynamic,
        .root_module = mod,
    });
    if (target.result.ofmt == .elf) {
        // Export the module entry points only.  The static libgit2 would
        // otherwise export its whole API, which Emacs, loading modules
        // into the global namespace, would offer to every library loaded
        // after it -- another module linking a different libgit2.
        lib.setVersionScript(b.addWriteFiles().add("exports.map",
            \\{
            \\  global: emacs_module_init; plugin_is_GPL_compatible;
            \\  local: *;
            \\};
            \\
        ));
    }
    const suffix = if (target.result.os.tag == .windows) ".dll" else ".so";
    const install = b.addInstallFileWithDir(
        lib.getEmittedBin(),
        .lib,
        b.fmt("magit-git2-core{s}", .{suffix}),
    );
    b.getInstallStep().dependOn(&install.step);

    // The libgit2 wrappers are covered by the ERT suite; these are the
    // parts testable without Emacs and a repository.
    const test_step = b.step("test", "Run the unit tests of the Zig helpers");
    for ([_][]const u8{ "src/emacs.zig", "src/walk.zig", "src/git.zig" }) |file| {
        const tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path(file),
                .target = target,
                .optimize = optimize,
                .link_libc = true,
                .imports = &.{
                    .{ .name = "emacs_c", .module = emacs_c.createModule() },
                    .{ .name = "git2_c", .module = git2_c.createModule() },
                },
            }),
        });
        test_step.dependOn(&b.addRunArtifact(tests).step);
    }
}
