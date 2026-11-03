//! Static libgit2 without network support: no HTTPS, no SSH, no NTLM,
//! bundled zlib, builtin PCRE2 regex, builtin SHA-256.
//!
//! Adapted from the build.zig of allyourcodebase/libgit2, commit 71b31da
//! (2026-10-26, Zig 0.17.0):
//!
//!   Copyright 2025 Julian Del Signore
//!
//!   Permission is hereby granted, free of charge, to any person obtaining
//!   a copy of this software and associated documentation files (the
//!   "Software"), to deal in the Software without restriction, including
//!   without limitation the rights to use, copy, modify, merge, publish,
//!   distribute, sublicense, and/or sell copies of the Software, and to
//!   permit persons to whom the Software is furnished to do so, subject to
//!   the following conditions:
//!
//!   The above copyright notice and this permission notice shall be
//!   included in all copies or substantial portions of the Software.
//!
//!   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
//!
//! What differs from it: the TLS backends (mbedTLS, OpenSSL,
//! SecureTransport, WinHTTP), libssh2 and ntlmclient are gone, and with
//! them the dependencies other than the libgit2 tarball itself; zlib and
//! SHA-256 come from libgit2's own sources; the regex backend is the PCRE2
//! that libgit2 1.9.x bundles instead of PCRE; the CLI, the examples and
//! the clar test suite are not built.

const std = @import("std");
const sources = @import("libgit2-sources.zon");

pub fn build(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.Optimize,
    src: *std.Build.Dependency,
) *std.Build.Step.Compile {
    const os = target.result.os.tag;

    const mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .pic = true,
    });
    const lib = b.addLibrary(.{
        .name = "git2",
        .linkage = .static,
        .root_module = mod,
    });

    const features = b.addConfigHeader(
        .{ .style = .{ .cmake = src.path("src/util/git2_features.h.in") } },
        .{
            .GIT_THREADS = 1,
            .GIT_USE_NSEC = 1,
            .GIT_SHA1_COLLISIONDETECT = 1,
            .GIT_SHA256_BUILTIN = 1,
            .GIT_COMPRESSION_BUILTIN = 1,
            .GIT_REGEX_BUILTIN = 1,
            .GIT_HTTPPARSER_BUILTIN = 1,
        },
    );
    switch (target.result.ptrBitWidth()) {
        32 => features.addValues(.{ .GIT_ARCH_32 = 1 }),
        64 => features.addValues(.{ .GIT_ARCH_64 = 1 }),
        else => |size| std.debug.panic("Unsupported architecture ({d}bit)", .{size}),
    }
    if (os == .windows) {
        features.addValues(.{
            .GIT_QSORT_MSC = 1,
            .GIT_IO_WSAPOLL = 1,
        });
    } else {
        features.addValues(.{
            .GIT_USE_STAT_MTIM = 1,
            .GIT_USE_FUTIMENS = 1,
            .GIT_RAND_GETENTROPY = 1,
            .GIT_RAND_GETLOADAVG = 1,
            .GIT_IO_POLL = 1,
            .GIT_IO_SELECT = 1,
        });
    }

    const flags: []const []const u8 = if (os == .windows) &.{
        "-DHAVE_CONFIG_H",
        "-DWIN32",
        "-D_WIN32_WINNT=0x0600",
        "-D__USE_MINGW_ANSI_STDIO=1",
        "-fno-sanitize=undefined",
    } else &.{
        "-DHAVE_CONFIG_H",
        "-fno-sanitize=undefined",
    };

    if (os == .windows) {
        mod.linkSystemLibrary("ws2_32", .{});
        mod.linkSystemLibrary("advapi32", .{});
        mod.linkSystemLibrary("rpcrt4", .{});
        mod.linkSystemLibrary("ole32", .{});
        // SSPI authentication (auth_sspi.c) is built even without HTTPS.
        mod.linkSystemLibrary("secur32", .{});
        mod.addCSourceFiles(.{
            .root = src.path("src/util"),
            .files = &sources.util_win32,
            .flags = flags,
        });
    } else {
        mod.addCSourceFiles(.{
            .root = src.path("src/util"),
            .files = &sources.util_unix,
            .flags = flags,
        });
    }

    // SHA-1 with collision detection
    mod.addCSourceFiles(.{
        .root = src.path("src/util"),
        .files = &sources.util_sha1dc,
        .flags = &.{
            "-DHAVE_CONFIG_H",
            "-fno-sanitize=undefined",
            "-DSHA1DC_NO_STANDARD_INCLUDES",
            "-DSHA1DC_CUSTOM_INCLUDE_SHA1_C=\"git2_util.h\"",
            "-DSHA1DC_CUSTOM_INCLUDE_UBC_CHECK_C=\"git2_util.h\"",
        },
    });

    // The bundled dependencies are separate libraries, compiled with their
    // own include path only: on Windows zlib includes the system
    // <process.h>, which libgit2's src/util would shadow.

    // deps/llhttp
    {
        const llhttp = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .pic = true,
        });
        llhttp.addIncludePath(src.path("deps/llhttp"));
        llhttp.addCSourceFiles(.{
            .root = src.path("deps/llhttp"),
            .files = &sources.llhttp,
            .flags = &.{ "-Wno-unused-parameter", "-Wno-missing-declarations" },
        });
        mod.linkLibrary(b.addLibrary(.{
            .name = "llhttp",
            .linkage = .static,
            .root_module = llhttp,
        }));
        mod.addIncludePath(src.path("deps/llhttp"));
    }

    // deps/pcre2
    {
        const pcre2_config = b.addConfigHeader(
            .{
                .style = .{ .cmake = src.path("deps/pcre2/config.h.in") },
                .include_path = "config.h",
            },
            .{
                .HAVE_ASSERT_H = 1,
                .HAVE_BUILTIN_MUL_OVERFLOW = 1,
                .HAVE_BUILTIN_UNREACHABLE = 1,
                .HAVE_ATTRIBUTE_UNINITIALIZED = 1,
                .HAVE_SYS_STAT_H = 1,
                .HAVE_SYS_TYPES_H = 1,
                .HAVE_DIRENT_H = os != .windows,
                .HAVE_UNISTD_H = os != .windows,
                .HAVE_WINDOWS_H = os == .windows,
                .SUPPORT_PCRE2_8 = 1,
                .SUPPORT_UNICODE = 1,
                .PCRE2_LINK_SIZE = 2,
                .PCRE2_HEAP_LIMIT = 20000000,
                .PCRE2_MATCH_LIMIT = 10000000,
                .PCRE2_MATCH_LIMIT_DEPTH = "MATCH_LIMIT",
                .PCRE2_MAX_VARLOOKBEHIND = 255,
                .NEWLINE_DEFAULT = 2,
                .PCRE2_PARENS_NEST_LIMIT = 250,
                .PCRE2GREP_BUFSIZE = 20480,
                .PCRE2GREP_MAX_BUFSIZE = 1048576,
            },
        );
        const pcre2 = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .pic = true,
        });
        pcre2.addConfigHeader(pcre2_config);
        pcre2.addIncludePath(src.path("deps/pcre2"));
        pcre2.addCSourceFiles(.{
            .root = src.path("deps/pcre2"),
            .files = &sources.pcre2,
            .flags = &.{
                "-DHAVE_CONFIG_H",
                "-DPCRE2_CODE_UNIT_WIDTH=8",
                "-DPCRE2_STATIC",
                "-DPCRE2_EXPORT=",
                "-DPCRE2_EXP_DECL=extern",
                "-DPCRE2_EXP_DEFN=",
                "-Wno-unused-function",
                "-Wno-implicit-fallthrough",
            },
        });
        mod.linkLibrary(b.addLibrary(.{
            .name = "pcre2",
            .linkage = .static,
            .root_module = pcre2,
        }));
        mod.addIncludePath(src.path("deps/pcre2"));
        mod.addCMacro("PCRE2_STATIC", "1");
        mod.addCMacro("PCRE2_EXPORT", "");
        mod.addCMacro("PCRE2_EXP_DECL", "");
        mod.addCMacro("PCRE2_EXP_DEFN", "");
    }

    // deps/zlib
    {
        const zlib = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
            .pic = true,
        });
        zlib.addIncludePath(src.path("deps/zlib"));
        zlib.addCSourceFiles(.{
            .root = src.path("deps/zlib"),
            .files = &sources.zlib,
            .flags = if (os == .windows) &.{
                "-DNO_VIZ",                "-DSTDC",
                "-DNO_GZIP",               "-DHAVE_SYS_TYPES_H",
                "-DHAVE_STDINT_H",         "-DHAVE_STDDEF_H",
                "-DZ_HAVE_UNISTD_H",       "-D_LFS64_LARGEFILE",
                "-D_LARGEFILE64_SOURCE=1", "-Wno-implicit-fallthrough",
            } else &.{
                "-DNO_VIZ",                  "-DSTDC",
                "-DNO_GZIP",                 "-DHAVE_SYS_TYPES_H",
                "-DHAVE_STDINT_H",           "-DHAVE_STDDEF_H",
                "-Wno-implicit-fallthrough",
            },
        });
        mod.linkLibrary(b.addLibrary(.{
            .name = "z",
            .linkage = .static,
            .root_module = zlib,
        }));
        mod.addIncludePath(src.path("deps/zlib"));
    }

    // deps/xdiff relies on libgit2's headers, so it is compiled into the
    // library directly.
    mod.addCSourceFiles(.{
        .root = src.path("deps/xdiff"),
        .files = &sources.xdiff,
        .flags = &.{ "-Wno-sign-compare", "-Wno-unused-parameter" },
    });
    mod.addIncludePath(src.path("deps/xdiff"));

    mod.addConfigHeader(features);
    mod.addIncludePath(src.path("src/libgit2"));
    mod.addIncludePath(src.path("src/util"));
    mod.addIncludePath(src.path("include"));
    mod.addCSourceFiles(.{
        .root = src.path("src/libgit2"),
        .files = &sources.libgit2,
        .flags = flags,
    });
    mod.addCSourceFiles(.{
        .root = src.path("src/util"),
        .files = &sources.util,
        .flags = flags,
    });

    return lib;
}
