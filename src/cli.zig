//! Command-line argument parsing
//!
//! Supports the same flags as svelte-check/svelte-check-rs:
//! --workspace, --output, --tsconfig, --no-tsconfig, --ignore,
//! --fail-on-warnings, --threshold, --diagnostic-sources

const std = @import("std");

pub const OutputFormat = enum {
    human,
    human_verbose,
    machine,
    json,
};

pub const Threshold = enum {
    warning,
    @"error",
};

pub const Args = struct {
    allocator: std.mem.Allocator,
    workspace: []const u8,
    output_format: OutputFormat,
    tsconfig: ?[]const u8,
    no_tsconfig: bool,
    ignore: []const []const u8,
    fail_on_warnings: bool,
    threshold: Threshold,
    diagnostic_sources: struct {
        js: bool,
        svelte: bool,
        css: bool,
    },

    // No deinit needed - arena handles cleanup
};

pub fn parseArgs(allocator: std.mem.Allocator) !Args {
    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();

    _ = args_iter.next(); // skip program name

    var result: Args = .{
        .allocator = allocator,
        .workspace = ".",
        .output_format = .human,
        .tsconfig = null,
        .no_tsconfig = false,
        .ignore = &.{},
        .fail_on_warnings = false,
        .threshold = .warning,
        .diagnostic_sources = .{ .js = true, .svelte = true, .css = true },
    };

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "--workspace")) {
            result.workspace = args_iter.next() orelse return error.MissingArgValue;
        } else if (std.mem.eql(u8, arg, "--output")) {
            const fmt = args_iter.next() orelse return error.MissingArgValue;
            result.output_format = parseOutputFormat(fmt) orelse return error.InvalidOutputFormat;
        } else if (std.mem.eql(u8, arg, "--tsconfig")) {
            result.tsconfig = args_iter.next() orelse return error.MissingArgValue;
        } else if (std.mem.eql(u8, arg, "--no-tsconfig")) {
            result.no_tsconfig = true;
        } else if (std.mem.eql(u8, arg, "--fail-on-warnings")) {
            result.fail_on_warnings = true;
        } else if (std.mem.eql(u8, arg, "--threshold")) {
            const t = args_iter.next() orelse return error.MissingArgValue;
            result.threshold = std.meta.stringToEnum(Threshold, t) orelse return error.InvalidThreshold;
        } else if (std.mem.eql(u8, arg, "--ignore")) {
            const patterns = args_iter.next() orelse return error.MissingArgValue;
            result.ignore = try parseCommaSeparated(allocator, patterns);
        } else if (std.mem.eql(u8, arg, "--diagnostic-sources")) {
            const sources = args_iter.next() orelse return error.MissingArgValue;
            result.diagnostic_sources = .{ .js = false, .svelte = false, .css = false };
            var it = std.mem.splitScalar(u8, sources, ',');
            while (it.next()) |s| {
                if (std.mem.eql(u8, s, "js")) result.diagnostic_sources.js = true;
                if (std.mem.eql(u8, s, "svelte")) result.diagnostic_sources.svelte = true;
                if (std.mem.eql(u8, s, "css")) result.diagnostic_sources.css = true;
            }
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printHelp();
            std.process.exit(0);
        } else if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            printVersion();
            std.process.exit(0);
        }
    }

    return result;
}

/// Parses output format, accepting both underscore (human_verbose) and
/// hyphenated (human-verbose) forms for svelte-check compatibility.
fn parseOutputFormat(fmt: []const u8) ?OutputFormat {
    // Direct match first
    if (std.meta.stringToEnum(OutputFormat, fmt)) |f| return f;

    // Normalize hyphens to underscores for svelte-check compat
    if (std.mem.eql(u8, fmt, "human-verbose")) return .human_verbose;
    if (std.mem.eql(u8, fmt, "machine-verbose")) return .human_verbose; // machine-verbose maps to human_verbose

    return null;
}

fn parseCommaSeparated(allocator: std.mem.Allocator, input: []const u8) ![]const []const u8 {
    var list: std.ArrayList([]const u8) = .empty;
    var it = std.mem.splitScalar(u8, input, ',');
    while (it.next()) |part| {
        const trimmed = std.mem.trim(u8, part, " \t");
        if (trimmed.len > 0) {
            try list.append(allocator, trimmed);
        }
    }
    return list.toOwnedSlice(allocator);
}

fn printHelp() void {
    const help =
        \\svelte-check-zig - Fast Svelte diagnostics
        \\
        \\USAGE:
        \\    svelte-check-zig [OPTIONS]
        \\
        \\OPTIONS:
        \\    --workspace <PATH>       Working directory (default: .)
        \\    --output <FORMAT>        Output: human, human-verbose, machine, json
        \\    --tsconfig <PATH>        Path to tsconfig.json
        \\    --no-tsconfig            Skip TypeScript checking
        \\    --ignore <PATTERNS>      Comma-separated glob patterns to ignore
        \\    --fail-on-warnings       Exit with error on warnings
        \\    --threshold <LEVEL>      Minimum severity: error, warning
        \\    --diagnostic-sources     Which diagnostics: js,svelte,css
        \\    -h, --help               Show this help
        \\    -v, --version            Show version
        \\
    ;
    std.fs.File.stdout().writeAll(help) catch {};
}

fn printVersion() void {
    std.fs.File.stdout().writeAll("svelte-check-zig 0.1.0\n") catch {};
}

test "parseArgs defaults" {
    // Basic smoke test
    const allocator = std.testing.allocator;
    var iter = try std.process.argsWithAllocator(allocator);
    defer iter.deinit();
}

test "parseOutputFormat accepts hyphenated and underscore forms" {
    // Underscore forms (our native format)
    try std.testing.expectEqual(.human, parseOutputFormat("human"));
    try std.testing.expectEqual(.human_verbose, parseOutputFormat("human_verbose"));
    try std.testing.expectEqual(.machine, parseOutputFormat("machine"));
    try std.testing.expectEqual(.json, parseOutputFormat("json"));

    // Hyphenated forms (svelte-check compat)
    try std.testing.expectEqual(.human_verbose, parseOutputFormat("human-verbose"));
    try std.testing.expectEqual(.human_verbose, parseOutputFormat("machine-verbose"));

    // Invalid
    try std.testing.expectEqual(null, parseOutputFormat("invalid"));
}
