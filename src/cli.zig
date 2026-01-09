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
    machine_verbose,
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
    diagnostic_sources: DiagnosticSources,
    compiler_warnings: CompilerWarnings,
};

pub const DiagnosticSources = struct {
    js: bool = true,
    svelte: bool = true,
    css: bool = true,
};

pub const WarningBehavior = enum {
    ignore,
    @"error",
};

pub const CompilerWarnings = std.StringHashMapUnmanaged(WarningBehavior);

pub fn parseArgs(allocator: std.mem.Allocator) !Args {
    var args_iter = try std.process.argsWithAllocator(allocator);
    defer args_iter.deinit();
    return parseArgsFromIterator(allocator, &args_iter);
}

/// Parse arguments from any iterator (used by tests)
pub fn parseArgsFromIterator(allocator: std.mem.Allocator, args_iter: anytype) !Args {
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
        .diagnostic_sources = .{},
        .compiler_warnings = .empty,
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
        } else if (std.mem.eql(u8, arg, "--compiler-warnings")) {
            const warnings = args_iter.next() orelse return error.MissingArgValue;
            try parseCompilerWarnings(allocator, warnings, &result.compiler_warnings);
        } else if (std.mem.eql(u8, arg, "--watch") or std.mem.eql(u8, arg, "-w")) {
            return error.WatchModeNotSupported;
        } else if (std.mem.eql(u8, arg, "--preserveWatchOutput")) {
            return error.WatchModeNotSupported;
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
    if (std.mem.eql(u8, fmt, "machine-verbose")) return .machine_verbose;

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

fn parseCompilerWarnings(
    allocator: std.mem.Allocator,
    input: []const u8,
    map: *CompilerWarnings,
) !void {
    var it = std.mem.splitScalar(u8, input, ',');
    while (it.next()) |pair| {
        const trimmed = std.mem.trim(u8, pair, " \t");
        if (trimmed.len == 0) continue;

        if (std.mem.indexOfScalar(u8, trimmed, ':')) |colon| {
            const code = trimmed[0..colon];
            const behavior_str = trimmed[colon + 1 ..];
            const behavior = std.meta.stringToEnum(WarningBehavior, behavior_str) orelse
                return error.InvalidWarningBehavior;
            try map.put(allocator, code, behavior);
        } else {
            return error.InvalidWarningFormat;
        }
    }
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
        \\    --compiler-warnings      Code:behavior pairs (code:ignore, code:error)
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
    try std.testing.expectEqual(.machine_verbose, parseOutputFormat("machine-verbose"));

    // Invalid
    try std.testing.expectEqual(null, parseOutputFormat("invalid"));
}

test "parseCompilerWarnings parses code:behavior pairs" {
    const allocator = std.testing.allocator;

    var map: CompilerWarnings = .empty;
    defer map.deinit(allocator);

    try parseCompilerWarnings(allocator, "a11y-click-events-have-key-events:ignore", &map);
    try std.testing.expectEqual(.ignore, map.get("a11y-click-events-have-key-events").?);

    try parseCompilerWarnings(allocator, "css-unused-selector:error", &map);
    try std.testing.expectEqual(.@"error", map.get("css-unused-selector").?);
}

test "parseCompilerWarnings handles multiple pairs" {
    const allocator = std.testing.allocator;

    var map: CompilerWarnings = .empty;
    defer map.deinit(allocator);

    try parseCompilerWarnings(allocator, "a11y-foo:ignore,a11y-bar:error", &map);
    try std.testing.expectEqual(.ignore, map.get("a11y-foo").?);
    try std.testing.expectEqual(.@"error", map.get("a11y-bar").?);
}

test "parseCompilerWarnings rejects invalid format" {
    const allocator = std.testing.allocator;

    var map: CompilerWarnings = .empty;
    defer map.deinit(allocator);

    try std.testing.expectError(error.InvalidWarningFormat, parseCompilerWarnings(allocator, "no-colon", &map));
    try std.testing.expectError(error.InvalidWarningBehavior, parseCompilerWarnings(allocator, "code:badvalue", &map));
}

const TestArgIterator = struct {
    args: []const []const u8,
    index: usize = 0,

    fn next(self: *@This()) ?[]const u8 {
        if (self.index >= self.args.len) return null;
        defer self.index += 1;
        return self.args[self.index];
    }
};

test "watch mode flags return WatchModeNotSupported error" {
    const allocator = std.testing.allocator;

    var watch_iter: TestArgIterator = .{ .args = &.{ "prog", "--watch" } };
    try std.testing.expectError(error.WatchModeNotSupported, parseArgsFromIterator(allocator, &watch_iter));

    var short_iter: TestArgIterator = .{ .args = &.{ "prog", "-w" } };
    try std.testing.expectError(error.WatchModeNotSupported, parseArgsFromIterator(allocator, &short_iter));

    var preserve_iter: TestArgIterator = .{ .args = &.{ "prog", "--preserveWatchOutput" } };
    try std.testing.expectError(error.WatchModeNotSupported, parseArgsFromIterator(allocator, &preserve_iter));
}
