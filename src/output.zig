//! Output formatting - human, machine, and JSON formats

const std = @import("std");
const Diagnostic = @import("diagnostic.zig").Diagnostic;
const cli = @import("cli.zig");

pub fn print(
    allocator: std.mem.Allocator,
    format: cli.OutputFormat,
    diagnostics: []const Diagnostic,
    threshold: cli.Threshold,
    compiler_warnings: cli.CompilerWarnings,
) !void {
    const stdout = std.fs.File.stdout();

    // Apply compiler warnings and filter by threshold
    var filtered: std.ArrayList(Diagnostic) = .empty;
    defer filtered.deinit(allocator);

    for (diagnostics) |d| {
        var diag = d;

        // Apply compiler warning behaviors
        if (d.code) |code| {
            if (compiler_warnings.get(code)) |behavior| {
                switch (behavior) {
                    .ignore => continue,
                    .@"error" => diag.severity = .@"error",
                }
            }
        }

        // Filter by threshold
        const include = switch (threshold) {
            .@"error" => diag.severity == .@"error",
            .warning => true,
        };
        if (include) try filtered.append(allocator, diag);
    }

    switch (format) {
        .human => try printHuman(stdout, filtered.items),
        .human_verbose => try printHumanVerbose(stdout, filtered.items),
        .machine => try printMachine(stdout, filtered.items),
        .machine_verbose => try printMachineVerbose(stdout, filtered.items),
        .json => try printJson(stdout, filtered.items),
    }

    // Print summary
    var errors: usize = 0;
    var warnings: usize = 0;
    for (filtered.items) |d| {
        switch (d.severity) {
            .@"error" => errors += 1,
            .warning => warnings += 1,
        }
    }

    var buf: [256]u8 = undefined;
    const summary = std.fmt.bufPrint(&buf, "\n{d} error(s), {d} warning(s)\n", .{ errors, warnings }) catch return;
    try stdout.writeAll(summary);
}

fn printHuman(file: std.fs.File, diagnostics: []const Diagnostic) !void {
    var buf: [1024]u8 = undefined;
    for (diagnostics) |d| {
        const line = std.fmt.bufPrint(&buf, "{s}:{d}:{d} - {s}: {s}\n", .{
            d.file_path,
            d.start_line,
            d.start_col,
            @tagName(d.severity),
            d.message,
        }) catch continue;
        try file.writeAll(line);
    }
}

fn printHumanVerbose(file: std.fs.File, diagnostics: []const Diagnostic) !void {
    var buf: [2048]u8 = undefined;
    for (diagnostics) |d| {
        const line = std.fmt.bufPrint(&buf, "{s}:{d}:{d} - {d}:{d}\n  {s} [{s}]: {s}\n\n", .{
            d.file_path,
            d.start_line,
            d.start_col,
            d.end_line,
            d.end_col,
            @tagName(d.severity),
            @tagName(d.source),
            d.message,
        }) catch continue;
        try file.writeAll(line);
    }
}

fn printMachine(file: std.fs.File, diagnostics: []const Diagnostic) !void {
    const timestamp = std.time.milliTimestamp();
    var buf: [1024]u8 = undefined;

    const start = std.fmt.bufPrint(&buf, "{d} START \".\"\n", .{timestamp}) catch return;
    try file.writeAll(start);

    for (diagnostics) |d| {
        const severity_str = switch (d.severity) {
            .@"error" => "ERROR",
            .warning => "WARNING",
        };
        const line = std.fmt.bufPrint(&buf, "{d} {s} \"{s}\" {d}:{d} \"{s}\"\n", .{
            timestamp,
            severity_str,
            d.file_path,
            d.start_line,
            d.start_col,
            d.message,
        }) catch continue;
        try file.writeAll(line);
    }

    var errors: usize = 0;
    var warnings: usize = 0;
    for (diagnostics) |d| {
        switch (d.severity) {
            .@"error" => errors += 1,
            .warning => warnings += 1,
        }
    }
    const files_count = diagnostics.len;

    const completed = std.fmt.bufPrint(&buf, "{d} COMPLETED 0 FILES {d} ERRORS {d} WARNINGS {d} FILES_WITH_PROBLEMS\n", .{
        timestamp,
        errors,
        warnings,
        files_count,
    }) catch return;
    try file.writeAll(completed);
}

/// NDJSON format compatible with svelte-check's machine-verbose output.
/// Each line is a JSON object with: timestamp, filename, start/end positions,
/// message, code, hint, and source.
fn printMachineVerbose(file: std.fs.File, diagnostics: []const Diagnostic) !void {
    const timestamp = std.time.milliTimestamp();
    var buf: [2048]u8 = undefined;

    for (diagnostics) |d| {
        const code_str = d.code orelse "";
        const hint = codeToHint(d.code);
        const source_str = switch (d.source) {
            .js => "js",
            .svelte => "svelte",
            .css => "css",
        };

        const line = std.fmt.bufPrint(&buf,
            \\{{"timestamp":{d},"filename":"{s}","start":{{"line":{d},"column":{d}}},"end":{{"line":{d},"column":{d}}},"message":"{s}","code":"{s}","hint":"{s}","source":"{s}"}}
            \\
        , .{
            timestamp,
            d.file_path,
            d.start_line,
            d.start_col,
            d.end_line,
            d.end_col,
            d.message, // TODO: escape JSON special chars
            code_str,
            hint,
            source_str,
        }) catch continue;
        try file.writeAll(line);
    }
}

/// Map diagnostic codes to human-friendly hints
fn codeToHint(code: ?[]const u8) []const u8 {
    const c = code orelse return "";
    if (std.mem.startsWith(u8, c, "a11y-")) return "Accessibility";
    if (std.mem.startsWith(u8, c, "css-")) return "CSS";
    if (std.mem.startsWith(u8, c, "ts")) return "TypeScript";
    return "";
}

fn printJson(file: std.fs.File, diagnostics: []const Diagnostic) !void {
    var buf: [2048]u8 = undefined;

    try file.writeAll("[\n");
    for (diagnostics, 0..) |d, i| {
        const entry = std.fmt.bufPrint(&buf,
            \\  {{
            \\    "file": "{s}",
            \\    "severity": "{s}",
            \\    "source": "{s}",
            \\    "message": "{s}",
            \\    "start": {{ "line": {d}, "column": {d} }},
            \\    "end": {{ "line": {d}, "column": {d} }}
            \\  }}{s}
            \\
        , .{
            d.file_path,
            @tagName(d.severity),
            @tagName(d.source),
            d.message, // TODO: escape JSON
            d.start_line,
            d.start_col,
            d.end_line,
            d.end_col,
            if (i < diagnostics.len - 1) "," else "",
        }) catch continue;
        try file.writeAll(entry);
    }
    try file.writeAll("]\n");
}

pub fn printNoFiles(format: cli.OutputFormat) !void {
    const stdout = std.fs.File.stdout();
    var buf: [256]u8 = undefined;

    switch (format) {
        .human, .human_verbose => try stdout.writeAll("No Svelte files found.\n"),
        .machine => {
            const timestamp = std.time.milliTimestamp();
            const start = std.fmt.bufPrint(&buf, "{d} START \".\"\n", .{timestamp}) catch return;
            try stdout.writeAll(start);
            const completed = std.fmt.bufPrint(&buf, "{d} COMPLETED 0 FILES 0 ERRORS 0 WARNINGS 0 FILES_WITH_PROBLEMS\n", .{timestamp}) catch return;
            try stdout.writeAll(completed);
        },
        .machine_verbose => {}, // NDJSON outputs nothing when no files
        .json => try stdout.writeAll("[]\n"),
    }
}

test "output formats compile" {
    // Smoke test that format functions compile
    const diags = [_]Diagnostic{};
    _ = diags;
}
