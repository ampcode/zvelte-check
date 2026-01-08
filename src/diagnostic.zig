//! Diagnostic types shared across all modules

const std = @import("std");

pub const Severity = enum {
    warning,
    @"error",

    pub fn format(self: Severity, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(switch (self) {
            .warning => "warning",
            .@"error" => "error",
        });
    }
};

pub const Source = enum {
    js,
    svelte,
    css,

    pub fn format(self: Source, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(switch (self) {
            .js => "js",
            .svelte => "svelte",
            .css => "css",
        });
    }
};

pub const Diagnostic = struct {
    source: Source,
    severity: Severity,
    code: ?[]const u8,
    message: []const u8,
    file_path: []const u8,
    start_line: u32,
    start_col: u32,
    end_line: u32,
    end_col: u32,
};
