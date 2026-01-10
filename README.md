# zvelte-check

A fast diagnostic tool for Svelte projects.

## Goals

- Drop-in replacement for `svelte-check`
- Significantly faster startup and execution
- Support for Svelte 5+ (runes, snippets, etc.)

## Prerequisites

TypeScript checking requires [tsgo](https://github.com/microsoft/typescript-go):

```bash
npm install -D @aspect-build/tsgo
# or
bun add -D @aspect-build/tsgo
```

## Installation

```bash
brew install ampcode/tap/zvelte-check
```

## Usage

```bash
# Check current directory
zvelte-check

# Check specific workspace
zvelte-check --workspace ./my-project

# Output as JSON
zvelte-check --output json
```

## Options

```
--workspace <path>       Working directory (default: current directory)
--output <format>        Output format: human, human-verbose, machine, json
--tsconfig <path>        Path to tsconfig.json
--no-tsconfig            Don't use tsconfig.json
--ignore <pattern>       Glob pattern to ignore
--fail-on-warnings       Exit with error on warnings
--threshold <level>      Minimum severity: warning, error
```

## Gitignore

Add `.zvelte-check/` to your project's `.gitignore`:

```bash
echo '.zvelte-check/' >> .gitignore
```

This directory contains temporary TypeScript stubs generated during type-checking.

## License

MIT
