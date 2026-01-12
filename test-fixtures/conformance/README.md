# Conformance Tests

This directory contains test samples from [sveltejs/language-tools](https://github.com/sveltejs/language-tools) used for conformance testing between `zvelte-check` and `svelte-check`.

## Purpose

These tests verify that `zvelte-check` produces the same diagnostics as `svelte-check` for a variety of Svelte code patterns. The goal is **diagnostic parity**, not transformer output comparison — `zvelte-check` may generate different but equivalent TypeScript output.

## Test Samples

Samples are downloaded from `packages/svelte2tsx/test/svelte2tsx/samples/` in the sveltejs/language-tools repository. Each sample contains an `input.svelte` file.

By default, only `.v5` samples (Svelte 5) are tested since `zvelte-check` only supports Svelte 5+.

## Running Tests

```bash
# Sync samples from sveltejs/language-tools (first time only)
./scripts/sync-svelte2tsx-samples.sh

# Install dependencies
cd test-fixtures/conformance && pnpm install

# Run conformance tests (Svelte 5 samples only)
./scripts/conformance-test.sh

# Run with summary only
./scripts/conformance-test.sh --summary

# Run a specific sample
./scripts/conformance-test.sh --sample runes.v5 --verbose

# Run all samples including non-v5
./scripts/conformance-test.sh --all --summary

# Limit to first N samples
./scripts/conformance-test.sh --limit 10
```

## Options

| Option | Description |
|--------|-------------|
| `--sample NAME` | Run only the specified sample |
| `--verbose` | Show all output, not just failures |
| `--summary` | Show only summary statistics |
| `--list` | List all available samples |
| `--all` | Run all samples, not just `.v5` samples |
| `--limit N` | Run only the first N samples |
| `--timeout N` | Timeout per sample in seconds (default: 30) |

## Expected Differences

Some differences between `zvelte-check` and `svelte-check` are expected:

1. **Top-level await**: tsgo/tsc may report different errors about module options
2. **Slot deprecation warnings**: svelte-check warns about deprecated `<slot>`, zvelte-check may not
3. **Unused export warnings**: svelte-check reports these, zvelte-check may skip them
4. **Parsing errors for TS syntax in JS**: svelte-check uses Svelte's parser which emits "Unexpected token" errors when encountering TypeScript syntax (like `:` for type annotations) in non-lang=ts scripts. zvelte-check passes the transformed code directly to tsgo which reports semantic errors like "Type annotations can only be used in TypeScript files". Both tools correctly identify the issue, just with different error messages.
5. **Enum in module script without lang="ts"**: svelte-check warns "enums are not natively supported" when `enum` is used in a `<script module>` without `lang="ts"`. zvelte-check lets TypeScript report the actual type errors (e.g., enum/interface declaration merging conflicts). Both approaches catch the problematic code.

## License

The test samples are from [sveltejs/language-tools](https://github.com/sveltejs/language-tools) which is MIT licensed. See the original repository for the full license.
