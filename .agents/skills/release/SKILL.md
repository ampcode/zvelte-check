---
name: release
description: Publishes zvelte-check releases. Use when releasing, versioning, tagging, or publishing a new version.
---

# Releasing zvelte-check

## Quick Release Checklist

1. Update version in `build.zig.zon`
2. Write release notes in `docs/releases/vX.Y.Z.md`
3. Commit changes
4. Tag and push: `git tag vX.Y.Z && git push && git push --tags`

## Version Handling

The version is derived automatically from git tags at build time using `git describe`. No manual version updates needed.

- `build.zig` - `getVersion()` runs `git describe --match "v*.*.*" --tags`
- `src/cli.zig` - imports `build_options.version` for `--version` output

Version format examples:
- On a tag `v0.1.0` → `0.1.0`
- 3 commits after tag → `0.1.0-3+abc123`
- No tags → `unknown`

## Release Notes

Create a markdown file at `docs/releases/vX.Y.Z.md` (must match the tag name exactly).

Example for `docs/releases/v0.1.0.md`:

```markdown
## What's New

- Initial release of zvelte-check
- Support for Svelte 5 runes and snippets
- A11y, CSS, and TypeScript diagnostics

## Breaking Changes

None (initial release)

## Installation

```bash
brew install ampcode/tap/zvelte-check
```
```

If no release notes file exists, the release will use "Release vX.Y.Z" as the body.

## What Happens on Release

When you push a tag matching `v*`:

1. **Test job** - Runs `zig build` and `zig build test`
2. **Build job** - Cross-compiles for 5 platforms:
   - `x86_64-linux-gnu`
   - `aarch64-linux-gnu`
   - `x86_64-macos`
   - `aarch64-macos`
   - `x86_64-windows-gnu`
3. **Release job** - Creates GitHub release with:
   - Tarballs (`.tar.gz`) for Unix platforms
   - Zip (`.zip`) for Windows
   - `checksums.txt` with SHA256 hashes
   - Release notes from `docs/releases/vX.Y.Z.md`
4. **Homebrew update** - Pushes updated formula to `ampcode/homebrew-tap`

## Prerelease Tags

Tags containing `-` (e.g., `v0.1.0-beta`, `v0.2.0-rc1`) are automatically marked as prereleases on GitHub.

## Secrets Required

- `HOMEBREW_TAP_TOKEN` - GitHub PAT with write access to `ampcode/homebrew-tap`

## Aqua Registry

To make the release available via mise/aqua, submit a PR to [aquaproj/aqua-registry](https://github.com/aquaproj/aqua-registry) with the files from `.github/aqua/`.
