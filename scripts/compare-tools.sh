#!/bin/bash
# Compare zvelte-check output against svelte-check
# Usage: ./scripts/compare-tools.sh [fixture-dir]
# Exit 0 if outputs match, 1 if there are differences

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
FIXTURE_DIR="${1:-$PROJECT_ROOT/test-fixtures/comparison}"
ZVELTE_CHECK="$PROJECT_ROOT/zig-out/bin/zvelte-check"

if [ ! -x "$ZVELTE_CHECK" ]; then
    echo "Error: zvelte-check not found at $ZVELTE_CHECK"
    echo "Run 'zig build' first"
    exit 1
fi

if [ ! -d "$FIXTURE_DIR/node_modules" ]; then
    echo "Error: node_modules not found in $FIXTURE_DIR"
    echo "Run 'pnpm install' in $FIXTURE_DIR first"
    exit 1
fi

# Create temp files for output
SVELTE_OUT=$(mktemp)
ZVELTE_OUT=$(mktemp)
trap "rm -f $SVELTE_OUT $ZVELTE_OUT" EXIT

echo "Comparing svelte-check vs zvelte-check on $FIXTURE_DIR"
echo ""

# Run svelte-check (try bun first, fall back to pnpm)
cd "$FIXTURE_DIR"
if command -v bun >/dev/null 2>&1; then
    bun run svelte-check --output machine 2>&1 | grep -E "^[0-9]+ (ERROR|WARNING)" | sort > "$SVELTE_OUT" || true
else
    pnpm exec svelte-check --output machine 2>&1 | grep -E "^[0-9]+ (ERROR|WARNING)" | sort > "$SVELTE_OUT" || true
fi

# Run zvelte-check  
"$ZVELTE_CHECK" --workspace "$FIXTURE_DIR" --output machine 2>&1 | grep -E "^[0-9]+ (ERROR|WARNING)" | sort > "$ZVELTE_OUT" || true

# Normalize paths: svelte-check uses relative paths, zvelte-check uses full paths from workspace
# Extract just: SEVERITY "filename" line:col "message"
normalize() {
    sed -E 's|^[0-9]+ ||' | \
    sed -E 's|"[^"]*/(comparison)/([^"]+)"|"\2"|g' | \
    sed -E 's|"test-fixtures/comparison/([^"]+)"|"\1"|g' | \
    sort
}

SVELTE_NORM=$(mktemp)
ZVELTE_NORM=$(mktemp)
trap "rm -f $SVELTE_OUT $ZVELTE_OUT $SVELTE_NORM $ZVELTE_NORM" EXIT

cat "$SVELTE_OUT" | normalize > "$SVELTE_NORM"
cat "$ZVELTE_OUT" | normalize > "$ZVELTE_NORM"

echo "svelte-check errors:"
cat "$SVELTE_NORM" | head -20
echo ""
echo "zvelte-check errors:"
cat "$ZVELTE_NORM" | head -20
echo ""

# Compare
SVELTE_COUNT=$(wc -l < "$SVELTE_NORM" | tr -d ' ')
ZVELTE_COUNT=$(wc -l < "$ZVELTE_NORM" | tr -d ' ')

echo "svelte-check: $SVELTE_COUNT diagnostics"
echo "zvelte-check: $ZVELTE_COUNT diagnostics"
echo ""

# Check for false positives (zvelte has errors svelte doesn't)
FALSE_POSITIVES=$(comm -23 "$ZVELTE_NORM" "$SVELTE_NORM" | wc -l | tr -d ' ')
# Check for false negatives (zvelte misses errors svelte has)
FALSE_NEGATIVES=$(comm -13 "$ZVELTE_NORM" "$SVELTE_NORM" | wc -l | tr -d ' ')

if [ "$FALSE_POSITIVES" -gt 0 ]; then
    echo "FALSE POSITIVES (zvelte-check reports, svelte-check doesn't):"
    comm -23 "$ZVELTE_NORM" "$SVELTE_NORM"
    echo ""
fi

if [ "$FALSE_NEGATIVES" -gt 0 ]; then
    echo "FALSE NEGATIVES (svelte-check reports, zvelte-check doesn't):"
    comm -13 "$ZVELTE_NORM" "$SVELTE_NORM"
    echo ""
fi

if [ "$FALSE_POSITIVES" -eq 0 ] && [ "$FALSE_NEGATIVES" -eq 0 ]; then
    echo "✓ Outputs match!"
    exit 0
else
    echo "✗ Outputs differ: $FALSE_POSITIVES false positives, $FALSE_NEGATIVES false negatives"
    exit 1
fi
