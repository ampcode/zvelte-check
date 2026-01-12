#!/bin/bash
# Sync svelte2tsx test samples from sveltejs/language-tools
#
# Downloads input.svelte files from:
# - packages/svelte2tsx/test/svelte2tsx/samples/
#
# Usage: ./scripts/sync-svelte2tsx-samples.sh [--force]
#
# The --force flag re-downloads even if samples already exist.
#
# Samples are placed in test-fixtures/conformance/samples/<sample-name>/input.svelte
# Only Svelte 5 samples (.v5 suffix) are downloaded by default since zvelte-check
# only supports Svelte 5+.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SAMPLES_DIR="$PROJECT_ROOT/test-fixtures/conformance/samples"
FORCE=false

if [ "$1" = "--force" ]; then
    FORCE=true
fi

# Create samples directory
mkdir -p "$SAMPLES_DIR"

# GitHub API base for raw content
RAW_BASE="https://raw.githubusercontent.com/sveltejs/language-tools/master/packages/svelte2tsx/test/svelte2tsx/samples"
API_BASE="https://api.github.com/repos/sveltejs/language-tools/contents/packages/svelte2tsx/test/svelte2tsx/samples"

echo "Fetching sample list from GitHub API..."

# Get all sample directories
# Filter for .v5 samples (Svelte 5) since zvelte-check only supports Svelte 5+
# Also include samples that don't have version suffix (generic samples)
SAMPLES=$(curl -s "$API_BASE" | jq -r '.[] | select(.type == "dir") | .name')

if [ -z "$SAMPLES" ]; then
    echo "Error: Failed to fetch samples from GitHub API"
    echo "Rate limited? Try again later or use a GitHub token."
    exit 1
fi

# Count samples
TOTAL=$(echo "$SAMPLES" | wc -l | tr -d ' ')
echo "Found $TOTAL sample directories"

# Download samples
DOWNLOADED=0
SKIPPED=0
FAILED=0

for SAMPLE in $SAMPLES; do
    SAMPLE_DIR="$SAMPLES_DIR/$SAMPLE"
    INPUT_FILE="$SAMPLE_DIR/input.svelte"
    
    # Skip non-v5 samples that have version suffixes (like .v3, .v4)
    # We want: samples without version suffix OR .v5 suffix
    if echo "$SAMPLE" | grep -qE '\.v[0-4]$'; then
        continue
    fi
    
    # Skip if already downloaded (unless --force)
    if [ -f "$INPUT_FILE" ] && [ "$FORCE" = false ]; then
        ((SKIPPED++)) || true
        continue
    fi
    
    mkdir -p "$SAMPLE_DIR"
    
    # Download input.svelte
    URL="$RAW_BASE/$SAMPLE/input.svelte"
    if curl -sf "$URL" -o "$INPUT_FILE" 2>/dev/null; then
        ((DOWNLOADED++)) || true
        echo "  Downloaded: $SAMPLE"
    else
        # Some samples might not have input.svelte (unlikely but handle it)
        rm -rf "$SAMPLE_DIR"
        ((FAILED++)) || true
    fi
    
    # Rate limit protection (GitHub allows 60 unauthenticated requests/hour)
    # Sleep briefly between requests
    sleep 0.1
done

echo ""
echo "Sync complete:"
echo "  Downloaded: $DOWNLOADED"
echo "  Skipped (existing): $SKIPPED"  
echo "  Failed: $FAILED"

# Create package.json and tsconfig.json for the conformance directory
CONF_DIR="$PROJECT_ROOT/test-fixtures/conformance"

if [ ! -f "$CONF_DIR/package.json" ]; then
    cat > "$CONF_DIR/package.json" << 'EOF'
{
  "name": "zvelte-check-conformance",
  "private": true,
  "type": "module",
  "scripts": {
    "svelte-check": "svelte-check"
  },
  "devDependencies": {
    "svelte": "^5.0.0",
    "svelte-check": "^4.0.0",
    "typescript": "^5.0.0"
  }
}
EOF
    echo "Created package.json"
fi

if [ ! -f "$CONF_DIR/tsconfig.json" ]; then
    cat > "$CONF_DIR/tsconfig.json" << 'EOF'
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true
  },
  "include": ["samples/**/*.svelte", "samples/**/*.ts"],
  "exclude": ["node_modules"]
}
EOF
    echo "Created tsconfig.json"
fi

echo ""
echo "Next steps:"
echo "  cd test-fixtures/conformance && pnpm install"
echo "  ./scripts/conformance-test.sh"
