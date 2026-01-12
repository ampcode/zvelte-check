#!/bin/bash
# Run conformance tests against svelte2tsx samples
#
# Compares zvelte-check output against svelte-check for each sample.
# Both tools should report the same diagnostics (or no diagnostics) for valid code.
#
# Usage: ./scripts/conformance-test.sh [options]
#
# Options:
#   --sample NAME    Run only the specified sample
#   --verbose        Show all output, not just failures
#   --summary        Show only summary stats
#   --list           List all available samples
#   --all            Run all samples, not just .v5 (Svelte 5) samples
#   --limit N        Run only the first N samples
#   --timeout N      Timeout per sample in seconds (default: 30)
#
# Exit codes:
#   0 - All samples match (or only expected differences)
#   1 - Unexpected differences found

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SAMPLES_DIR="$PROJECT_ROOT/test-fixtures/conformance/samples"
CONF_DIR="$PROJECT_ROOT/test-fixtures/conformance"
ZVELTE_CHECK="$PROJECT_ROOT/zig-out/bin/zvelte-check"

# Options
SINGLE_SAMPLE=""
VERBOSE=false
SUMMARY_ONLY=false
LIST_ONLY=false

TIMEOUT=30
LIMIT=0
ALL_SAMPLES=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --sample)
            SINGLE_SAMPLE="$2"
            shift 2
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        --summary)
            SUMMARY_ONLY=true
            shift
            ;;
        --list)
            LIST_ONLY=true
            shift
            ;;
        --timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        --limit)
            LIMIT="$2"
            shift 2
            ;;
        --all)
            ALL_SAMPLES=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check prerequisites
if [ ! -x "$ZVELTE_CHECK" ]; then
    echo "Error: zvelte-check not found at $ZVELTE_CHECK"
    echo "Run 'zig build' first"
    exit 1
fi

if [ ! -d "$SAMPLES_DIR" ]; then
    echo "Error: Samples directory not found at $SAMPLES_DIR"
    echo "Run './scripts/sync-svelte2tsx-samples.sh' first"
    exit 1
fi

if [ ! -d "$CONF_DIR/node_modules" ]; then
    echo "Error: node_modules not found in $CONF_DIR"
    echo "Run 'cd test-fixtures/conformance && pnpm install' first"
    exit 1
fi

# List samples if requested
if [ "$LIST_ONLY" = true ]; then
    ls -1 "$SAMPLES_DIR"
    exit 0
fi

# Temp files for output
SVELTE_OUT=$(mktemp)
ZVELTE_OUT=$(mktemp)
trap "rm -f $SVELTE_OUT $ZVELTE_OUT" EXIT

# Stats
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

# Track failures for summary
FAILURES=""

# Get list of samples
if [ -n "$SINGLE_SAMPLE" ]; then
    if [ ! -d "$SAMPLES_DIR/$SINGLE_SAMPLE" ]; then
        echo "Error: Sample '$SINGLE_SAMPLE' not found"
        exit 1
    fi
    SAMPLES="$SINGLE_SAMPLE"
elif [ "$ALL_SAMPLES" = true ]; then
    SAMPLES=$(ls -1 "$SAMPLES_DIR")
else
    # Default to .v5 samples only since zvelte-check targets Svelte 5+
    SAMPLES=$(ls -1 "$SAMPLES_DIR" | grep '\.v5$' || ls -1 "$SAMPLES_DIR")
fi

# Normalize output: extract just severity, file, line:col, message
# Remove timestamps and paths, sort for comparison
normalize_output() {
    local input="$1"
    local sample_name="$2"
    
    # Extract ERROR/WARNING lines, normalize paths, sort
    # Also normalize escaped quotes (\") to unescaped quotes (") for comparison
    # Truncate multi-line error messages (TypeScript adds context lines that vary between implementations)
    # Normalize trailing quote (some get truncated, some don't)
    grep -E "^[0-9]+ (ERROR|WARNING)" "$input" 2>/dev/null | \
        sed -E 's/^[0-9]+ //' | \
        sed -E "s|\"[^\"]*/$sample_name/([^\"]+)\"|\"\\1\"|g" | \
        sed -E 's|"samples/[^/]+/([^"]+)"|"\1"|g' | \
        sed 's/\\"/"/g' | \
        sed -E 's/\\n.*//' | \
        sed -E 's/"$//' | \
        sort || true
}

# Run tests
for SAMPLE in $SAMPLES; do
    # Check limit
    if [ "$LIMIT" -gt 0 ] && [ "$TOTAL" -ge "$LIMIT" ]; then
        break
    fi
    
    ((TOTAL++)) || true
    
    SAMPLE_PATH="$SAMPLES_DIR/$SAMPLE"
    INPUT_FILE="$SAMPLE_PATH/input.svelte"
    
    if [ ! -f "$INPUT_FILE" ]; then
        ((SKIPPED++)) || true
        continue
    fi
    
    # Create a temporary isolated workspace for this sample
    # This is necessary because svelte-check walks up and finds all .svelte files
    TEMP_WORKSPACE=$(mktemp -d)
    trap "rm -rf $TEMP_WORKSPACE" RETURN
    
    # Copy input.svelte to temp workspace
    cp "$INPUT_FILE" "$TEMP_WORKSPACE/"
    
    # Symlink node_modules from the conformance directory
    ln -s "$CONF_DIR/node_modules" "$TEMP_WORKSPACE/node_modules"
    
    # Create a minimal tsconfig.json
    cat > "$TEMP_WORKSPACE/tsconfig.json" << 'TSCONFIG'
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true
  },
  "include": ["input.svelte"],
  "exclude": ["node_modules"]
}
TSCONFIG
    
    # Run svelte-check in the isolated temp workspace (with timeout)
    cd "$TEMP_WORKSPACE"
    if command -v bun >/dev/null 2>&1; then
        timeout "$TIMEOUT" bun x svelte-check --workspace "." --output machine > "$SVELTE_OUT" 2>&1 || true
    else
        timeout "$TIMEOUT" npx svelte-check --workspace "." --output machine > "$SVELTE_OUT" 2>&1 || true
    fi
    cd "$CONF_DIR"
    
    # Clean up temp workspace
    rm -rf "$TEMP_WORKSPACE"
    
    # Run zvelte-check on the sample
    "$ZVELTE_CHECK" --workspace "$SAMPLE_PATH" --output machine > "$ZVELTE_OUT" 2>&1 || true
    
    # Normalize and compare
    SVELTE_NORM=$(normalize_output "$SVELTE_OUT" "$SAMPLE")
    ZVELTE_NORM=$(normalize_output "$ZVELTE_OUT" "$SAMPLE")
    
    # Count diagnostics
    SVELTE_COUNT=$(echo "$SVELTE_NORM" | grep -c . || echo "0")
    ZVELTE_COUNT=$(echo "$ZVELTE_NORM" | grep -c . || echo "0")
    
    # Compare
    if [ "$SVELTE_NORM" = "$ZVELTE_NORM" ]; then
        ((PASSED++)) || true
        if [ "$VERBOSE" = true ] && [ "$SUMMARY_ONLY" = false ]; then
            echo "✓ $SAMPLE (${SVELTE_COUNT} diagnostics)"
        fi
    else
        ((FAILED++)) || true
        FAILURES="$FAILURES$SAMPLE\n"
        
        if [ "$SUMMARY_ONLY" = false ]; then
            echo "✗ $SAMPLE"
            echo "  svelte-check: $SVELTE_COUNT diagnostics"
            echo "  zvelte-check: $ZVELTE_COUNT diagnostics"
            
            if [ "$VERBOSE" = true ]; then
                if [ -n "$SVELTE_NORM" ]; then
                    echo "  svelte-check output:"
                    echo "$SVELTE_NORM" | sed 's/^/    /'
                fi
                if [ -n "$ZVELTE_NORM" ]; then
                    echo "  zvelte-check output:"
                    echo "$ZVELTE_NORM" | sed 's/^/    /'
                fi
            fi
        fi
    fi
done

# Summary
echo ""
echo "=========================================="
echo "Conformance Test Results"
echo "=========================================="
echo "Total samples: $TOTAL"
echo "Passed:        $PASSED"
echo "Failed:        $FAILED"
echo "Skipped:       $SKIPPED"

if [ $FAILED -gt 0 ]; then
    echo ""
    echo "Failed samples:"
    echo -e "$FAILURES" | grep -v '^$' | sed 's/^/  /'
    exit 1
else
    echo ""
    echo "All samples passed!"
    exit 0
fi
