#!/bin/bash
# Build script for CLAPS CLI executable
#
# Usage: ./scripts/build-claps.sh [output-path]
#
# Requires: SBCL and Quicklisp

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
OUTPUT_PATH="${1:-$PROJECT_DIR/claps}"

echo "CLAPS Build Script"
echo "=================="
echo "Project directory: $PROJECT_DIR"
echo "Output path: $OUTPUT_PATH"
echo

cd "$PROJECT_DIR"

# Check for SBCL
if ! command -v sbcl &> /dev/null; then
    echo "Error: SBCL not found. Please install SBCL first."
    exit 1
fi

# Build
echo "Building CLAPS executable..."
sbcl --non-interactive \
     --load "$PROJECT_DIR/src/cli/build.lisp" \
     --eval "(build-claps \"$OUTPUT_PATH\")"

if [ -f "$OUTPUT_PATH" ]; then
    chmod +x "$OUTPUT_PATH"
    echo
    echo "Build complete!"
    echo "Executable: $OUTPUT_PATH"
    echo
    echo "Test with: $OUTPUT_PATH --help"
else
    echo "Error: Build failed - executable not created"
    exit 1
fi
