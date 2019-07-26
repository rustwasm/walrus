#!/usr/bin/env bash

# This script can be used with `creduce` to reduce the size of a WAT test case
# that has different output before vs after being round tripped through
# Walrus. I've successfully used it to reduce ~5KiB test cases down to ~100
# bytes in only a couple minutes!
#
# Usage:
#
#     creduce walrus/crates/fuzz/before-after.sh path/to/fuzz.wat

set -eux

INPUT="fuzz.wat"
TEST_FILE="fuzz.wasm"
OUT_FILE="$TEST_FILE.walrus.wasm"
ORIG_OUTPUT_FILE="$TEST_FILE.output.txt"
NEW_OUTPUT_FILE="$OUT_FILE.output.txt"

function interp {
    wasm-interp \
        --run-all-exports \
        --dummy-import-func \
        --enable-threads \
        --enable-bulk-memory \
        --enable-reference-types \
        --enable-simd \
        "$1"
}

wat2wasm "$INPUT" -o "$TEST_FILE" --enable-simd --enable-bulk-memory
interp "$TEST_FILE" > "$ORIG_OUTPUT_FILE"

cargo run \
      --manifest-path "$(dirname "$0")/../../Cargo.toml" \
      --example round-trip \
      "$TEST_FILE" \
      "$OUT_FILE"

interp "$OUT_FILE" > "$NEW_OUTPUT_FILE"

! diff -U3 "$ORIG_OUTPUT_FILE" "$NEW_OUTPUT_FILE"
