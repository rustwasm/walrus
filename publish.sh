#!/usr/bin/env bash

# Helper script for publishing all the Walrus crates.
#
# Usage:
#
#     ./publish.sh

set -eux

cd "$(dirname "$0")/crates/macro"
cargo publish

cd ../..
cargo publish
