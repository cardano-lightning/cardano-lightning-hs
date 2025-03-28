#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
TEST_VECTORS_DIR="$ROOT_DIR/cardano-lightning/test/Fixtures"
BASE_URL="https://raw.githubusercontent.com/cardano-lightning/cardano-lightning/paluh/close-step/aik/test-vectors"

# Create Fixtures directory if it doesn't exist
mkdir -p "$TEST_VECTORS_DIR"

# Download stage.json
echo "Downloading stage.json..."
curl -s "$BASE_URL/stage.json" > "$TEST_VECTORS_DIR/stage.json"

echo "Test vectors updated successfully"
