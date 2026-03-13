#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"

echo "==> Installing Dwayne Raycast extension"

# Check prerequisites
if ! command -v bun &>/dev/null; then
  echo "ERROR: bun is required. Install with: brew install oven-sh/bun/bun"
  exit 1
fi

if ! command -v ray &>/dev/null; then
  echo "WARNING: Raycast Developer Tools not found."
  echo "Install from: Raycast > Settings > Extensions > Developer > Install Developer Tools"
  echo ""
  echo "After installing, re-run this script."
  exit 1
fi

if ! command -v dwayne &>/dev/null; then
  echo "WARNING: dwayne binary not found on PATH."
  echo "Build and install: cd core && cabal install --overwrite-policy=always"
  echo "Or set the binary path in the extension preferences."
fi

# Install dependencies
echo "==> Installing dependencies..."
bun install

# Register with Raycast in dev mode
echo "==> Registering with Raycast..."
bun run dev &
DEV_PID=$!

# Give it a moment to register, then stop
sleep 3
kill "$DEV_PID" 2>/dev/null || true

echo ""
echo "Done! The extension is now available in Raycast."
echo ""
echo "Configure preferences in Raycast:"
echo "  - Config Path: path to your dwayne config.yml"
echo "  - Dwayne Binary: path to dwayne binary (if not on PATH)"
