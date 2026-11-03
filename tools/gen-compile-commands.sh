#!/usr/bin/env bash
set -euo pipefail

# Usage:
#   tools/gen-compile-commands.sh [path-to-package]
# Examples:
#   tools/gen-compile-commands.sh package
#   tools/gen-compile-commands.sh .

PKGDIR="${1:-package}"

if ! command -v R >/dev/null 2>&1; then
  echo "ERROR: R not found in PATH" >&2
  exit 1
fi

if ! command -v bear >/dev/null 2>&1; then
  echo "ERROR: bear not found. Install it (e.g. on Ubuntu: sudo apt-get install bear)" >&2
  exit 1
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PKGDIR_ABS="$(cd "$PKGDIR" && pwd)"

OUTDIR="${ROOT}/build"
OUT="${OUTDIR}/compile_commands.json"
mkdir -p "$OUTDIR"
rm -f "$OUT"

# Install into a temp library so we don't touch the user's R libs.
LIBDIR="$(mktemp -d)"
cleanup() { rm -rf "$LIBDIR"; }
trap cleanup EXIT

# This helps avoid "staged" installs that can make paths point at temp copies.
export R_INSTALL_STAGED=false

# Record compile commands while building/installing.
bear --output "$OUT" -- \
  R CMD INSTALL "$PKGDIR_ABS" \
    --library="$LIBDIR" \
    --preclean \
    --no-test-load \
    --no-byte-compile \
    --no-build-vignettes

echo "Wrote: $OUT"