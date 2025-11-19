#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FIXTURE_SRC="$REPO_ROOT/crates/parser/tests/fixtures"
FIXTURE_DEST="$REPO_ROOT/pkg/fixtures"

if [ ! -d "$FIXTURE_SRC" ]; then
    echo "Fixture source directory not found: $FIXTURE_SRC" >&2
    exit 1
fi

mkdir -p "$FIXTURE_DEST"

files=()
if compgen -G "$FIXTURE_SRC"/*.idl >/dev/null; then
    files=( "$FIXTURE_SRC"/*.idl )
else
    echo "No .idl fixtures found under $FIXTURE_SRC" >&2
fi

for file in "${files[@]}"; do
    cp "$file" "$FIXTURE_DEST/"
done

{
    for file in "${files[@]}"; do
        printf '%s\n' "$(basename "$file")"
    done
} > "$FIXTURE_DEST/index.txt"

echo "Synced ${#files[@]} fixture(s) into $FIXTURE_DEST"
