#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
TEST_ROOT="${REPO_ROOT}/test"
IDL_PATH="${1:-${REPO_ROOT}/crates/parser/tests/fixtures/message_basic.idl}"

if [ ! -f "${IDL_PATH}" ]; then
  echo "IDL input not found: ${IDL_PATH}" >&2
  exit 1
fi

mkdir -p "${TEST_ROOT}/python" "${TEST_ROOT}/rust"

for lang in c cpp python rust; do
  rm -rf "${TEST_ROOT}/${lang}"
done

cd "${REPO_ROOT}"

for emit in c cpp python rust; do
  cargo run -p blueberry-cli -- "${IDL_PATH}" --emit-"${emit}" --output-dir "${TEST_ROOT}"
done
