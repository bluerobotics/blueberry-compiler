#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
IDL_PATH="${1:-${REPO_ROOT}/crates/parser/tests/fixtures/message_basic.idl}"

"${SCRIPT_DIR}/generate.sh" "${IDL_PATH}"

echo "Running C validation"
gcc -c -o "${REPO_ROOT}/test/c/blueberry_runtime.o" "${REPO_ROOT}/test/c/blueberry_runtime.c"
gcc -c -o "${REPO_ROOT}/test/c/blueberry_messages.o" "${REPO_ROOT}/test/c/blueberry_messages.c"

echo "Running C++ validation"
gcc -std=c++20 "${REPO_ROOT}/test/cpp/messages.hpp"

echo "Running Python validation"
cp "${REPO_ROOT}/examples/python/pyproject.toml" "${REPO_ROOT}/test/python/pyproject.toml"
uv run --directory "${REPO_ROOT}/test/python/" "message.py"

echo "Running TypeScript validation"
TS_DIR="${REPO_ROOT}/test/typescript-project"
rm -rf "${TS_DIR}"
mkdir -p "${TS_DIR}"
mv "${REPO_ROOT}/test/typescript" "${TS_DIR}/typescript"
cp "${REPO_ROOT}/examples/typescript/package.json"  "${TS_DIR}/package.json"
cp "${REPO_ROOT}/examples/typescript/tsconfig.json" "${TS_DIR}/tsconfig.json"
if [ -f "${REPO_ROOT}/examples/typescript/package-lock.json" ]; then
  cp "${REPO_ROOT}/examples/typescript/package-lock.json" "${TS_DIR}/package-lock.json"
  (cd "${TS_DIR}" && npm ci --no-audit --no-fund && ./node_modules/.bin/tsc --noEmit)
else
  (cd "${TS_DIR}" && npm install --no-audit --no-fund --loglevel=error && ./node_modules/.bin/tsc --noEmit)
fi
