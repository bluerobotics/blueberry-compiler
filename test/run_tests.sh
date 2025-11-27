#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
IDL_PATH="${1:-${REPO_ROOT}/crates/parser/tests/fixtures/message_basic.idl}"

"${SCRIPT_DIR}/generate.sh" "${IDL_PATH}"

echo "Running C validation"
gcc "${REPO_ROOT}/test/c/messages.h"

echo "Running C++ validation"
gcc -std=c++20 "${REPO_ROOT}/test/cpp/messages.hpp"

echo "Running Python validation"
python "${REPO_ROOT}/test/python/messages.py"
