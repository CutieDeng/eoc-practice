#!/bin/bash
# Run every unit test module under src/ and test/, plus the aarch64 integration script.
# bash 3 compatible (macOS /bin/bash): no mapfile.
set -uo pipefail
cd "$(dirname "$0")/.."
FILES=$(find src test -name '*-test.rkt' | sort)
# shellcheck disable=SC2086
raco test $FILES src/kernel/data/idmap.rkt gccast-plugin/gen/treedef.rkt
rc=$?
racket test/integration/aarch64/basic.rkt || rc=1
exit $rc
