#!/bin/bash
# Build oracle/expmed-probe.cc for each GCC given (default: gcc-12 gcc-15)
# and write test/integration/expmed/expmed.<version>.rktd
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
out=$here/../../test/integration/expmed
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
echo 'int f (int x) { return x; }' > "$work/unit.c"
for gcc in "${@:-gcc-12 gcc-15}"; do
  for g in $gcc; do
    gxx=${g/gcc/g++}
    inc=$($g -print-file-name=plugin)/include
    ver=$($g -dumpfullversion)
    extra=()
    if [ "$(uname)" = Darwin ]; then extra=(-I/opt/homebrew/include -Wl,-undefined,dynamic_lookup); fi
    $gxx -std=c++17 -fPIC -shared -fno-rtti -O1 -I"$inc" "${extra[@]}" -o "$work/expmedoracle.so" "$here/expmed-probe.cc"
    $g -O1 -c -o /dev/null -fplugin="$work/expmedoracle.so" -fplugin-arg-expmedoracle-out="$out/expmed.$ver.rktd" "$work/unit.c"
    echo "$ver: $(wc -c < "$out/expmed.$ver.rktd") bytes"
  done
done
