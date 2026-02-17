#!/usr/bin/env bash
set -euo pipefail

# Build aurumc if not present
if [ ! -x ./lyxc ]; then
  echo "aurumc not found, building..."
  fpc -O2 -Mobjfpc -Sh lyxc.lpr -olyxc
fi

OUT=/tmp/int_widths_test
./lyxc examples/int_widths.lyx -o "$OUT"
OUTPUT=$("$OUT" | tr -d '\r')
# Expect print_int(x) where a=int8(130)->trunc(130)->-126? But truncation semantics: storing 130 into int8 signed gives -126.
# b = uint8 250, so a + b (promoted) = -126 + 250 = 124
if [ "$OUTPUT" = "124" ]; then
  echo "E2E OK: output=$OUTPUT"
  exit 0
else
  echo "E2E FAIL: expected 124 but got '$OUTPUT'"
  exit 2
fi
