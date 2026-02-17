#!/usr/bin/env bash
set -euo pipefail
# Build and run examples to verify runtime behavior
ROOT=$(dirname "$0")/../..
cd "$ROOT"

# build compiler
fpc -O2 -Mobjfpc -Sh lyxc.lpr -olyxc

# run use_env
./lyxc examples/use_env.lyx -o /tmp/use_env
/tmp/use_env foo bar || true

# run use_math
./lyxc examples/use_math.lyx -o /tmp/use_math
/tmp/use_math || true

echo "Integration examples executed (outputs above)"
