#!/usr/bin/env bash
set -euo pipefail
# Build and run examples to verify runtime behavior
ROOT=$(dirname "$0")/../..
cd "$ROOT"

# build compiler
fpc -O2 -Mobjfpc -Sh aurumc.lpr -oaurumc

# run use_env
./aurumc examples/use_env.au -o /tmp/use_env
/tmp/use_env foo bar || true

# run use_math
./aurumc examples/use_math.au -o /tmp/use_math
/tmp/use_math || true

echo "Integration examples executed (outputs above)"
