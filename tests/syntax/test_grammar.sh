#!/usr/bin/env bash
set -euo pipefail

GRAMMAR=syntaxes/aurum.tmLanguage.json
EXAMPLES_DIR=examples/syntax_highlight_examples

echo "Validating grammar JSON: $GRAMMAR"
python3 - <<'PY'
import json,sys
try:
    with open('syntaxes/aurum.tmLanguage.json','r') as f:
        json.load(f)
except Exception as e:
    print('JSON parse error:', e)
    sys.exit(2)
print('OK: JSON valid')
PY

# Check that keywords and types exist in grammar
keywords_expected=(fn var let co con if else while return true false extern case switch break default)
types_expected=(int8 int16 int32 int64 int bool void pchar string)

for kw in "${keywords_expected[@]}"; do
  if ! grep -q "\\b${kw}\\b" "$GRAMMAR"; then
    echo "WARNING: keyword '${kw}' not found in grammar"
  else
    echo "Found keyword: ${kw}"
  fi
done

for t in "${types_expected[@]}"; do
  if ! grep -q "\\b${t}\\b" "$GRAMMAR"; then
    echo "WARNING: type '${t}' not found in grammar"
  else
    echo "Found type: ${t}"
  fi
done

# Check examples contain relevant constructs
for f in "$EXAMPLES_DIR"/*.au; do
  echo "Checking example: $f"
  if grep -q "\bcon\b" "$f"; then
    echo " - contains con"
  fi
  if grep -q "\bswitch\b" "$f" || grep -q "\bcase\b" "$f"; then
    echo " - contains switch/case"
  fi
done

echo "All checks completed." 
