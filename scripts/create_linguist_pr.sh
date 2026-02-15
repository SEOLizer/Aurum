#!/usr/bin/env bash
set -euo pipefail

# Script to fork github/linguist, copy Aurum grammar and samples, and open a PR.
# Usage: run after authenticating gh (gh auth login --web)

echo "Check gh authentication..."
gh auth status || {
  echo "gh is not authenticated. Run: gh auth login --web" >&2
  exit 2
}

# Fork and clone linguist
echo "Forking and cloning github/linguist..."
gh repo fork github/linguist --clone --remote=false

cd linguist
BRANCH=feat/aurum-grammar
git checkout -b $BRANCH

# Ensure directories
mkdir -p grammars/aurum samples/aurum

# Copy prepared files from the Aurum repo (assumes script run from Aurum repo root)
cp ../linguist-contrib/aurum/aurum.tmLanguage.json grammars/aurum/aurum.tmLanguage.json
cp -r ../linguist-contrib/aurum/examples/* samples/aurum/ || true
cp ../linguist-contrib/aurum/README.md . || true

# Append languages.yml snippet to the end of lib/linguist/languages.yml
if [ -f ../linguist-contrib/aurum/languages.yml.snippet ]; then
  echo "\n# ---- Aurum language (added by automation) ----" >> lib/linguist/languages.yml
  cat ../linguist-contrib/aurum/languages.yml.snippet >> lib/linguist/languages.yml
  echo "Appended languages.yml.snippet to lib/linguist/languages.yml"
else
  echo "languages.yml.snippet not found in ../linguist-contrib/aurum/. Please copy the snippet manually into lib/linguist/languages.yml" >&2
fi

# Commit & push
git add grammars/aurum samples/aurum lib/linguist/languages.yml README.md || true
git commit -m "Add Aurum TextMate grammar and samples (automation)" || true

echo "Pushing branch to your fork..."
git push -u origin $BRANCH

# Create PR
echo "Creating PR to github/linguist..."
PR_BODY=$(cat <<'EOF'
Adds an initial TextMate grammar for the Aurum language (scope: source.aurum).
Includes: grammar (grammars/aurum/aurum.tmLanguage.json), sample fixtures (samples/aurum) and a languages.yml snippet.

This is an initial, conservative grammar. Please review and suggest improvements. The Aurum compiler repo is at: https://github.com/SEOLizer/Aurum
EOF
)

gh pr create --repo github/linguist --title "Add Aurum language grammar" --body "$PR_BODY" --base main --head $(gh api user --jq .login):$BRANCH

echo "PR created. Please review the PR in your fork and add maintainer-facing notes/screenshots if needed."
