#!/bin/bash
# git-aicommit: AI-powered commit message generator using Claude Code
# Usage: git aicommit

set -euo pipefail

# Check for staged changes
if ! git diff --cached --quiet --exit-code 2>/dev/null; then
    :
else
    echo "Error: No staged changes to commit" >&2
    exit 1
fi

# JSON Schema for structured output
SCHEMA='{"type":"object","properties":{"message":{"type":"string"}},"required":["message"]}'

# Generate commit message using Claude with structured output
COMMITMSG=$(claude --model haiku -p --output-format json --json-schema "$SCHEMA" \
'Based on `git diff --cached`, generate a Git commit message following Conventional Commits format.

Format:
<type>(<scope>): <subject in Japanese>

<body in Japanese explaining WHY>

Types: feat|fix|docs|refactor|test|chore|style|perf|ci|build

Rules:
- Type/Scope: English
- Subject: Japanese, no period
- Body: Japanese, explain WHY (the reason/motivation), not WHAT
- Keep it concise

Output the commit message in the "message" field.' 2>/dev/null | \
    jq -r '[.[] | select(.type=="result")] | .[0].structured_output.message')

# Validate output
if [[ -z "$COMMITMSG" || "$COMMITMSG" == "null" ]]; then
    echo "Error: Failed to generate commit message" >&2
    exit 1
fi

# Commit with editor for review
git commit -m "$COMMITMSG" -e
