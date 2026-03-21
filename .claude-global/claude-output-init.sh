#!/bin/sh
set -e

SUBDIR="$1"
if [ -z "$SUBDIR" ]; then
    echo "Usage: $0 <subdir>" >&2
    exit 1
fi

TARGET=".claude/$SUBDIR"
mkdir -p "$TARGET"

GITIGNORE="$TARGET/.gitignore"
if [ ! -f "$GITIGNORE" ]; then
    printf '*\n' > "$GITIGNORE"
fi
