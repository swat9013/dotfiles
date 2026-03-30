#!/bin/bash
# Package manager version checker
brew_ver=$(brew --version 2>/dev/null | head -1 | awk '{print $2}') || brew_ver="(未インストール)"
mise_ver=$(mise version 2>/dev/null | awk '{print $1}') || mise_ver="(未インストール)"
mas_ver=$(mas version 2>/dev/null) || mas_ver="(未インストール)"
echo "brew: ${brew_ver} | mise: ${mise_ver} | mas: ${mas_ver}"
