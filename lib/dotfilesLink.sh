#!/bin/sh
cd ~/.dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".gitmodule" ] && continue
    [ "$f" = ".gitconfig.local.sample" ] && continue
    [ "$f" = ".claude" ] && continue
    [ "$f" = ".claude-global" ] && continue

    ln -snfv "$HOME"/.dotfiles/"$f" "$HOME"/"$f"
done

if [ ! -e "$HOME"/.gitconfig.local ]; then
    cp .gitconfig.local.sample "$HOME"/.gitconfig.local
fi

if [ ! -e "$HOME"/.config/sheldon/plugins.toml ]; then
    ln -snfv "$HOME"/.dotfiles/sheldon "$HOME"/.config/sheldon
fi

# Ghostty設定
if [ ! -e "$HOME"/.config/ghostty ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/ghostty "$HOME"/.config/ghostty
fi

# Yazi設定
if [ ! -e "$HOME"/.config/yazi ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/yazi "$HOME"/.config/yazi
fi

# Claude Code global settings
mkdir -p "$HOME"/.claude
ln -snfv "$HOME"/.dotfiles/.claude-global/settings.json "$HOME"/.claude/settings.json
ln -snfv "$HOME"/.dotfiles/.claude-global/CLAUDE.md "$HOME"/.claude/CLAUDE.md

# Claude Code skills
if [ -d "$HOME"/.dotfiles/.claude-global/skills ]; then
    ln -snfv "$HOME"/.dotfiles/.claude-global/skills "$HOME"/.claude/skills
fi

# Claude Code rules
if [ -d "$HOME"/.dotfiles/.claude-global/rules ]; then
    ln -snfv "$HOME"/.dotfiles/.claude-global/rules "$HOME"/.claude/rules
fi
