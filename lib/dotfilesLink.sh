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
# sh "$HOME"/.dotfiles/ghostty/macos-defaults.sh

# Alacritty設定
if [ ! -e "$HOME"/.config/alacritty ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/alacritty "$HOME"/.config/alacritty
fi

# Starship設定
if [ ! -e "$HOME"/.config/starship ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/starship "$HOME"/.config/starship
fi

# Yazi設定
if [ ! -e "$HOME"/.config/yazi ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/yazi "$HOME"/.config/yazi
fi

# Zellij設定
if [ ! -e "$HOME"/.config/zellij ]; then
    mkdir -p "$HOME"/.config
    ln -snfv "$HOME"/.dotfiles/zellij "$HOME"/.config/zellij
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

# Colima設定
if [ -f "$HOME"/.dotfiles/colima/colima.yaml ]; then
    mkdir -p "$HOME"/.colima/default
    ln -snfv "$HOME"/.dotfiles/colima/colima.yaml "$HOME"/.colima/default/colima.yaml
fi

# VS Code設定
VSCODE_USER="$HOME/Library/Application Support/Code/User"
if [ -d "$VSCODE_USER" ]; then
    ln -snfv "$HOME/.dotfiles/vscode/settings.json" "$VSCODE_USER/settings.json"
    ln -snfv "$HOME/.dotfiles/vscode/keybindings.json" "$VSCODE_USER/keybindings.json"
fi
