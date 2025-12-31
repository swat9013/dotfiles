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

# Claude Code global settings
mkdir -p "$HOME"/.claude
ln -snfv "$HOME"/.dotfiles/.claude-global/settings.json "$HOME"/.claude/settings.json
