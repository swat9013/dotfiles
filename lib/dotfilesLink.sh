#!/bin/sh
cd ~/.dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".gitmodule" ] && continue
    [ "$f" = ".gitconfig.local.sample" ] && continue

    ln -snfv "$HOME"/.dotfiles/"$f" "$HOME"/"$f"
done

if [ ! -e "$HOME"/.gitconfig.local ]; then
    cp .gitconfig.local.sample "$HOME"/.gitconfig.local
fi
