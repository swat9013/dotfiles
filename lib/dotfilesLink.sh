#!/bin/sh
cd ~/.dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".gitmodule" ] && continue
    [ "$f" = ".gitconfig.local.sample" ] && continue
    [ "$f" = ".zshrc.local.template" ] && continue

    ln -snfv "$HOME"/.dotfiles/"$f" "$HOME"/"$f"
done

if [ ! -e "$HOME"/.gitconfig.local ]; then
    cp .gitconfig.local.sample "$HOME"/.gitconfig.local
fi

if [ ! -e "$HOME"/.zshrc.local ]; then
    cp .zshrc.local.template "$HOME"/.zshrc.local
fi

cd oh-my-zsh_theme
for f in ??*
do
  ln -f ~/.dotfiles/oh-my-zsh_theme/$f ~/.dotfiles/oh-my-zsh/themes/$f
done
