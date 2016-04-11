#!/bin/sh
cd ~/.dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".gitmodule" ] && continue
    [ "$f" = ".gitconfig.local.sample" ] && continue
    [ "$f" = ".zshrc.template" ] && continue

    ln -snfv "$HOME"/.dotfiles/"$f" "$HOME"/"$f"
done

ln -f ~/dotfiles/my-agnoster.zsh-theme ~/dotfiles/oh-my-zsh/themes/my-agnoster.zsh-theme
ln -f ~/dotfiles/powerline.zsh-theme ~/dotfiles/oh-my-zsh/themes/powerline.zsh-theme
ln -snf ~/dotfiles/zsh-syntax-highlighting ~/dotfiles/oh-my-zsh/custom/plugins/zsh-syntax-highlighting

if [ ! -e "$HOME"/.gitconfig.local ]; then
    cp .gitconfig.local.sample "$HOME"/.gitconfig.local
fi
