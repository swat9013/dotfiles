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

ln -f ~/.dotfiles/oh-my-zsh_plugins/my-agnoster.zsh-theme ~/.dotfiles/oh-my-zsh/themes/my-agnoster.zsh-theme
ln -f ~/.dotfiles/oh-my-zsh_plugins/powerline.zsh-theme ~/.dotfiles/oh-my-zsh/themes/powerline.zsh-theme
ln -snf ~/.dotfiles/zsh-syntax-highlighting ~/.dotfiles/oh-my-zsh/custom/plugins/zsh-syntax-highlighting
ln -snf ~/.dotfiles/zsh-notify ~/.dotfiles/oh-my-zsh/custom/plugins/notify

if [ ! -e "$HOME"/.gitconfig.local ]; then
    cp .gitconfig.local.sample "$HOME"/.gitconfig.local
fi

if [ ! -e "$HOME"/.zshrc.local ]; then
    cp .zshrc.local.template "$HOME"/.zshrc.local
fi
