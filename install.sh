#!/bin/sh
DOTPATH=~/.dotfiles
GITHUB_URL=https://github.com/swat9013/dotfiles.git

# git が使えるなら git
if  [ `which git` ]; then
    git clone --recursive "$GITHUB_URL" "$DOTPATH"
else
    echo "git requred"
    exit 1
fi

cd ~/.dotfiles
if [ $? -ne 0 ]; then
    echo "not found: $DOTPATH"
    exit 1
fi

sh dotfilesLink.sh
