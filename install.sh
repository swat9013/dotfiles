#!/bin/sh
DOTPATH=~/.dotfiles
GITHUB_URL=https://github.com/swat9013/dotfiles.git

# git が使えるなら git
if  [ `which git` ]; then
    git clone --recursive "$GITHUB_URL" "$DOTPATH"
else
    echo "git requrired"
    exit 1
fi

cd ~/.dotfiles
if [ $? -ne 0 ]; then
    echo "not found: $DOTPATH"
    exit 1
fi

if [ "$(uname)" == 'Darwin' ]; then
    echo "=== Mac OS==="
    if  [ `which brew` ]; then
        brew install peco ripgrep
    else
        echo "home brew requrired"
        echo "https://brew.sh/index_ja.html"
        exit 1
    fi

    if  [ `which pip` ]; then
        pip install powerline-status
    else
        echo "pip requrired"
        exit 1
    fi

    if  [ `which gem` ]; then
        gem install tmuxinator
    else
        echo "gem requrired"
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    echo "=== Linux OS ==="
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then
    echo "=== Cygwin OS ==="
else
    echo "Your platform ($(uname -a))"
fi

sh dotfilesLink.sh
