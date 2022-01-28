#!/bin/sh
DOTPATH=~/.dotfiles
GITHUB_URL=https://github.com/swat9013/dotfiles.git

# git が使えるなら git
if  [ `which git` ]; then
    git clone "$GITHUB_URL" "$DOTPATH"
else
    echo "git requrired"
    exit 1
fi

cd $DOTPATH
if [ $? -ne 0 ]; then
    echo "not found: $DOTPATH"
    exit 1
fi

git clone --depth 1 https://github.com/ohmyzsh/ohmyzsh.git oh-my-zsh
git clone --depth 1 https://github.com/marzocchi/zsh-notify.git oh-my-zsh/custom/plugins/notify
git clone --depth 1 https://github.com/zsh-users/zsh-syntax-highlighting.git oh-my-zsh/custom/plugins/zsh-syntax-highlighting

sh lib/dotfilesLink.sh

if [ "$(uname)" == 'Darwin' ]; then
    echo "=== Mac OS==="
    if  [ `which brew` ]; then
        brew bundle --global --no-lock --cleanup
    else
        echo "home brew requrired"
        echo "https://brew.sh/index_ja.html"
        exit 1
    fi

    # http://hoppsjots.org/?p=177
    # Fix Underline + Color in TMUX (TERM=screen-256color)
    $({ infocmp -x screen-256color; printf '\t%s\n' 'ncv@,'; } > /tmp/t && tic -x /tmp/t)
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    echo "=== Linux OS ==="
elif [ "$(expr substr $(uname -s) 1 10)" == 'MINGW32_NT' ]; then
    echo "=== Cygwin OS ==="
else
    echo "Your platform ($(uname -a))"
fi
