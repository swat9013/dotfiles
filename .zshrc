## 重複パスを登録しない
typeset -U path cdpath fpath manpath

$HOME/.dotfiles/lib/auto_update.sh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.dotfiles/oh-my-zsh

[ -f ~/.zshrc.local ] && source ~/.zshrc.local

source $ZSH/oh-my-zsh.sh
for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf};
done

if [[ `uname` == 'Darwin' ]]; then
    export PATH="/usr/local/sbin:$PATH"
fi

if  (which go >/dev/null 2>&1) ; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi


# 起動速度測定用
if (which zprof >/dev/null 2>&1) ;then
    zprof | less
fi
