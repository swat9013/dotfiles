## 重複パスを登録しない
typeset -U path cdpath fpath manpath

$HOME/.dotfiles/lib/auto_update.sh

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.dotfiles/oh-my-zsh

[ -f ~/.zshrc.local ] && source ~/.zshrc.local

export PATH=$HOME/local/bin:$PATH
export PATH="$HOME/.evm/bin:$PATH"
source $ZSH/oh-my-zsh.sh
for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf};
done

export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
if which pyenv >/dev/null 2>&1 ; then
    eval "$(pyenv init -)"
fi

if [[ `uname` == 'Darwin' ]]; then
    export PATH="/usr/local/sbin:$PATH"
fi

# 起動速度測定用
if (which zprof >/dev/null 2>&1) ;then
    zprof | less
fi
