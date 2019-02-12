## 重複パスを登録しない
typeset -U path cdpath fpath manpath

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.dotfiles/oh-my-zsh

ZSH_THEME="powerline"

plugins=(
    # react-native
    bundler
    # cdd
    docker
    docker-compose
    # encode64
    embulk
    gem
    # git
    # homeshick
    rake
    # vagrant
    # knife
    tmux
    tmuxinator
    rbenv
    notify
)

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
    source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.zsh.inc'
    source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.zsh.inc'
fi

# 起動速度測定用
if (which zprof >/dev/null 2>&1) ;then
    zprof | less
fi
