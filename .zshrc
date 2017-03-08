## 起動速度測定用
##zmodload zsh/zprof && zprof

## 重複パスを登録しない
typeset -U path cdpath fpath manpath

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.dotfiles/oh-my-zsh

ZSH_THEME="powerline"

plugins=(
    react-native
    bundler
    cdd
    docker
    docker-compose
    encode64
    gem
    git
    homeshick
    rails
    rake
    vagrant
    knife
    tmux
    tmuxinator
    rbenv
    # zsh-syntax-highlighting
)

export PATH=$HOME/local/bin:$PATH
source $ZSH/oh-my-zsh.sh
for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf};
done

export PYENV_ROOT="${HOME}/.pyenv"
export PATH="${PYENV_ROOT}/bin:$PATH"
if [[ -x `which pyenv` ]]; then
    eval "$(pyenv init -)"
fi

## 起動速度測定用
# if (which zprof > /dev/null) ;then
#     zprof | less
# fi
