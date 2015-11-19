## 起動速度測定用
##zmodload zsh/zprof && zprof

# Path to your oh-my-zsh installation.
export ZSH=$HOME/dotfiles/oh-my-zsh

ZSH_THEME="powerline"

plugins=(
    bundler
    cdd
    docker
    encode64
    gem
    git
    homeshick
    rails
    rake
    vagrant
    knife
    # zsh-syntax-highlighting
)

source $ZSH/oh-my-zsh.sh
for conf in $HOME/dotfiles/.zsh/*.zsh; do
    source ${conf};
done

## 起動速度測定用
# if (which zprof > /dev/null) ;then
#     zprof | less
# fi
