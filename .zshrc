## 重複パスを登録しない
typeset -U path cdpath fpath manpath

# 自動アップデート用設定
export DISABLE_UPDATE_PROMPT=true
export UPDATE_ZSH_DAYS=13
$HOME/.dotfiles/lib/auto_update.sh

# oh-my-zsh
# export ZSH=$HOME/.dotfiles/oh-my-zsh
# [ -f ~/.zshrc.local ] && source ~/.zshrc.local
# source $ZSH/oh-my-zsh.sh

# sheldon
eval "$(sheldon source)"

for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf};
done

if [[ `uname` == 'Darwin' ]]; then
    export PATH="/usr/local/sbin:$PATH"
fi

if  (which go >/dev/null 2>&1) ; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi
