## 重複パスを登録しない
typeset -U path cdpath fpath manpath

# 自動アップデート用設定
export DISABLE_UPDATE_PROMPT=true
export UPDATE_ZSH_DAYS=13
$HOME/.dotfiles/lib/auto_update.sh &!

## 補完を有効化
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit

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

if  (which asdf >/dev/null 2>&1) ; then
    export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
fi

export PATH="$HOME/.local/bin/:$PATH"

if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
   source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/s-watanabe/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
