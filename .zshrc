## 重複パスを登録しない
typeset -U path cdpath fpath manpath

# 自動アップデート用設定
export DISABLE_UPDATE_PROMPT=true
export UPDATE_ZSH_DAYS=13
$HOME/.dotfiles/lib/auto_update.sh &!

## 補完を有効化（24時間以上経過時のみ再生成）
autoload -Uz compinit
if [[ -n ~/.zcompdump(N.mh+24) ]]; then
    compinit
else
    compinit -C
fi
autoload -U +X bashcompinit && bashcompinit

# sheldon（キャッシュファイル方式）
sheldon_cache="${XDG_CACHE_HOME:-$HOME/.cache}/sheldon/sheldon.zsh"
sheldon_toml="${XDG_CONFIG_HOME:-$HOME/.config}/sheldon/plugins.toml"
if [[ ! -r "$sheldon_cache" || "$sheldon_toml" -nt "$sheldon_cache" ]]; then
    mkdir -p "${sheldon_cache:h}"
    sheldon source > "$sheldon_cache"
fi
source "$sheldon_cache"
unset sheldon_cache sheldon_toml

eval "$(starship init zsh)"

for conf in $HOME/.dotfiles/.zsh/*.zsh; do
    source ${conf};
done

if [[ `uname` == 'Darwin' ]]; then
    export PATH="/usr/local/sbin:$PATH"
fi

# Go（デフォルトGOPATH使用、外部コマンド回避）
[[ -d "$HOME/go/bin" ]] && export PATH="$HOME/go/bin:$PATH"

if  (which asdf >/dev/null 2>&1) ; then
    export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
fi

export PATH="$HOME/.local/bin:$PATH"

if [[ -n $GHOSTTY_RESOURCES_DIR ]]; then
   source "$GHOSTTY_RESOURCES_DIR/shell-integration/zsh/ghostty-integration"
fi

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/s-watanabe/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
