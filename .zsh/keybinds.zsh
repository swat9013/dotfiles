## zsh のキーバインドを環境変数 EDITOR に関わらず emacs 風にする
bindkey -e

if which fzy > /dev/null 2>&1; then

    function fzy-select-history() {
        local tac
        if which tac > /dev/null; then
            tac="tac"
        else
            tac="tail -r"
        fi
        BUFFER=$(\history -n 1 | eval $tac | awk '!a[$0]++'| fzy --query "$LBUFFER")
        CURSOR=$#BUFFER
        zle reset-prompt
    }
    zle -N fzy-select-history
    bindkey '^r' fzy-select-history

    # cdrの有効化
    if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
      autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
      add-zsh-hook chpwd chpwd_recent_dirs
      zstyle ':completion:*' recent-dirs-insert both
      zstyle ':chpwd:*' recent-dirs-default true
      zstyle ':chpwd:*' recent-dirs-max 1000
    fi

    function fzy-go-to-dir () {
        local line
        local selected="$(
      {
        (
          autoload -Uz chpwd_recent_filehandler
          chpwd_recent_filehandler && for line in $reply; do
            if [[ -d "$line" ]]; then
              echo "$line"
            fi
          done
        )
        for line in *(-/) ${^cdpath}/*(N-/); do echo "$line"; done | sort -u
      } | fzy --query "$LBUFFER"
    )"
        if [ -n "$selected" ]; then
            BUFFER="cd ${(q)selected}"
            zle accept-line
        fi
        zle reset-prompt
    }
    zle -N fzy-go-to-dir
    bindkey '^s' fzy-go-to-dir
fi

## C-^ で一つ上のディレクトリへ
function cdup() {
    echo
    cd ..
    echo
    zle reset-prompt
}
zle -N cdup
bindkey '^^' cdup

function do_enter() {
    if [ -n "$BUFFER" ]; then
        zle accept-line
        return 0
    fi
    echo
    ls
    # ↓おすすめ
    # ls_abbrev
    if [ "$(git rev-parse --is-inside-work-tree 2> /dev/null)" = 'true' ]; then
        # echo -e "\e[0;33m--- git status ---\e[0m"
        echo
        git status --short --branch
    fi
    echo
    zle reset-prompt
    return 0
}
zle -N do_enter
bindkey '^m' do_enter

#cd 後のlsの省略
function chpwd() { ls }

## Ctrl+v でカレントディレクトリをVSCodeで開く
function open-vscode() {
    code .
    zle reset-prompt
}
zle -N open-vscode
bindkey '^v' open-vscode

#スクリーンロックを無効化
stty stop undef

## サスペンド無効化
stty susp undef
