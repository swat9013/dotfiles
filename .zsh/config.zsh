## 保管を有効化
## fpath=(~/dotfiles/zsh-completions $fpath)
autoload -Uz compinit; compinit -c

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1

autoload -Uz colors; colors

#スクリーンロックを無効化
stty stop undef

## サスペンド無効化
stty susp undef

#cd 後のlsの省略
function chpwd() { ls }

#
# Goolge Search by Google Chrome
# terminalからググったりqiita検索をできる
#
google() {
    local str opt
    if [ $# != 0 ]; then
        for i in $*; do
            # $strが空じゃない場合、検索ワードを+記号でつなぐ(and検索)
            str="$str${str:++}$i"
        done
        opt='search?num=100'
        opt="${opt}&q=${str}"
    fi
    open -a Google\ Chrome http://www.google.co.jp/$opt
}

## 補完候補に色を付ける
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}


# ------------------------------
## Other Settings
# ------------------------------

case "$TERM" in
    kterm*|xterm*)
        precmd() {
            printf "\e]0;${USER}@${HOST%%.*}:${PWD}\a"
            psvar=()
        }
        ;;
    screen*|ansi*)
        preexec() {
            printf "\eP\e]0;${USER}@${HOST%%.*}:${PWD}\a\e\\"
            #printf "\eP\e]0;!${1%% *}\a\e\\"
            printf "\ek#${1%% *}\e\\"
        }
        precmd() {
            printf "\eP\e]0;${USER}@${HOST%%.*}:${PWD}\a\e\\"
            #printf "\eP\e]0;~$(basename $(pwd))\a\e\\"
            printf "\ek$(basename $(pwd))\e\\"
            psvar=()
        }
        ;;
esac
