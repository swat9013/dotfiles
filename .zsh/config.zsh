## 保管を有効化
## fpath=(~/dotfiles/zsh-completions $fpath)
autoload -Uz compinit; compinit -c
autoload -Uz colors; colors

## cdr
if [[ -n $(echo ${^fpath}/chpwd_recent_dirs(N)) && -n $(echo ${^fpath}/cdr(N)) ]]; then
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
  add-zsh-hook chpwd chpwd_recent_dirs
  zstyle ':completion:*:*:cdr:*:*' menu selection
  zstyle ':completion:*' recent-dirs-insert both
  zstyle ':chpwd:*' recent-dirs-max 500
  zstyle ':chpwd:*' recent-dirs-default true
  # zstyle ':chpwd:*' recent-dirs-file "${XDG_CACHE_HOME:-$HOME/.cache}/shell/chpwd-recent-dirs"
  zstyle ':chpwd:*' recent-dirs-pushd true
fi

hosts=( ${(@)${${(M)${(s:# :)${(zj:# :)${(Lf)"$([[ -f ~/.ssh/config ]] && < ~/.ssh/config)"}%%\#*}}##host(|name) *}#host(|name) }/\*} )
zstyle ':completion:*:hosts' hosts $hosts

## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1

#スクリーンロックを無効化
stty stop undef

## サスペンド無効化
stty susp undef

#cd 後のlsの省略
function chpwd() { ls }

## 補完候補に色を付ける
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

## title
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

# http://hoppsjots.org/?p=177
# Fix Underline + Color in TMUX (TERM=screen-256color)
$({ infocmp -x screen-256color; printf '\t%s\n' 'ncv@,'; } > /tmp/t && tic -x /tmp/t)
