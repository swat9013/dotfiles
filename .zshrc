export EDITOR=emacs

autoload -Uz compinit
compinit
HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000
setopt auto_cd
setopt auto_pushd
setopt prompt_subst
setopt hist_ignore_dups
setopt auto_param_slash
setopt mark_dirs
setopt auto_list
bindkey "^R" history-incremental-search-backward
bindkey "^S" history-incremental-search-forward
setopt hist_no_store
setopt hist_save_no_dups
setopt hist_reduce_blanks

#日本語ファイルの表示
setopt print_eight_bit

autoload -Uz colors
colors
setopt share_history
#setopt correct

# zsh のキーバインドを環境変数 EDITOR に関わらず emacs 風にする
bindkey -e

# cdコマンド実行後、lsを実行する
function cd() {
    builtin cd $@ && ls;
}

#スクリーンロックを無効化
stty stop undef

# サスペンド無効化
stty susp undef

# C-^ で一つ上のディレクトリへ
function cdup() {
    echo
    cd .. && ls
    zle reset-prompt
}
zle -N cdup
bindkey '^^' cdup

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# ------------------------------
# Look And Feel Settings
# ------------------------------
### Ls Color ###
# 色の設定
export LSCOLORS=Exfxcxdxbxegedabagacad
# 補完時の色の設定
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
# ZLS_COLORSとは？
export ZLS_COLORS=$LS_COLORS
# lsコマンド時、自動で色がつく(ls -Gのようなもの？)
export CLICOLOR=true
# 補完候補に色を付ける
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

### Prompt ###
# プロンプトに色を付ける
autoload -U colors; colors

# 一般ユーザ時
tmp_prompt="%F{cyan}[%n@%m${WINDOW:+"[$WINDOW]"}]%f-%{${fg[green]}%}[%~]%{${reset_color}%}-%F{magenta}[%D{%m/%d %T}]%f
%#"
#tmp_prompt="%{${fg[cyan]}%}%n%# %{${reset_color}%}"
tmp_prompt2="%{${fg[cyan]}%}%_> %{${reset_color}%}"
tmp_rprompt="%{${fg[green]}%}[%~]%{${reset_color}%}"
tmp_sprompt="%{${fg[yellow]}%}%r is correct? [Yes, No, Abort, Edit]:%{${reset_color}%}"

# rootユーザ時(太字にし、アンダーバーをつける)
if [ ${UID} -eq 0 ]; then
    tmp_prompt="%B%U${tmp_prompt}%u%b"
    tmp_prompt2="%B%U${tmp_prompt2}%u%b"
    tmp_rprompt="%B%U${tmp_rprompt}%u%b"
    tmp_sprompt="%B%U${tmp_sprompt}%u%b"
fi

#gitのブランチの表示
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
RPROMPT="%1(v|%F{green}%1v%f|)"


PROMPT=$tmp_prompt    # 通常のプロンプト
PROMPT2=$tmp_prompt2  # セカンダリのプロンプト(コマンドが2行以上の時に表示される)
#RPROMPT=$tmp_rprompt  # 右側のプロンプト
SPROMPT=$tmp_sprompt  # スペル訂正用プロンプト

# ------------------------------
# Other Settings
# ------------------------------

### Aliases ###
#時刻を表示させる
alias history='history -E'
alias ll='ls -l'
alias la='ls -a'
alias s='screen'
alias emacs-kill='emacsclient -e "(kill-emacs)"'

#colordiff設定
if [[ -x `which colordiff` ]]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

#lessの色設定、改行ありなし等
export LESS='-SRnq'


case "$TERM" in
    kterm*|xterm*)
        precmd() {
            printf "\e]0;${USER}@${HOST%%.*}:${PWD}\a"
            psvar=()
            LANG=en_US.UTF-8 vcs_info
            [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
        }
        ;;
    screen*|ansi*)
        preexec() {
            printf "\eP\e]0;${USER}@${HOST%%.*}:${PWD}\a\e\\"
            #printf "\eP\e]0;!${1%% *}\a\e\\"
            printf "\ek#$1\e\\"
        }
        precmd() {
            printf "\eP\e]0;${USER}@${HOST%%.*}:${PWD}\a\e\\"
            #printf "\eP\e]0;~$(basename $(pwd))\a\e\\"
            printf "\ek$(basename $(pwd))\e\\"
            psvar=()
            LANG=en_US.UTF-8 vcs_info
            [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
        }
        ;;
esac
setopt transient_rprompt


## emacsclient をシームレスに使うための関数
## http://k-ui.jp/?p=243
function e(){
    echo "[$0] emacsclient -c -t $*";
    (emacsclient -c -t $* ||
            (echo "[$0] emacs --daemon"; emacs --daemon &&
                 (echo "[$0] emacsclient -c -t $*"; emacsclient -c -t $*)) ||
            (echo "[$0] emacs $*"; emacs $*))
}
