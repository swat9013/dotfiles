# bindkey "^R" history-incremental-search-backward
# bindkey "^S" history-incremental-search-forward

## zsh のキーバインドを環境変数 EDITOR に関わらず emacs 風にする
bindkey -e

if which peco > /dev/null 2>&1; then

    function peco-select-history() {
        local tac
        if which tac > /dev/null; then
            tac="tac"
        else
            tac="tail -r"
        fi
        BUFFER=$(\history -n 1 | eval $tac | awk '!a[$0]++'| peco --query "$LBUFFER")
        CURSOR=$#BUFFER
        # zle clear-screen
        # zle reset-screen
        zle reset-prompt
    }
    zle -N peco-select-history
    bindkey '^r' peco-select-history

    function peco-go-to-dir () {
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
        # ghq list --full-path
        for line in *(-/) ${^cdpath}/*(N-/); do echo "$line"; done | sort -u
      } | peco --query "$LBUFFER"
    )"
        if [ -n "$selected" ]; then
            BUFFER="cd ${(q)selected}"
            zle accept-line
        fi
        zle reset-prompt
    }
    zle -N peco-go-to-dir
    bindkey '^f' peco-go-to-dir
fi

if [ "$(uname)" != "Darwin" ]; then
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
fi
