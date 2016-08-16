### Aliases ###
alias ..='cd ..'
alias ld='ls -ld'          # Show info about the directory
alias lla='ls -lAF'        # Show hidden all files
alias ll='ls -lF'          # Show long file information
alias l='ls -1F'           # Show long file information
alias la='ls -AF'          # Show hidden files
alias lx='ls -lXB'         # Sort by extension
alias lk='ls -lShr'         # Sort by size, biggest last
alias lc='ls -ltcr'        # Sort by and show change time, most recent last
alias lu='ls -ltur'        # Sort by and show access time, most recent last
alias lt='ls -ltr'         # Sort by date, most recent last
alias lr='ls -lR'          # Recursive ls
alias grep='grep --color=auto'
alias history='history -E'
alias s='screen'
alias emacs-kill='emacsclient -e "(kill-emacs)"'
alias zshrc='$EDITOR_CMD ~/.zshrc'
alias tf='tail -f'
alias g='git'
alias db_rollback='rake db:rollback'
alias db_migrate='rake db:migrate'
alias tail_log='tail -f *.log'
alias ssh-aa='eval `ssh-agent -s` ; ssh-add'
alias less="less -qnR"
alias tm="tmux"
alias tmkss="tmux kill-session"
alias tmksr="tmux kill-server"
alias m="mux"
alias relogin='exec $SHELL -l'
alias -g L='`git log --decorate --oneline | peco | cut -d" " -f1`'
alias -g LA='`git log --decorate --oneline --all | peco | cut -d" " -f1`'
alias -g R='`git reflog | peco | cut -d" " -f1`'
alias dotfiles='cd ~/.dotfiles'
alias sshdir='cd ~/.ssh'

#colordiff設定
if [[ -x `which colordiff` ]]; then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

## emacsclient をシームレスに使うための関数
## http://k-ui.jp/?p=243
function e(){
    echo "[$0] emacsclient -c -t $*";
    (emacsclient -c -t $* ||
            (echo "[$0] emacs --daemon"; emacs --daemon &&
                 (echo "[$0] emacsclient -c -t $*"; emacsclient -c -t $*)) ||
            (echo "[$0] emacs $*"; emacs $*))
}

# 標準出力をクリップボードにコピー
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    alias -g C='| putclip'
fi
