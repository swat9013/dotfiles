### Aliases ###
alias history='history -E'
alias ll='ls -l'
alias la='ls -a'
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
alias m="mux"

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
