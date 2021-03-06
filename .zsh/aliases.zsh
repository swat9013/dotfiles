
### Aliases ###
alias nuget="mono /usr/local/bin/nuget.exe"
alias -g L='`git log --decorate --oneline | peco | cut -d" " -f1`'
alias -g LA='`git log --decorate --oneline --all | peco | cut -d" " -f1`'
alias -g R='`git reflog | peco | cut -d" " -f1`'
alias ..='cd ..'
alias attach='docker attach webapplication_web_1'
alias brew="PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/sbin brew"
alias con='docker-compose run --rm web bundle exec rails c'
alias db_migrate='rake db:migrate'
alias db_rollback='rake db:rollback'
alias dcew='docker-compose exec web'
alias dcewtest='docker-compose exec web rails test'
alias dclog='COMPOSE_HTTP_TIMEOUT=30000 docker-compose logs -f'
alias dcr='docker-compose run --rm'
alias dcrw='docker-compose run --rm web'
alias dcrw-rails='docker-compose run --rm web bundle exec rails'
alias dcrwrubo-branch='docker-compose run --rm web bundle exec rubocop -a --force-exclusion $(git diff --name-only --diff-filter=AMRC origin/master HEAD) $(git status --porcelain | grep -v "^ D " | sed s/^...//)'
alias dcrwrubo-cache='docker-compose run --rm web bundle exec rubocop -a --force-exclusion $( git diff --cached --name-only)'
alias dcrwrubo-diff='docker-compose run --rm web bundle exec rubocop -a --force-exclusion $( git diff --name-only --diff-filter=AMRC)'
alias dcrwrubo-status='docker-compose run --rm web bundle exec rubocop -a --force-exclusion $( git status --porcelain | grep -v "^ D " | sed s/^...// | paste -s -)'
alias dcrwrubo='docker-compose run --rm web bundle exec rubocop -a'
alias dcrwtest='docker-compose run --rm web bundle exec rails test'
alias dcud='docker-compose up -d'
alias dotfiles='cd ~/.dotfiles'
alias emacs-kill='emacsclient -e "(kill-emacs)"'
alias g='git'
alias grep='grep --color=auto'
alias gtags-setup='gtags --gtagslabel=pygments'
alias history='history -E'
alias l='ls -1F'           # Show long file information
alias la='ls -AF'          # Show hidden files
alias lc='ls -ltcr'        # Sort by and show change time, most recent last
alias ld='ls -ld'          # Show info about the directory
alias less="less -qnR"
alias lk='ls -lShr'         # Sort by size, biggest last
alias ll='ls -lF'          # Show long file information
alias lla='ls -lAF'        # Show hidden all files
alias lr='ls -lR'          # Recursive ls
alias lt='ls -ltr'         # Sort by date, most recent last
alias lu='ls -ltur'        # Sort by and show access time, most recent last
alias lx='ls -lXB'         # Sort by extension
alias p='python'
alias ps-grep="ps aux | grep"
alias relogin='exec $SHELL -l'
alias rubo-branch='rubocop -a --force-exclusion $(git diff --name-only --diff-filter=AMRC origin/master HEAD) $(git status --porcelain | grep -v "^ D " | sed s/^...//)'
alias s='screen'
alias sed-filename='(){find ./ -type f | sed \"p;s/$1/$2/\" | xargs -n2 mv}'
alias ssh='~/.dotfiles/lib/ssh_host_color.sh'
alias ssh-aa='eval `ssh-agent -s` ; ssh-add'
alias sshdir='cd ~/.ssh'
alias stop='docker-compose stop'
alias tf='tail -f'
alias tm="tmux"
alias tmksr="tmux kill-server"
alias tmkss="tmux kill-session"
alias up='docker-compose up -d'
alias zshrc='$EDITOR_CMD ~/.zshrc'
alias marp-w='docker run --rm --init -v $PWD:/home/marp/app/ -e LANG=$LANG -p 37717:37717 marpteam/marp-cli -w --html $*'
alias marp-convert-pdf='docker run --rm --init -v $PWD:/home/marp/app/ -e LANG=$LANG marpteam/marp-cli --allow-local-files --html --pdf $*'
alias log-start='now=`date +%Y%m%d%H%M%S`;dirpath=~/scriptlogs; logpath=${dirpath}/script_${now}.log; mkdir -p ${dirpath}; script ${logpath}'
alias ide="~/.dotfiles/ide.sh"
alias pip-upgrade-all="pip list -o | tail -n +3 | awk '{ print \$1 }' | xargs pip install -U"

#colordiff設定
if which colordiff >/dev/null 2>&1 ;then
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

#rmtrash
if which rmtrash >/dev/null 2>&1 ;then
    alias rm='rmtrash'
fi
