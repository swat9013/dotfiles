
### Aliases ###
#
# linux
#
alias grep='grep --color=auto'
alias ps-grep="ps aux | grep"
alias relogin='exec $SHELL -l'
alias sed-filename='(){find ./ -type f | sed \"p;s/$1/$2/\" | xargs -n2 mv}'
alias tf='tail -f'

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

#colordiff設定
if which colordiff >/dev/null 2>&1 ;then
    alias diff='colordiff -u'
else
    alias diff='diff -u'
fi

#rmtrash
if which rmtrash >/dev/null 2>&1 ;then
    alias rm='rmtrash'
fi

#
# editor
#
alias c='code'
alias emacs-kill='emacsclient -e "(kill-emacs)"'

# emacsclient をシームレスに使うための関数
# http://k-ui.jp/?p=243
function e(){
    echo "[$0] emacsclient -c -t $*";
    (emacsclient -c -t $* ||
            (echo "[$0] emacs --daemon"; emacs --daemon &&
                 (echo "[$0] emacsclient -c -t $*"; emacsclient -c -t $*)) ||
            (echo "[$0] emacs $*"; emacs $*))
}

#
# ssh
#
alias ssh-aa='eval `ssh-agent -s` ; ssh-add'
# 補完が効かないので一時的に無効にする
# alias sshc='~/.dotfiles/lib/ssh_host_color.sh'
alias sshdir='cd ~/.ssh'

#
# tmux
#
alias tm="tmux"
alias tmksr="tmux kill-server"
alias tmkss="tmux kill-session"
alias ide="~/.dotfiles/lib/ide.sh"

#
# git
#
alias g='git'
alias -g L='`git log --decorate --oneline | peco | cut -d" " -f1`'
alias -g LA='`git log --decorate --oneline --all | peco | cut -d" " -f1`'
alias -g R='`git reflog | peco | cut -d" " -f1`'

#
# ls
#
alias lr='ls -lR'          # Recursive ls
alias lt='ls -ltr'         # Sort by date, most recent last
alias lu='ls -ltur'        # Sort by and show access time, most recent last
alias lx='ls -lXB'         # Sort by extension
alias l='ls -1F'           # Show long file information
alias la='ls -AF'          # Show hidden files
alias lc='ls -ltcr'        # Sort by and show change time, most recent last
alias ld='ls -ld'          # Show info about the directory
alias less="less -qnR"
alias lk='ls -lShr'         # Sort by size, biggest last
alias ll='ls -lF'          # Show long file information
alias lla='ls -lAF'        # Show hidden all files

#
# marp
#
alias marp-convert-pdf='docker run --rm --init -v $PWD:/home/marp/app/ -e LANG=$LANG marpteam/marp-cli --allow-local-files --html --pdf $*'
alias marp-w='docker run --rm --init -v $PWD:/home/marp/app/ -e LANG=$LANG -p 37717:37717 marpteam/marp-cli -w --html $*'

#
# ruby
#
alias rubo-branch='rubocop -a --force-exclusion $(git diff --name-only --diff-filter=AMRC origin/master HEAD) $(git status --porcelain | grep -v "^ D " | sed s/^...//)'

#
# rails
#
alias con='docker-compose run --rm web bundle exec rails c'
alias db_migrate='rake db:migrate'
alias db_rollback='rake db:rollback'

#
# docker
#
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
alias attach='docker attach webapplication_web_1'
alias up='docker-compose up -d'
alias stop='docker-compose stop'

#
# repository scripts
#
alias lint="./script/lint.sh"
alias build="./script/build.sh"
alias setup="./script/setup.sh"

#
# python
#
alias pip-upgrade-all="pip list -o | tail -n +3 | awk '{ print \$1 }' | xargs pip install -U"

#
# vibe-kanban
#
alias vk="~/.dotfiles/scripts/vibe-kanban.sh"
