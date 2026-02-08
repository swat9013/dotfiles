
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
alias emacs-kill-force='pkill -9 emacs'

# emacsclient をシームレスに使うための関数
# http://k-ui.jp/?p=243
# TERM=xterm-256color: Ghosttyのxterm-ghostty terminfoがEmacsデーモンに
# 伝播しないためフォールバック（SGRマウスプロトコル不整合回避）
function e(){
    echo "[$0] emacsclient -c -t $*";
    (TERM=xterm-256color emacsclient -c -t $* ||
            (echo "[$0] emacs --daemon"; emacs --daemon &&
                 (echo "[$0] emacsclient -c -t $*"; TERM=xterm-256color emacsclient -c -t $*)) ||
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
# zellij
#
alias zj="zellij"
alias zja="zellij attach"
alias zjls="zellij list-sessions"
alias zjk="zellij kill-session"
alias zjka="zellij kill-all-sessions"
alias zjide="zellij --layout ide"

#
# git
#
alias g='git'
alias -g L='`git log --decorate --oneline | fzy | cut -d" " -f1`'
alias -g LA='`git log --decorate --oneline --all | fzy | cut -d" " -f1`'
alias -g R='`git reflog | fzy | cut -d" " -f1`'
alias difit='npx difit . --include-untracked'

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
# vibe-kanban
#
alias vk="~/.dotfiles/lib/vibe-kanban.sh"

#
# vpn (OpenFortiVPN - SSL-VPN用)
#
alias fvpn="~/.dotfiles/lib/fortivpn.sh"

#
# AI Coding
#
alias cc='claude'
alias cco='claude --model opus'
alias ccs='claude --model sonnet'

# 軽量Claude Codeでワンライナー質問（ファイル参照オプション対応）
# Usage: cask "質問内容" [file1] [file2] ...
# Example:
#   cask "このコードを説明して" main.py
#   cask "これらのファイルの違いは？" old.js new.js
#   cask "今日の日付は？"
function ccask() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: cask \"質問内容\" [file1] [file2] ..."
        echo "Example: cask \"このコードを説明して\" main.py"
        return 1
    fi

    local prompt="$1"
    shift

    # ファイル引数があれば内容を追加
    if [[ $# -gt 0 ]]; then
        local file_contents=""
        for file in "$@"; do
            if [[ -f "$file" ]]; then
                file_contents="${file_contents}
--- ${file} ---
$(cat "$file")
"
            else
                echo "Warning: '$file' is not a file, skipping." >&2
            fi
        done

        if [[ -n "$file_contents" ]]; then
            prompt="${prompt}

${file_contents}"
        fi
    fi

    claude --model haiku -p "$prompt"
}

#
# wtp (git worktree)
#
function wtp-init() {
    local project_name="${1:-$(basename $(pwd))}"
    if [ -f ".wtp.yml" ]; then
        echo "Error: .wtp.yml already exists"
        return 1
    fi
    sed "s/{{ .ProjectName }}/$project_name/g" ~/.dotfiles/.wtp.yml.template > .wtp.yml
    echo "Created .wtp.yml for project: $project_name"
}
