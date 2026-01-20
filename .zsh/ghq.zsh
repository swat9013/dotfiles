# ghq + fzy 連携設定

# ghq + fzyでリポジトリを選択してcd（最近アクセスしたリポジトリを優先表示）
function ghq-fzy() {
    local ghq_root="$(ghq root)"
    local repo=$(
        {
            autoload -Uz chpwd_recent_filehandler
            chpwd_recent_filehandler
            for dir in $reply; do
                if [[ "$dir" == "$ghq_root"/* ]]; then
                    echo "${dir#$ghq_root/}"
                fi
            done
            ghq list
        } | awk '!seen[$0]++' | fzy
    )
    if [ -n "$repo" ]; then
        cd "$ghq_root/$repo"
    fi
}

# エイリアス
alias gf='ghq-fzy'
alias gg='ghq get'
