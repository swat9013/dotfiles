# ghq + fzy 連携設定

# ghq + fzyでリポジトリを選択してcd
function ghq-fzy() {
    local repo=$(ghq list | fzy)
    if [ -n "$repo" ]; then
        cd "$(ghq root)/$repo"
    fi
}

# エイリアス
alias gf='ghq-fzy'
alias gg='ghq get'
