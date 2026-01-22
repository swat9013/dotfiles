# ファイル検索 + エディタで開く (rg --files + fzy)
function ef() {
    local file=$(rg --files --hidden --follow --glob '!.git' 2>/dev/null | fzy)
    if [ -n "$file" ]; then
        ${EDITOR:-vim} "$file"
    fi
}
