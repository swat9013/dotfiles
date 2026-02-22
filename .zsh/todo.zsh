#
# AI Coding / Todo Management
#

function _init-work() {
    [[ ! -d ".work" ]] && mkdir -p .work
    [[ ! -f ".work/.gitignore" ]] && echo '*' > .work/.gitignore
}

function _cache_todo_path() {
    local path="$1"
    local cache_dir="$HOME/.dotfiles/.cache"
    local cache_file="$cache_dir/todo-paths.txt"

    # 絶対パスに変換
    path=$(realpath "$path" 2>/dev/null) || return 0
    [[ ! -f "$path" ]] && return 0

    # キャッシュディレクトリ確保
    mkdir -p "$cache_dir"

    # アトミック追加（重複排除）
    {
        cat "$cache_file" 2>/dev/null
        echo "$path"
    } | awk '!seen[$0]++' > "$cache_file.tmp" && mv "$cache_file.tmp" "$cache_file"
}

function te() {
    _init-work
    e .work/todo.md
    _cache_todo_path "$PWD/.work/todo.md"
}

function tre() {
    local cache_dir="$HOME/.dotfiles/.cache"
    local cache_file="$cache_dir/todo-paths.txt"

    echo "Searching for .work/todo.md files in ~/ ..." >&2
    mkdir -p "$cache_dir"

    local tmp_file="${cache_file}.rebuild"
    : > "$tmp_file"

    find ~ -maxdepth 5 -name '.work' -type d 2>/dev/null | while read -r d; do
        [[ -f "$d/todo.md" ]] && echo "$d/todo.md"
    done >> "$tmp_file"

    awk '!seen[$0]++' "$tmp_file" > "$cache_file.tmp" && \
        mv "$cache_file.tmp" "$cache_file" && \
        rm -f "$tmp_file"

    local count=$(wc -l < "$cache_file" | tr -d ' ')
    echo "Found $count todo.md files" >&2
}

# bk() の退避対象ファイル（追加はここに）
_bk_targets=(
    review.md
    plan.md
    implementation.md
    report.md
    review-fix.md
    discovery.md
)

function bk() {
    _init-work
    local branch=$(git symbolic-ref --short HEAD 2>/dev/null | tr '/' '-')
    local d=".work/$(date +%Y%m%d_%H%M%S)${branch:+_$branch}"
    mkdir -p "$d"
    mv "${_bk_targets[@]}" "$d/" 2>/dev/null
}

function tl() {
    [[ -x /opt/homebrew/bin/fzf ]] || { echo "Error: fzf is required" >&2; return 1 }

    local cache_file="$HOME/.dotfiles/.cache/todo-paths.txt"

    # キャッシュがない場合はリフレッシュ
    if [[ ! -s "$cache_file" ]]; then
        echo "Cache not found. Rebuilding..." >&2
        tre || return 1
    fi

    # cdr履歴を取得（順序付きリスト）
    autoload -Uz chpwd_recent_filehandler
    chpwd_recent_filehandler
    local -a recent_dirs=("${reply[@]}")

    # キャッシュからプロジェクト一覧を作成
    local -a entries=()
    local -A path_map=()
    while IFS= read -r todo_path; do
        [[ -f "$todo_path" ]] || continue
        local project_root="${todo_path:h:h}"
        local project_name="${project_root:t}"
        local parent_name="${project_root:h:t}"
        local display="${parent_name}/${project_name}"
        entries+=("$display")
        path_map[$display]="$project_root"
    done < "$cache_file"

    # cdr履歴順でソート（履歴にないものはアルファベット順で後ろ）
    local -a sorted=()
    for dir in "${recent_dirs[@]}"; do
        for entry in "${entries[@]}"; do
            if [[ "${path_map[$entry]}" == "$dir" ]]; then
                sorted+=("$entry")
                break
            fi
        done
    done
    # 履歴にないものをアルファベット順で追加
    for entry in "${(o)entries[@]}"; do
        (( ${sorted[(I)$entry]} )) || sorted+=("$entry")
    done

    # fzyで選択
    local selected=$(printf '%s\n' "${sorted[@]}" | /opt/homebrew/bin/fzf --height=20)
    [[ -z "$selected" ]] && return 0

    [[ -n "${path_map[$selected]}" ]] && cd "${path_map[$selected]}"
}

function tll() {
    local cache_file="$HOME/.dotfiles/.cache/todo-paths.txt"

    # キャッシュがない場合はリフレッシュ
    if [[ ! -s "$cache_file" ]]; then
        echo "Cache not found. Rebuilding..." >&2
        tre || return 1
    fi

    while IFS= read -r todo_path; do
        [[ -f "$todo_path" ]] || continue
        # 未完了タスクがなければスキップ
        grep -qE '^[[:space:]]*[-*][[:space:]]+\[[[:space:]]\]' "$todo_path" || continue
        local project_root="${todo_path:h:h}"
        local project_name="${project_root:t}"
        local parent_name="${project_root:h:t}"
        echo "=== ${parent_name}/${project_name} ==="
        cat "$todo_path"
        echo ""
    done < "$cache_file"
}

function tt() {
    (( $+commands[fzf] )) || { echo "Error: fzf is required" >&2; return 1 }
    (( $+commands[claude] )) || { echo "Error: claude is required" >&2; return 1 }

    local todo_file=".work/todo.md"
    [[ ! -f "$todo_file" ]] && { echo "Error: $todo_file not found" >&2; return 1 }

    # 未完了タスクを抽出
    local tasks=$(grep -E '^[[:space:]]*[-*][[:space:]]+\[[[:space:]]\]' "$todo_file")
    [[ -z "$tasks" ]] && { echo "No incomplete tasks found" >&2; return 0 }

    # fzf で選択
    local selected=$(echo "$tasks" | fzf)
    [[ -z "$selected" ]] && return 0

    # チェックボックス部分を除去
    local task_text=$(echo "$selected" | sed 's/^[[:space:]]*[-*][[:space:]]*\[[[:space:]]\][[:space:]]*//')

    # Claude Code 起動
    claude -- "$task_text"
}
