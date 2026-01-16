#!/bin/bash
# task.sh - プロジェクトタスク管理CLI
# Usage: task.sh <command> [args...]

set -e

# 定数
TASKS_FILE=".claude/tasks.json"
COMPLETED_FILE=".claude/completed.json"

# ヘルパー関数
ensure_tasks_file() {
    if [ ! -f "$TASKS_FILE" ]; then
        mkdir -p "$(dirname "$TASKS_FILE")"
        echo '{"version":"1.0","tasks":[]}' > "$TASKS_FILE"
    fi
}

ensure_completed_file() {
    if [ ! -f "$COMPLETED_FILE" ]; then
        mkdir -p "$(dirname "$COMPLETED_FILE")"
        echo '{"version":"1.0","tasks":[]}' > "$COMPLETED_FILE"
    fi
}

generate_id() {
    openssl rand -hex 4
}

now_iso() {
    date -u +"%Y-%m-%dT%H:%M:%SZ"
}

# コマンド: add
cmd_add() {
    local title="$1"
    local description="${2:-}"
    local type="${3:-}"

    if [ -z "$title" ]; then
        echo "Usage: task.sh add <title> [description] [type]" >&2
        exit 1
    fi

    ensure_tasks_file
    local id=$(generate_id)
    local now=$(now_iso)

    # Build task object based on provided fields
    if [ -n "$type" ] && [ -n "$description" ]; then
        local task_obj='{ id: $id, title: $title, status: "pending", type: $type, description: $description, createdAt: $now }'
    elif [ -n "$type" ]; then
        local task_obj='{ id: $id, title: $title, status: "pending", type: $type, createdAt: $now }'
    elif [ -n "$description" ]; then
        local task_obj='{ id: $id, title: $title, status: "pending", description: $description, createdAt: $now }'
    else
        local task_obj='{ id: $id, title: $title, status: "pending", createdAt: $now }'
    fi

    jq --arg id "$id" \
       --arg title "$title" \
       --arg description "$description" \
       --arg type "$type" \
       --arg now "$now" \
       ".tasks += [$task_obj]" "$TASKS_FILE" > "$TASKS_FILE.tmp" && mv "$TASKS_FILE.tmp" "$TASKS_FILE"

    echo "$id"
}

# コマンド: list
cmd_list() {
    local filter1="${1:-all}"
    local filter2="${2:-}"

    ensure_tasks_file
    ensure_completed_file

    # Determine filters (status and/or type)
    local status_filter=""
    local type_filter=""

    for arg in "$filter1" "$filter2"; do
        [ -z "$arg" ] && continue
        case "$arg" in
            pending|in_progress|completed)
                status_filter="$arg"
                ;;
            bug|feature|refactor|review|docs|test)
                type_filter="$arg"
                ;;
            all)
                ;;
            *)
                echo "Unknown filter: $arg" >&2
                exit 1
                ;;
        esac
    done

    # completed の場合は completed.json から読み込み
    local target_file="$TASKS_FILE"
    if [ "$status_filter" = "completed" ]; then
        target_file="$COMPLETED_FILE"
    fi

    # Build jq filter
    if [ -n "$status_filter" ] && [ -n "$type_filter" ]; then
        jq -r --arg s "$status_filter" --arg t "$type_filter" \
           '.tasks[] | select(.status == $s and .type == $t) | "\(.id) [\(.status)] [\(.type // "none")] \(.title)"' "$target_file"
    elif [ -n "$status_filter" ]; then
        jq -r --arg s "$status_filter" \
           '.tasks[] | select(.status == $s) | "\(.id) [\(.status)] [\(.type // "none")] \(.title)"' "$target_file"
    elif [ -n "$type_filter" ]; then
        jq -r --arg t "$type_filter" \
           '.tasks[] | select(.type == $t) | "\(.id) [\(.status)] [\(.type // "none")] \(.title)"' "$target_file"
    else
        jq -r '.tasks[] | "\(.id) [\(.status)] [\(.type // "none")] \(.title)"' "$target_file"
    fi
}

# コマンド: update
cmd_update() {
    local id="$1"
    local field="$2"
    local value="$3"

    if [ -z "$id" ] || [ -z "$field" ] || [ -z "$value" ]; then
        echo "Usage: task.sh update <id> <field> <value>" >&2
        exit 1
    fi

    ensure_tasks_file
    local now=$(now_iso)

    jq --arg id "$id" \
       --arg field "$field" \
       --arg value "$value" \
       --arg now "$now" \
       '(.tasks[] | select(.id == $id)) |= . + {($field): $value, updatedAt: $now}' \
       "$TASKS_FILE" > "$TASKS_FILE.tmp" && mv "$TASKS_FILE.tmp" "$TASKS_FILE"
}

# コマンド: done
cmd_done() {
    local id="$1"

    if [ -z "$id" ]; then
        echo "Usage: task.sh done <id>" >&2
        exit 1
    fi

    ensure_tasks_file
    ensure_completed_file
    local now=$(now_iso)

    # タスクを取得して completedAt を付与
    local task=$(jq --arg id "$id" --arg now "$now" \
        '.tasks[] | select(.id == $id) | . + {status: "completed", completedAt: $now, updatedAt: $now}' \
        "$TASKS_FILE")

    if [ -z "$task" ]; then
        echo "Task not found: $id" >&2
        exit 1
    fi

    # completed.json に追加
    jq --argjson task "$task" '.tasks += [$task]' \
        "$COMPLETED_FILE" > "$COMPLETED_FILE.tmp" && mv "$COMPLETED_FILE.tmp" "$COMPLETED_FILE"

    # tasks.json から削除
    jq --arg id "$id" '.tasks |= map(select(.id != $id))' \
        "$TASKS_FILE" > "$TASKS_FILE.tmp" && mv "$TASKS_FILE.tmp" "$TASKS_FILE"
}

# コマンド: delete
cmd_delete() {
    local id="$1"

    if [ -z "$id" ]; then
        echo "Usage: task.sh delete <id>" >&2
        exit 1
    fi

    ensure_tasks_file
    jq --arg id "$id" '.tasks |= map(select(.id != $id))' \
       "$TASKS_FILE" > "$TASKS_FILE.tmp" && mv "$TASKS_FILE.tmp" "$TASKS_FILE"
}

# コマンド: clear
cmd_clear() {
    local target="${1:-completed}"

    ensure_tasks_file
    ensure_completed_file

    case "$target" in
        completed)
            echo '{"version":"1.0","tasks":[]}' > "$COMPLETED_FILE"
            ;;
        all)
            echo '{"version":"1.0","tasks":[]}' > "$TASKS_FILE"
            echo '{"version":"1.0","tasks":[]}' > "$COMPLETED_FILE"
            ;;
        *)
            echo "Usage: task.sh clear [completed|all]" >&2
            exit 1
            ;;
    esac
}

# コマンド: show
cmd_show() {
    local id="$1"

    if [ -z "$id" ]; then
        echo "Usage: task.sh show <id>" >&2
        exit 1
    fi

    ensure_tasks_file
    ensure_completed_file

    # tasks.json から探す
    local result=$(jq --arg id "$id" '.tasks[] | select(.id == $id)' "$TASKS_FILE")

    # 見つからなければ completed.json から探す
    if [ -z "$result" ]; then
        result=$(jq --arg id "$id" '.tasks[] | select(.id == $id)' "$COMPLETED_FILE")
    fi

    echo "$result"
}

# メインディスパッチ
case "${1:-}" in
    add)     shift; cmd_add "$@" ;;
    list)    shift; cmd_list "$@" ;;
    update)  shift; cmd_update "$@" ;;
    done)    shift; cmd_done "$@" ;;
    delete)  shift; cmd_delete "$@" ;;
    clear)   shift; cmd_clear "$@" ;;
    show)    shift; cmd_show "$@" ;;
    *)
        echo "Usage: task.sh <command> [args...]" >&2
        echo "Commands: add, list, update, done, delete, clear, show" >&2
        exit 1
        ;;
esac
