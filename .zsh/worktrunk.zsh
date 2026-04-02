# Worktrunk workflow helpers
# /wt スキルをターミナルから1コマンドで起動する

function wt-init() {
    claude --model sonnet "/wt init"
}

function wt-switch() {
    if [[ $# -eq 0 ]]; then
        echo "Usage: wt-switch <task description>" >&2
        return 1
    fi
    claude --model sonnet "/wt switch $*"
}

function wt-home() {
    wt switch '^'
}
