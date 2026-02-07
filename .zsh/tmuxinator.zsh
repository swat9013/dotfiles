# tmuxinator 連携設定

# Ctrl+T: tmuxinatorプロジェクトをfzyで選択して起動
function fzy-select-tmuxinator() {
    local selected=$(tmuxinator list | tail -n +2 | sed 's/^ *//' | grep -v '^$' | fzy)
    if [ -n "$selected" ]; then
        BUFFER="tmuxinator start ${selected}"
        zle accept-line
    fi
    zle reset-prompt
}
zle -N fzy-select-tmuxinator
bindkey '^t' fzy-select-tmuxinator

# カレントディレクトリ用のtmuxinator設定を作成
# Usage: init_tmuxinator [project_name]
#   project_name省略時はディレクトリ名を使用
function init_tmuxinator() {
    local project_name="${1:-$(basename "$PWD")}"
    local config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/tmuxinator"
    local config_file="${config_dir}/${project_name}.yml"

    if [[ -f "$config_file" ]]; then
        echo "Already exists: ${config_file}"
        echo "Edit: $EDITOR ${config_file}"
        return 1
    fi

    cat > "$config_file" <<EOF
name: ${project_name}
root: ${PWD}
tmux_options: -2

startup_window: editor

windows:
  - editor:
      layout: main-vertical
      panes:
        - editor:
            - clear
        - console:
            - clear
  - server:
      panes:
        - server:
            - clear
  - shell:
      panes:
        - shell:
            - clear
EOF

    echo "Created: ${config_file}"
    echo "Start: tmuxinator start ${project_name}"
}

# 現在のtmuxセッションをtmuxinator YAML形式で出力
# Usage: dump_tmuxinator [project_name]
#   project_name省略時はセッション名を使用
#   stdoutに出力。ファイル保存は dump_tmuxinator myproj > ~/.config/tmuxinator/myproj.yml
function dump_tmuxinator() {
    if [[ -z "$TMUX" ]]; then
        echo "Not in a tmux session" >&2
        return 1
    fi

    local session_name=$(tmux display-message -p '#{session_name}')
    local name="${1:-${session_name}}"
    local current_window=$(tmux display-message -p '#{window_name}')

    echo "name: ${name}"
    echo "root: ~/"
    echo "tmux_options: -2"
    echo ""
    echo "startup_window: ${current_window}"
    echo ""
    echo "windows:"

    tmux list-windows -F '#{window_index} #{window_name} #{window_layout}' | while read idx wname layout; do
        echo "  - ${wname}:"
        echo "      layout: ${layout}"
        echo "      panes:"
        tmux list-panes -t ":${idx}" -F '#{pane_current_path} #{pane_current_command}' | while read pane_path pane_cmd; do
            echo "        - # ${pane_cmd} @ ${pane_path}"
        done
    done
}
