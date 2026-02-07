# locale
export LANGUAGE="ja_JP.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# 環境別設定（EDITOR判定前にPATHを確定させる）
if [ "$(uname)" = 'Darwin' ]; then
    export ANDROID_HOME=~/Library/Android/sdk
    if [ -e /opt/homebrew/bin/brew ]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    export PATH=$HOME/.emacsenv/bin:$PATH
elif [ "$(expr substr $(uname -s) 1 10)" = 'MINGW32_NT' ]; then
else
fi

# editor
# VS Code内のターミナルではVS Code、それ以外ではEmacsを優先
if [ "$TERM_PROGRAM" = "vscode" ] && command -v code > /dev/null 2>&1; then
    export EDITOR="code --wait"
    export VISUAL="$EDITOR"
    EDITOR_CMD="code"
elif command -v emacs > /dev/null 2>&1; then
    export EDITOR=emacs
    export VISUAL="$EDITOR"
    EDITOR_CMD="e"
elif command -v code > /dev/null 2>&1; then
    export EDITOR="code --wait"
    export VISUAL="$EDITOR"
    EDITOR_CMD="code"
else
    export EDITOR=vim
    export VISUAL="$EDITOR"
    EDITOR_CMD="vim"
fi

export CLICOLOR=true # lsコマンド時、自動で色がつく

# Starship
export STARSHIP_CONFIG="$HOME/.config/starship/starship.toml"

## Gemini CLI用の環境変数
export GOOGLE_CLOUD_PROJECT="gen-lang-client-0404398232"
