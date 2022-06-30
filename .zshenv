# locale
export LANGUAGE="ja_JP.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

# editor
export EDITOR=emacs
if [ "$EDITOR" = "emacs" ]; then
    EDITOR_CMD="e"
else
    EDITOR_CMD="$EDITOR"
fi

# history
export HISTFILE=$HOME/.zsh-history
export HISTSIZE=100000
export SAVEHIST=100000

# 環境別設定
if [ "$(uname)" = 'Darwin' ]; then
    export ANDROID_HOME=~/Library/Android/sdk
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    export PATH=$HOME/.emacsenv/bin:$PATH
elif [ "$(expr substr $(uname -s) 1 10)" = 'MINGW32_NT' ]; then
else
fi
