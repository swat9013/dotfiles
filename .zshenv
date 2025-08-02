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

export CLICOLOR=true # lsコマンド時、自動で色がつく

# 環境別設定
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

## 補完を有効化
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit


## Gemini CLI用の環境変数
export GOOGLE_CLOUD_PROJECT="gen-lang-client-0404398232"
