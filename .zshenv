# LANGUAGE must be set by en_US
export LANGUAGE="ja_JP.UTF-8"
export LANG="${LANGUAGE}"
export LC_ALL="${LANGUAGE}"
export LC_CTYPE="${LANGUAGE}"

export EDITOR=emacs
if [ "$EDITOR" = "emacs" ]; then
    EDITOR_CMD="e"
else
    EDITOR_CMD="$EDITOR"
fi

#### Ls Color ###
## 色の設定
export LSCOLORS=Exfxcxdxbxegedabagacad
## 補完時の色の設定
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
## ZLS_COLORSとは？
export ZLS_COLORS=$LS_COLORS
## lsコマンド時、自動で色がつく(ls -Gのようなもの？)
export CLICOLOR=true

export HISTFILE=$HOME/.zsh-history
export HISTSIZE=100000
export SAVEHIST=100000

if [ "$(uname)" = 'Darwin' ]; then
    export ANDROID_HOME=~/Library/Android/sdk
    # export PATH=${PATH}:${ANDROID_HOME}/tools
    # export PATH=${PATH}:${ANDROID_HOME}/platform-tools
elif [ "$(expr substr $(uname -s) 1 5)" = 'Linux' ]; then
    export PATH=$HOME/.emacsenv/bin:$PATH
elif [ "$(expr substr $(uname -s) 1 10)" = 'MINGW32_NT' ]; then
else
fi
