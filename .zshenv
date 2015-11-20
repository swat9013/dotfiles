export EDITOR=emacs
if [ "$EDITOR" = "emacs" ]; then
    EDITOR_CMD="e"
else
    EDITOR_CMD="$EDITOR"
fi

# rbenvのパスを通す
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

#### Ls Color ###
## 色の設定
export LSCOLORS=Exfxcxdxbxegedabagacad
## 補完時の色の設定
export LS_COLORS='di=01;34:ln=01;35:so=01;32:ex=01;31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
## ZLS_COLORSとは？
export ZLS_COLORS=$LS_COLORS
## lsコマンド時、自動で色がつく(ls -Gのようなもの？)
export CLICOLOR=true


PATH=$PATH:~/local/bin/
export PATH=$PATH:~/local/bin/