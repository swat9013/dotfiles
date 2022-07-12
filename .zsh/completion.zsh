if [[ -d $ASDF_DIR ]]; then
  source $ASDF_DIR/asdf.sh
  source $ASDF_DIR/completions/asdf.bash
fi

## 保管を有効化
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit
