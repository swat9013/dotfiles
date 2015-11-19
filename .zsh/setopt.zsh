## cd無しでディレクトリ移動
## setopt auto_cd
## 補完候補を詰めて表示
setopt list_packed
## cd -[tab]で移動の履歴を表示
setopt auto_pushd
## 同じディレクトリを pushd しない
setopt pushd_ignore_dups
## 色を使う
setopt prompt_subst
## カッコの対応などを自動的に補完
setopt auto_param_keys
setopt hist_ignore_dups
setopt auto_param_slash
setopt mark_dirs
## 補完候補を一覧表示
setopt auto_list
## 補完候補一覧でファイルの種別をマーク表示
setopt list_types
setopt hist_no_store
setopt hist_save_no_dups
setopt hist_reduce_blanks

#日本語ファイルの表示
setopt print_eight_bit

setopt share_history
#setopt correct

setopt transient_rprompt
