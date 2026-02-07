# direnv hookをキャッシュ（direnvアップデート時は手動でキャッシュ削除）
_direnv_cache="${XDG_CACHE_HOME:-$HOME/.cache}/direnv/hook.zsh"
if [[ ! -r "$_direnv_cache" ]]; then
    mkdir -p "${_direnv_cache:h}"
    direnv hook zsh > "$_direnv_cache"
fi
source "$_direnv_cache"
unset _direnv_cache
