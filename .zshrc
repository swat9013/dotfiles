## 起動速度測定用
##zmodload zsh/zprof && zprof

# Path to your oh-my-zsh installation.
# export ZSH=$HOME/dotfiles/oh-my-zsh

# ZSH_THEME="powerline"

# plugins=(
#     bundler
#     cdd
#     docker
#     encode64
#     gem
#     git
#     homeshick
#     rails
#     rake
#     vagrant
#     knife
#     tmux
#     tmuxinator
#     # zsh-syntax-highlighting
# )

# source $ZSH/oh-my-zsh.sh
for conf in $HOME/dotfiles/.zsh/*.zsh; do
    source ${conf};
done

## 起動速度測定用
# if (which zprof > /dev/null) ;then
#     zprof | less
# fi


#### Prompt ###
## プロンプトに色を付ける
autoload -U colors; colors

## 一般ユーザ時
tmp_prompt="%F{cyan}[%n@%m${WINDOW:+"[$WINDOW]"}]%f-%{${fg[green]}%}[%~]%{${reset_color}%}-%F{magenta}[%D{%m/%d %T}]%f
%#"
#tmp_prompt="%{${fg[cyan]}%}%n%# %{${reset_color}%}"
tmp_prompt2="%{${fg[cyan]}%}%_> %{${reset_color}%}"
tmp_rprompt="%{${fg[green]}%}[%~]%{${reset_color}%}"
tmp_sprompt="%{${fg[yellow]}%}%r is correct? [Yes, No, Abort, Edit]:%{${reset_color}%}"

## rootユーザ時(太字にし、アンダーバーをつける)
if [ ${UID} -eq 0 ]; then
    tmp_prompt="%B%U${tmp_prompt}%u%b"
    tmp_prompt2="%B%U${tmp_prompt2}%u%b"
    tmp_rprompt="%B%U${tmp_rprompt}%u%b"
    tmp_sprompt="%B%U${tmp_sprompt}%u%b"
fi

#gitのブランチの表示
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '(%s)-[%b]'
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a]'
RPROMPT="%1(v|%F{green}%1v%f|)"


PROMPT=$tmp_prompt    # 通常のプロンプト
PROMPT2=$tmp_prompt2  # セカンダリのプロンプト(コマンドが2行以上の時に表示される)
#RPROMPT=$tmp_rprompt  # 右側のプロンプト
SPROMPT=$tmp_sprompt  # スペル訂正用プロンプト
