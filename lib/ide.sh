#!/bin/bash

# set tmux panes for ide

if [ "$#" -eq 0 ]; then
  tmux split-window -h
  tmux select-pane -t 0
  tmux split-window -v
  tmux select-pane -t 0
  clear
else
  case $1 in
    1)
      tmux split-window -h
      tmux select-pane -t 0
      clear
      ;;
    *)
      echo [ERROR] "$1" は設定されていない引数です。
      ;;
  esac
fi
