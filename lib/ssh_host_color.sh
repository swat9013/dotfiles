#!/bin/bash

set_term_bgcolor(){
local R=$1
local G=$2
local B=$3
/usr/bin/osascript <<EOF
tell application "Terminal" to set background color of window 1 to {$(($R*65535/255)), $(($G*65535/255)), $(($B*65535/255)),0}
EOF
}

if [[ "$@" =~ pro ]]; then
  set_term_bgcolor 40 0 0
elif [[ "$@" =~ stg ]]; then
  set_term_bgcolor 0 40 0
elif [[ "$@" =~ dev ]]; then
  set_term_bgcolor 0 0 40
else
  set_term_bgcolor 40 40 40
fi

ssh $@

set_term_bgcolor 0 0 0
