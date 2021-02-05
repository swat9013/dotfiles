# Number of seconds to wait before printing a reminder
UPDATE_THRESHOLD="86400"

check_for_updates() {
  echo "check dotfiles update"
  [ ! -e $HOME/.last_update ] && touch $HOME/.last_update
  # Initialize for when we have no GNU date available
  last_check=0
  time_now=0

  # Darwin uses BSD, check for gdate, else use date
  if [[ $(uname) = "Darwin" && -n $(which gdate) ]]; then
    last_login=$(gdate -r ~/.last_update +%s)
    time_now=$(gdate +%s)
  else
    # Ensure this is GNU grep
    if [ -n "$(date --version 2> /dev/null | grep GNU)" ]; then
      last_login=$(date -r ~/.last_update +%s)
      time_now=$(date +%s)
    fi
  fi

  time_since_check=$((time_now - last_login))

  if [ "$time_since_check" -ge "$UPDATE_THRESHOLD" ]; then
    echo "since_time: $time_since_check"
    echo "==> Your system is out of date!"
    echo 'Run `update` to bring it up to date.'
    update
  fi
}

update() {
  touch $HOME/.last_update

  # Update dotfiles repo
  cd ~/.dotfiles/
  echo "==> Updating dotfiles..."
  git fetch --quiet origin
  if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/master)" ]; then
    echo "  --> outdated.";
    echo "git pull"
    git pull
  fi
}


check_for_updates
