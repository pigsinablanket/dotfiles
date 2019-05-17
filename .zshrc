HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
bindkey -e

export PATH=~/.local/bin:$PATH

# PROMPT
PROMPT='%n@%m %F{green}%~%f> '

function preexec() {
  timer=${timer:-$SECONDS}
}

function precmd() {
  if [ $timer ]; then
    timer_show=$(($SECONDS - $timer))
    export RPROMPT="%F{cyan}${timer_show}s %{$reset_color%}[%F{yellow}%?%f]"
    unset timer
  fi
}
