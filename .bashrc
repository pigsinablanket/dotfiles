[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

alias emacs='emacs -nw'
alias valgrind='valgrind --track-origins=yes'
export TERM=xterm-256color
export EDITOR=$(which emacs)
