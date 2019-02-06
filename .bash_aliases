#!/usr/bin/env bash

# This file is part of the dotfiles project. Changes should be checked in to the
# repository to be propagated to other computers.

if [ $(uname -s) = 'Darwin' ]; then
    alias ls='ls -G'
    alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
else
    alias ls='ls --color=auto'

    # alias to start e  macs in the terminal
    alias emacs='emacs -nw'

    # Add an "alert" alias for long running commands. Use like: `sleep 10; alert`
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
fi

alias grep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias ll='ls -Alh'
alias la='ls -alh'
alias l='ls -CF'
alias cls='printf "\033c"'

# grc aliases: grc provides some extra color support using regular expressions
# and user-defined colors
alias netstat='grc netstat'
