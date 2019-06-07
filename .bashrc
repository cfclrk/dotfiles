#!/usr/bin/env bash

source $HOME/bin/colors.sh

case "$TERM" in
    xterm*|rxvt*|screen*)
        PS1="${Blue}\w ${Cyan}≫ ${Color_Off}"
        ;;
    *)
        # term is probably dumb, e.g. doing an scp operation
        ;;
esac

# append to the history file, don't overwrite it
shopt -s histappend

# check window size after each command, update LINES and COLUMNS if necessary
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# bash completion (you don't need this here if it's already enabled in
# /etc/bash.bashrc and /etc/profile sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi
if [ $(uname -s) = 'Darwin' ]; then
    if [ -f $(brew --prefix)/etc/bash_completion ]; then
        . $(brew --prefix)/etc/bash_completion
    fi
fi

. ~/.bash_aliases
. ~/.functions.sh

if [ -e ~/.extras.sh ]; then
    . ~/.extras.sh
fi

# aws CLI yumminess
complete -C aws_completer aws
