#!/bin/bash

eval "$(/opt/homebrew/bin/brew shellenv)"

HISTCONTROL=ignoreboth  # Ignore duplicate lines in the history
HISTSIZE=1000
HISTFILESIZE=2000

paths=($HOME/bin
       $HOME/.local/bin
       $HOME/.cargo/bin
       $HOME/.cabal/bin
       $HOME/.ghcup/bin
       $HOME/Work/bin
       $HOMEBREW_PREFIX/sbin
       $HOMEBREW_PREFIX/opt/curl/bin
       $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin
       $HOMEBREW_PREFIX/opt/findutils/libexec/gnubin
       $HOMEBREW_PREFIX/opt/libpq/bin
       $HOMEBREW_PREFIX/opt/make/libexec/gnubin
       $HOMEBREW_PREFIX/opt/openssl@1.1/bin
       $HOMEBREW_PREFIX/opt/texinfo/bin)

for path in "${paths[@]}"; do
    PATH=$path:$PATH
done

PATH=$(go env GOPATH)/bin:$PATH

# pyenv
if command -v pyenv > /dev/null; then
   eval "$(pyenv init -)"
   eval "$(pyenv virtualenv-init -)"
fi

# pipenv
export PIPENV_IGNORE_VIRTUALENVS=1

. ~/.bashrc
