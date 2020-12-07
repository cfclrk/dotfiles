#!/usr/bin/env bash

set -eu -o pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# MacOS
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
   ./macos.sh
fi

# Dotfiles
dotFiles=$(ls -A "$DIR/dotfiles")
for f in $dotFiles; do
    ln -svfh "$DIR/dotfiles/$f" "$HOME/$f"
done

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls -A "$DIR/bin")
for f in $scriptFiles; do
    ln -svf "$DIR/bin/$f" "$HOME/bin/$f"
done

# XDG config dir
mkdir -p ~/.config
xdgConfigs=$(ls -A "$DIR/xdg_config")
for f in $xdgConfigs; do
    ln -svfh "$DIR/xdg_config/$f" "$HOME/.config/$f"
done

# Emacs Prelude
mkdir -p ~/emacs
PRELUDE_DIR=~/emacs/emacs_prelude
if [[ ! -d $PRELUDE_DIR ]]; then
    printf "\nEmacs Prelude not found. Installing Prelude...\n"
    export PRELUDE_INSTALL_DIR="$PRELUDE_DIR"
    curl -L https://git.io/epre | sh
fi
ln -svf "$DIR/emacs_prelude/init.el" $PRELUDE_DIR/personal/init.el
ln -svf "$DIR/emacs_prelude/org.el" $PRELUDE_DIR/personal/org.el
mkdir -p $PRELUDE_DIR/personal/preload
ln -svf "$DIR/emacs_prelude/preload/init.el" $PRELUDE_DIR/personal/preload/init.el
mkdir -p $PRELUDE_DIR/personal/babel
ln -svf "$DIR/emacs_prelude/personal/babel/library-of-babel.org" $PRELUDE_DIR/personal/babel/library-of-babel.org

# Emacs minimal
MINIMAL_DIR=~/emacs/emacs_minimal
ln -svfh "$DIR/emacs_minimal" $MINIMAL_DIR

# Make Prelude the default
ln -svfh $PRELUDE_DIR "$HOME/.config/emacs"

# Tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "Tmux package manager is not installed. Installing!"
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Pyenv
mkdir -p ~/.pyenv
ln -svf "$DIR/.pyenv/default_packages" ~/.pyenv/default-packages

# SSH
mkdir -p ~/.ssh
ln -svf "$DIR/.ssh/config" ~/.ssh/config
