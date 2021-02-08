#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd)"

# MacOS
os=$(uname -s)
if [[ "$os" == "Darwin" ]]; then
    ./macos.sh
fi

# Dotfiles
dotFiles=$(ls -A "$DOTFILES_DIR/dotfiles")
for f in $dotFiles; do
    ln -svfh "$DOTFILES_DIR/dotfiles/$f" "$HOME/$f"
done

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls -A "$DOTFILES_DIR/bin")
for f in $scriptFiles; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# XDG config: ~/.config
mkdir -p ~/.config
xdgConfigs=$(ls -A "$DOTFILES_DIR/xdg_config")
for f in $xdgConfigs; do
    ln -svfh "$DOTFILES_DIR/xdg_config/$f" "$HOME/.config/$f"
done

# Emacs Prelude
mkdir -p ~/emacs
PRELUDE_DIR=~/emacs/prelude
if [[ ! -d $PRELUDE_DIR ]]; then
    printf "\nEmacs Prelude not found. Installing Prelude...\n"
    export PRELUDE_INSTALL_DIR="$PRELUDE_DIR"
    curl -L https://git.io/epre | sh
fi
ln -svf "$DOTFILES_DIR/emacs_prelude/init.el" $PRELUDE_DIR/personal/init.el
ln -svf "$DOTFILES_DIR/emacs_prelude/org.el" $PRELUDE_DIR/personal/org.el
mkdir -p $PRELUDE_DIR/personal/preload
ln -svf "$DOTFILES_DIR/emacs_prelude/preload/init.el" $PRELUDE_DIR/personal/preload/init.el
mkdir -p $PRELUDE_DIR/personal/babel
ln -svf "$DOTFILES_DIR/emacs_prelude/personal/babel/library-of-babel.org" $PRELUDE_DIR/personal/babel/library-of-babel.org

# Emacs minimal
MINIMAL_DIR=~/emacs/minimal
mkdir -p $MINIMAL_DIR
ln -svfh "$DOTFILES_DIR/emacs_minimal/init.el" $MINIMAL_DIR/init.el

# Emacs cfclrk
CFCLRK_DIR=~/emacs/cfclrk
mkdir -p $CFCLRK_DIR
ln -svfh "$DOTFILES_DIR/emacs_cfclrk/init.el" $CFCLRK_DIR/init.el

# Set up symlinks, and make cfclrk the default
ln -svfh ~/.config/emacs ~/.emacs.d
ln -svfh $CFCLRK_DIR ~/.config/emacs

# Tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "Tmux package manager is not installed. Installing!"
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi

# Pyenv
mkdir -p ~/.pyenv
ln -svf "$DOTFILES_DIR/.pyenv/default_packages" ~/.pyenv/default-packages

# SSH
mkdir -p ~/.ssh
ln -svf "$DOTFILES_DIR/.ssh/config" ~/.ssh/config
