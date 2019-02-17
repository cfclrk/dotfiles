#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR=~/Projects/dotfiles

# Ensure dotfiles project exists
cd "$DOTFILES_DIR"

mkdir -p ~/.lein
mkdir -p ~/.ssh
mkdir -p ~/.config/fish
dotFiles=(".bash_logout"
          ".bash_aliases"
          ".bashrc"
          ".functions.fish"
          ".functions.sh"
          ".gitconfig"
          ".gitignore_global"
          ".profile"
          ".tmux.conf"
          ".vimrc"
          ".lein/profiles.clj"
          ".ssh/config"
          ".config/fish/config.fish")
for f in ${dotFiles[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

# Emacs
if [[ ! -d ~/.emacs.d ]]; then
    echo "No ~/.emacs.d directory. Install Prelude!"
    exit 1
fi
ln -svf "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/personal/init.el
ln -svf "$DOTFILES_DIR/emacs/org.el" ~/.emacs.d/personal/org.el

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls $DOTFILES_DIR/bin)
for f in $scriptFiles; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "Tmux package manager is not installed. Installing!"
    mkdir -p ~/.tmux/plugins
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
fi
