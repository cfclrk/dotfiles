#!/usr/bin/env bash

set -eu -o pipefail

DOTFILES_DIR=~/Projects/dotfiles

# Ensure dotfiles project exists
cd "$DOTFILES_DIR"

# Link to all top-level dot-files files from $HOME
dotFiles=(".bash_logout"
          ".bash_aliases"
          ".bashrc"
          ".functions.fish"
          ".functions.sh"
          ".gitconfig"
          ".gitignore_global"
          ".profile"
          ".tmux.conf"
          ".vimrc")
for f in ${dotFiles[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

# Fish
mkdir -p ~/.config/fish
ln -svf "$DOTFILES_DIR/.config/fish/config.fish" "$HOME/.config/fish/config.fish"

# Emacs
if [[ ! -d ~/.emacs.d ]]; then
    echo "No ~/.emacs.d directory. Install Prelude!"
    exit 1
fi
ln -svf "$DOTFILES_DIR/emacs/init.el" ~/.emacs.d/personal/init.el
ln -svf "$DOTFILES_DIR/emacs/org.el" ~/.emacs.d/personal/org.el

# SSH config
mkdir -p ~/.ssh
ln -svf "$DOTFILES_DIR/.ssh/config" ~/.ssh/config

# ~/bin
mkdir -p ~/bin
scriptFiles=$(ls $DOTFILES_DIR/bin)
for f in $scriptFiles; do
    ln -svf "$DOTFILES_DIR/bin/$f" "$HOME/bin/$f"
done

# tmux
if [[ ! -d ~/.tmux/plugins/tpm ]]; then
    echo "No ~/.tmux/plugins/tpm directory found. Installing tmux package manager!"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    exit 1
fi
