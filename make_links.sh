#!/usr/bin/env bash

set -eu -o pipefail

# Update emacs Prelude
cd ~/.emacs.d/
git pull

# Ensure dotfiles project exists
DOTFILES_DIR=$HOME/Projects/dotfiles
cd "$DOTFILES_DIR"

dot_files=(".bash_logout"
           ".bash_aliases"
           ".bashrc"
           ".functions.fish"
           ".functions.sh"
           ".gitconfig"
           ".profile"
           ".tmux.conf"
           ".vimrc")

for f in ${dot_files[*]}; do
    ln -svf "$DOTFILES_DIR/$f" "$HOME/$f"
done

ln -svf "$DOTFILES_DIR/.config/fish/config.fish" "$HOME/.config/fish/config.fish"

ln -svf "$DOTFILES_DIR/emacs/init.el" "$HOME/.emacs.d/personal/init.el"

ln -svf "$DOTFILES_DIR/emacs/org.el" "$HOME/.emacs.d/personal/org.el"
